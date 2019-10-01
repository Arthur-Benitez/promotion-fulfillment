WITH

-- Tiendas validas
STORE AS (
  SELECT DISTINCT
    TR.STORE_NBR,
		TR.TRAIT_NBR,
		CASE WHEN TR.TRAIT_NBR = 297 THEN 'SUPERCENTER'
		  WHEN TR.TRAIT_NBR = 9 THEN 'BODEGA'
		  WHEN TR.TRAIT_NBR = 11 THEN 'SUPERAMA'
		  WHEN TR.TRAIT_NBR = 1312 THEN 'MIBODEGA'
		  WHEN TR.TRAIT_NBR = 969 THEN 'BAE'
		  WHEN TR.TRAIT_NBR = 136 THEN 'MEDIMART'
		  ELSE 'OTRO'
		END AS NEGOCIO
  FROM MX_CF_VM.TRAIT_STORE AS TR
    INNER JOIN MX_CF_VM.STORE_INFO AS T
      ON TR.STORE_NBR = T.STORE_NBR
	WHERE
	  TR.TRAIT_NBR IN (11, 297, 1312, 969, 136, 9)
	  AND NEGOCIO IN (?NEGOCIOS)
    AND T.OPEN_STATUS NOT IN (0, 3, 7, 6, 8)
),

-- Articulos referenciados asociados a los old_nbrs (para sacar las ventas cuando cambian el old_nbr)
-- Aqui *NO* se debe aplicar los filtros de validez
XREF AS (
  SELECT
    PRIME_XREF_ITEM_NBR,
    ITEM_NBR,
    REPL_GROUP_NBR
  FROM
    MX_CF_VM.ITEM A
  WHERE
    PRIME_XREF_ITEM_NBR IN (
      SELECT DISTINCT
        PRIME_XREF_ITEM_NBR
      FROM
        MX_CF_VM.ITEM
      WHERE
        OLD_NBR IN (?OLD_NBRS)
    )
),

-- Tabla con combinaciones activas actualmente, agrupadas por Prime
-- Aqui *SI* se debe aplicar los filtros de validez, porque solo aplica a los actuales
INV AS (
  SELECT DISTINCT
    XREF.PRIME_XREF_ITEM_NBR,
    WKLY_INV.STORE_NBR,
    WKLY_INV.WM_YR_WK,
    ST.NEGOCIO,
    --ITEM.OLD_NBR,
    --ITEM.PRIMARY_DESC,
    XREF.REPL_GROUP_NBR,
    SUM(GREATEST(WKLY_INV.ON_HAND_QTY, 0)) AS OH_QTY
  FROM
    MX_CF_VM.REPL_SKU_WKLY_INV AS WKLY_INV
    INNER JOIN MX_CF_VM.ITEM_DESC AS ITEM
      ON WKLY_INV.ITEM_NBR = ITEM.ITEM_NBR
    INNER JOIN STORE AS ST
      ON WKLY_INV.STORE_NBR = ST.STORE_NBR
    INNER JOIN XREF
      ON WKLY_INV.ITEM_NBR = XREF.ITEM_NBR
  WHERE
  	WKLY_INV.WM_YR_WK >= ('?WK_INI_STUD')
		AND WKLY_INV.WM_YR_WK <= ('?WK_FIN_STUD')
		AND ITEM.STATUS_CODE IN ('A')
		AND ITEM.ORDBK_FLAG IN ('Y')
		AND ITEM.CANCEL_WHEN_OUT_FLAG IN ('N')
		AND ITEM.ITM_MBM_CODE IN ('M', 'I')
		--AND ITEM.ITEM_NBR = ITEM.PRIME_XREF_ITEM_NBR
    AND WKLY_INV.CARRY_OPTION IN ('R')
    AND WKLY_INV.CARRIED_STATUS IN ('R')
	GROUP BY 1, 2, 3, 4, 5
),

COMB AS (
	SELECT DISTINCT
		WKLY_INV.ITEM_NBR,
		WKLY_INV.STORE_NBR,
		WKLY_INV.WM_YR_WK
	FROM
		MX_CF_VM.REPL_SKU_WKLY_INV AS WKLY_INV
  	INNER JOIN STORE AS ST
  		ON WKLY_INV.STORE_NBR = ST.STORE_NBR
  	INNER JOIN XREF
	    ON WKLY_INV.ITEM_NBR = XREF.ITEM_NBR
  WHERE
  	WKLY_INV.WM_YR_WK >= ('?WK_INI_VTAS')
		AND WKLY_INV.WM_YR_WK <= ('?WK_FIN_VTAS')
),

POS AS (
	SELECT
		COMB.ITEM_NBR,
		COMB.STORE_NBR,
		AVERAGE(ZEROIFNULL(SALES.WKLY_QTY)) AS SALES_AVG
	FROM
		COMB 
		LEFT JOIN (
		  SELECT
				ITEM_NBR,
				STORE_NBR,
		    WM_YR_WK,
				SUM(GREATEST(WKLY_QTY, 0)) AS WKLY_QTY
			FROM
				MX_CF_VM.SKU_DLY_POS
			WHERE
				REPORT_CODE NOT IN (8)
				AND WM_YR_WK >= ('?WK_INI_VTAS')
				AND WM_YR_WK <= ('?WK_FIN_VTAS')
			GROUP BY 1, 2, 3
		) SALES
			ON SALES.ITEM_NBR = COMB.ITEM_NBR AND SALES.STORE_NBR = COMB.STORE_NBR AND SALES.WM_YR_WK = COMB.WM_YR_WK
	GROUP BY 1, 2
),

BASE AS (
	SELECT
		INV.PRIME_XREF_ITEM_NBR,
	  INV.STORE_NBR,
		INV.WM_YR_WK,
	  INV.NEGOCIO,
	  INV.REPL_GROUP_NBR,
	  INV.OH_QTY,
		POS.SALES_AVG,
		CASE WHEN POS.SALES_AVG <= 0 THEN 1000 ELSE (INV.OH_QTY * 1.000 / (POS.SALES_AVG / 7.000)) END AS DDV_OH_ST
	FROM
		INV
		LEFT JOIN POS
			ON INV.PRIME_XREF_ITEM_NBR = POS.ITEM_NBR AND INV.STORE_NBR = POS.STORE_NBR
),

N_STORES AS (
	SELECT
		PRIME_XREF_ITEM_NBR,
		NEGOCIO,
		WM_YR_WK,
		COUNT(STORE_NBR) AS MAX_STORES
	FROM
		BASE
	GROUP BY
		1, 2, 3
),

PERC_ALL AS (
	SELECT
		BASE.*,
		NS.MAX_STORES,
		ROW_NUMBER() OVER (
			PARTITION BY BASE.PRIME_XREF_ITEM_NBR, BASE.NEGOCIO, BASE.WM_YR_WK
			ORDER BY DDV_OH_ST
		) AS ORDENADO,
		ORDENADO * 100 / MAX_STORES AS PERCENTIL,
		CASE WHEN PERCENTIL = 25 THEN DDV_OH_ST ELSE 0 END AS P25,
		CASE WHEN PERCENTIL = 50 THEN DDV_OH_ST ELSE 0 END AS P50,
		CASE WHEN PERCENTIL = 75 THEN DDV_OH_ST ELSE 0 END AS P75
	FROM
		BASE
		LEFT JOIN N_STORES AS NS
			ON NS.PRIME_XREF_ITEM_NBR = BASE.PRIME_XREF_ITEM_NBR AND NS.NEGOCIO = BASE.NEGOCIO AND NS.WM_YR_WK = BASE.WM_YR_WK
),

PERC_IMP AS (
	SELECT
		PRIME_XREF_ITEM_NBR,
		NEGOCIO,
		WM_YR_WK,
		PERCENTIL,
		AVERAGE(P25) AS P25,
		AVERAGE(P50) AS P50,
		AVERAGE(P75) AS P75
	FROM
		(SELECT * FROM PERC_ALL WHERE (P25 > 0) OR (P50 > 0) OR (P75 > 0)) A
	GROUP BY
		1, 2, 3, 4
),

PERC_SUM AS (
	SELECT
		PRIME_XREF_ITEM_NBR,
		NEGOCIO,
		WM_YR_WK,
		MAX(P25) AS PERC_25,
		MAX(P50) AS PERC_50,
		MAX(P75) AS PERC_75
	FROM
		PERC_IMP
	GROUP BY
		1, 2, 3
)

SELECT
	BASE.PRIME_XREF_ITEM_NBR,
	BASE.NEGOCIO,
	BASE.WM_YR_WK,
	BASE.REPL_GROUP_NBR,
	SUM(BASE.OH_QTY) AS OH,
	SUM(BASE.SALES_AVG) AS SALES,
	CASE WHEN SALES <= 0 THEN 1000 ELSE (OH * 1.000 / (SALES / 7.000)) END AS DDV_OH,
	COUNT(BASE.STORE_NBR) AS N_STORES,
	AVERAGE(BASE.DDV_OH_ST) AS DDV_OH_AVG,
	MIN(BASE.DDV_OH_ST) AS PERC_00,
	PERC_SUM.PERC_25,
	PERC_SUM.PERC_50,
	PERC_SUM.PERC_75,
	MAX(BASE.DDV_OH_ST) AS PERC_100
FROM
	BASE
	LEFT JOIN PERC_SUM
		ON BASE.PRIME_XREF_ITEM_NBR = PERC_SUM.PRIME_XREF_ITEM_NBR AND BASE.NEGOCIO = PERC_SUM.NEGOCIO AND BASE.WM_YR_WK = PERC_SUM.WM_YR_WK
GROUP BY 1, 2, 3, 4, 11, 12, 13
ORDER BY 1, 2, 3, 4, 11, 12, 13
