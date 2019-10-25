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
    OLD_NBR,
    REPL_GROUP_NBR,
    ITEM1_DESC,
    SUBSTR(CAST(FINELINE_NBR AS CHAR(4)),1,2) AS CATEGORY
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
    XREF.OLD_NBR,
    XREF.REPL_GROUP_NBR,
    XREF.ITEM1_DESC,
    XREF.CATEGORY,
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
	GROUP BY 1, 2, 3, 4, 5, 6, 7, 8
),

POS_DDV AS (
SELECT
		INV.PRIME_XREF_ITEM_NBR,
		INV.STORE_NBR,
		AVERAGE(ZEROIFNULL(SALES.WKLY_QTY)) AS SALES_AVG
	FROM
		INV
		LEFT JOIN (
		  SELECT
				B.PRIME_XREF_ITEM_NBR,
				A.STORE_NBR,
				A.WM_YR_WK,
				SUM(GREATEST(A.WKLY_QTY, 0)) AS WKLY_QTY
			FROM
				MX_CF_VM.SKU_DLY_POS A
				INNER JOIN XREF B
					ON A.ITEM_NBR = B.ITEM_NBR
			WHERE
				REPORT_CODE NOT IN (8)
				AND WM_YR_WK >= ('?WK_INI_VTAS')
				AND WM_YR_WK <= ('?WK_FIN_VTAS')
			GROUP BY 1, 2, 3	
		) SALES
			ON SALES.ITEM_NBR = INV.PRIME_XREF_ITEM_NBR AND SALES.STORE_NBR = INV.STORE_NBR AND SALES.WM_YR_WK = INV.WM_YR_WK
	GROUP BY 1, 2
),

-- Tabla de ventas (se agrupa por PRIME por si cambiaron el item_nbr de un articulo)
POS_STUD AS (
  SELECT
		XREF.PRIME_XREF_ITEM_NBR,
		VTAS.STORE_NBR,
    VTAS.WM_YR_WK,
		SUM(GREATEST(VTAS.WKLY_QTY, 0)) AS WKLY_QTY,
		SUM(GREATEST(VTAS.WKLY_SALES, 0)) AS WKLY_SALES
	FROM
		MX_CF_VM.SKU_DLY_POS AS VTAS
    INNER JOIN XREF
      ON VTAS.ITEM_NBR = XREF.ITEM_NBR
	WHERE
		VTAS.REPORT_CODE NOT IN (8)
		AND VTAS.WM_YR_WK >= ('?WK_INI_STUD')
		AND VTAS.WM_YR_WK <= ('?WK_FIN_STUD')
	GROUP BY 1, 2, 3
),

BASE AS (
	SELECT
		INV.*,
		POS_DDV.SALES_AVG,
		CASE WHEN
			(POS_DDV.SALES_AVG <= 0 OR POS_DDV.SALES_AVG IS NULL) THEN 1000
			ELSE (INV.OH_QTY * 1.000 / (POS_DDV.SALES_AVG / 7.000))
		END AS DDV_OH_ST
	FROM
		INV
		LEFT JOIN POS_DDV
			ON INV.PRIME_XREF_ITEM_NBR = POS_DDV.PRIME_XREF_ITEM_NBR AND INV.STORE_NBR = POS_DDV.STORE_NBR
),

INST AS (
	SELECT
		INST.VP,
		INST.DIVISION,
		INST.DEP_NBR,
		INST.CATEGORY_NBR,
		CAT.CATEGORY_NAME,
		XREF.PRIME_XREF_ITEM_NBR,
		INST.FORMATO,
		INST.WM_YR_WK,
		SUM(INST.CATALOGO) AS CATALOGO,
		SUM(INST.FALT_OH) AS FALT_OH,
		SUM(INST.FALT_OH_IT) AS FALT_OH_IT,
		SUM(INST.FALT_OH_IT_IW) AS FALT_OH_IT_IW,
		SUM(INST.FALT_TTL) AS FALT_TTL,
		1 - (SUM(INST.FALT_OH) * 1.0000 / SUM(INST.CATALOGO) * 1.0000) AS IS_OH,
		1 - (SUM(INST.FALT_TTL) * 1.0000 / SUM(INST.CATALOGO) * 1.0000) AS IS_TTL	
	FROM
		MR101_WM_AD_HOC.IS_ACUM_ITEM_VF AS INST
		INNER JOIN XREF
			ON XREF.ITEM_NBR = INST.ITEM_NBR
		LEFT JOIN WM_AD_HOC.CATEGORY_NAME AS CAT
			ON INST.DEP_NBR = CAT.DEPT_NBR AND INST.CATEGORY_NBR = CAT.CATEGORY_NBR
	WHERE
		INST.FORMATO IN (?NEGOCIOS)
		AND INST.WM_YR_WK >= (?WK_INI_STUD)
		AND INST.WM_YR_WK <= (?WK_FIN_STUD)
	GROUP BY
		1, 2, 3, 4, 5, 6, 7, 8
),

OH_POS AS (
	SELECT
		BASE.PRIME_XREF_ITEM_NBR,
		BASE.NEGOCIO,
		BASE.WM_YR_WK,
	  BASE.REPL_GROUP_NBR,
	  BASE.OLD_NBR,
	  BASE.ITEM1_DESC,
	  BASE.CATEGORY,
		SUM(BASE.OH_QTY) AS OH,
		SUM(BASE.SALES_AVG) AS SALES,
		CASE WHEN SALES <= 0 THEN 1000 ELSE (OH * 1.000 / (SALES / 7.000)) END AS DDV_OH,
		SUM(ZEROIFNULL(POS_STUD.WKLY_QTY)) AS WKLY_QTY,
		SUM(ZEROIFNULL(POS_STUD.WKLY_SALES))/SUM(ZEROIFNULL(POS_STUD.WKLY_QTY)) AS SELL_PRICE,
		COUNT(BASE.STORE_NBR) AS N_STORES,
		AVERAGE(BASE.DDV_OH_ST) AS DDV_OH_AVG,
		PERCENTILE_CONT(0.00) WITHIN GROUP (ORDER BY BASE.DDV_OH_ST) AS PERC_00,
		PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY BASE.DDV_OH_ST) AS PERC_25,
		PERCENTILE_CONT(0.50) WITHIN GROUP (ORDER BY BASE.DDV_OH_ST) AS PERC_50,
		PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY BASE.DDV_OH_ST) AS PERC_75,
		PERCENTILE_CONT(1.00) WITHIN GROUP (ORDER BY BASE.DDV_OH_ST) AS PERC_100
	FROM
		BASE
	LEFT JOIN POS_STUD
		ON BASE.PRIME_XREF_ITEM_NBR = POS_STUD.PRIME_XREF_ITEM_NBR AND BASE.STORE_NBR = POS_STUD.STORE_NBR AND BASE.WM_YR_WK = POS_STUD.WM_YR_WK
	GROUP BY 1, 2, 3, 4, 5, 6, 7
)

SELECT
	INST.PRIME_XREF_ITEM_NBR,
	INST.FORMATO,
	INST.WM_YR_WK,
	INST.DEP_NBR AS DEPT_NBR,
	INST.CATEGORY_NBR,
	INST.CATEGORY_NAME,
	OH_POS.REPL_GROUP_NBR,
	OH_POS.ITEM1_DESC,
	OH_POS.OLD_NBR,
	INST.CATALOGO,
	INST.FALT_OH,
	INST.FALT_TTL,
	INST.IS_OH,
	INST.IS_TTL,
	OH_POS.WKLY_QTY,
	OH_POS.SELL_PRICE,
	OH_POS.N_STORES,
	OH_POS.OH,
	OH_POS.SALES,
	OH_POS.DDV_OH,
	OH_POS.DDV_OH_AVG,
	OH_POS.PERC_00,
	OH_POS.PERC_25,
	OH_POS.PERC_50,
	OH_POS.PERC_75,
	OH_POS.PERC_100
FROM
	OH_POS
	LEFT JOIN INST
		ON INST.PRIME_XREF_ITEM_NBR = OH_POS.PRIME_XREF_ITEM_NBR AND INST.FORMATO = OH_POS.NEGOCIO AND INST.WM_YR_WK = OH_POS.WM_YR_WK
ORDER BY DEPT_NBR, CATEGORY_NBR, CATEGORY_NAME, REPL_GROUP_NBR, PRIME_XREF_ITEM_NBR, FORMATO, WM_YR_WK
