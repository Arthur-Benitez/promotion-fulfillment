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

BINS AS (
	SELECT
		INV.PRIME_XREF_ITEM_NBR,
	  INV.STORE_NBR,
		INV.WM_YR_WK,
	  INV.NEGOCIO,
	  INV.REPL_GROUP_NBR,
	  INV.OH_QTY,
		POS.SALES_AVG,
		CASE WHEN POS.SALES_AVG <= 0 THEN NULL ELSE (INV.OH_QTY * 1.000 / (POS.SALES_AVG / 7.000)) END AS DDV_OH,
		CASE
			WHEN DDV_OH <= 0   THEN 'DDV00_IG_000'
			WHEN DDV_OH  < 3   THEN 'DDV01_MN_003'
			WHEN DDV_OH  < 7   THEN 'DDV02_MN_007'
			WHEN DDV_OH  < 14  THEN 'DDV03_MN_014'
			WHEN DDV_OH  < 21  THEN 'DDV04_MN_021'
			WHEN DDV_OH  < 28  THEN 'DDV05_MN_028'
			WHEN DDV_OH  < 35  THEN 'DDV06_MN_035'
			WHEN DDV_OH  < 50  THEN 'DDV07_MN_050'
			WHEN DDV_OH  < 75  THEN 'DDV08_MN_075'
			WHEN DDV_OH  < 100 THEN 'DDV09_MN_100'
			WHEN DDV_OH  < 150 THEN 'DDV10_MN_150'
			WHEN DDV_OH  < 250 THEN 'DDV11_MN_250'
			WHEN DDV_OH  < 350 THEN 'DDV12_MN_350'
			WHEN DDV_OH  < 450 THEN 'DDV13_MN_450'
			WHEN DDV_OH >= 450 THEN 'DDV14_MY_450'
			ELSE 										'DDV14_MY_450'
		END AS BIN
	FROM
		INV
		LEFT JOIN POS
			ON INV.PRIME_XREF_ITEM_NBR = POS.ITEM_NBR AND INV.STORE_NBR = POS.STORE_NBR
)

SELECT
	BINS.PRIME_XREF_ITEM_NBR,
	BINS.NEGOCIO,
	BINS.WM_YR_WK,
	BINS.REPL_GROUP_NBR,
	BINS.BIN,
	SUM(BINS.OH_QTY) AS OH,
	SUM(BINS.SALES_AVG) AS SALES,
	CASE WHEN SALES <= 0 THEN NULL ELSE (OH * 1.000 / (SALES / 7.000)) END AS DDV_OH,
	COUNT(BINS.STORE_NBR) AS N_STORES
FROM
	BINS
GROUP BY 1, 2, 3, 4, 5
ORDER BY 1, 2, 3, 4, 5
