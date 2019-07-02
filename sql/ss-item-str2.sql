WITH
ITEMS AS (
  SELECT ITEM_NBR, OLD_NBR
  FROM MX_CF_VM.ITEM
  WHERE OLD_NBR IN (?OLD_NBRS)
),
STORES AS (
  SELECT DISTINCT
		STORE_NBR,
		CASE
			WHEN TRAIT_NBR = 297 THEN 'SUPERCENTER'
			WHEN TRAIT_NBR = 9 THEN 'BODEGA'
			WHEN TRAIT_NBR = 11 THEN 'SUPERAMA'
			WHEN TRAIT_NBR = 1312 THEN 'MIBODEGA'
			WHEN TRAIT_NBR IN (136, 969) THEN 'BAE'
			ELSE 'OTRO'
		END AS NEGOCIO
	FROM
		MX_CF_VM.TRAIT_STORE
	WHERE
		NEGOCIO IN ('?NEGOCIOS')
),
MAXEF AS (
  SELECT
    SS_EVENT_NAME,
    MAX(SS_EVENT_EFF_DATE) AS EFF
  FROM
    MX_CF_REPL_VM.SAFETY_STOCK_EVENT
  WHERE
    SS_EVENT_EFF_DATE <= DATE
  GROUP BY 1
),
EVENT AS (
  SELECT
    MAXEF.SS_EVENT_NAME,
    MAXEF.EFF,
    B.SS_ADJUST_INV_DAY_QTY
  FROM
    MAXEF
  LEFT JOIN
    MX_CF_REPL_VM.SAFETY_STOCK_EVENT B
  ON (MAXEF.SS_EVENT_NAME = B.SS_EVENT_NAME AND MAXEF.EFF = B.SS_EVENT_EFF_DATE)
),
FCST AS (
  SELECT
    	A.STORE_NBR,
		A.ITEM_NBR,
		(SUM(A.SALES_FCST_EACH_QTY) / COUNT(DISTINCT A.WM_YR_WK)) / 7 AS AVG_DLY_FCST
	FROM
		MX_CF_VM.STORE_ITEM_FCST_WK_CONV AS A
		INNER JOIN (SELECT DISTINCT WM_YR_WK FROM MX_CF_VM.CALENDAR_DAY WHERE GREGORIAN_DATE = CURRENT_DATE) AS CURRWK
			ON A.FCST_WM_YR_WK = CURRWK.WM_YR_WK
		INNER JOIN (
			SELECT DISTINCT WM_YR_WK
			FROM MX_CF_VM.CALENDAR_DAY
			WHERE GREGORIAN_DATE BETWEEN ('?START_DATE') AND ('?END_DATE')
		) AS PER
			ON A.WM_YR_WK IN PER.WM_YR_WK
	GROUP BY 1, 2
),
BPR AS (
  SELECT
    ITEM_NBR,
    DEST_STORE_NBR AS STORE_NBR,
    MAX(CASE WHEN SS_PRESN_SOURCE_NAME = 'DEFAULT' THEN PRESENTATION_QTY ELSE 0 END) AS BASE_PRESS,
    SUM(CASE WHEN SS_PRESN_SOURCE_NAME <> 'DEFAULT' THEN PRESENTATION_QTY ELSE 0 END) AS SSPRESS
  FROM MX_CF_REPL_VM.GRS_PRESENTATION_PARM
  WHERE
      EFF_DATE <= DATE
      AND (EXPIRE_DATE > DATE OR EXPIRE_DATE = '1970-01-01')
  GROUP BY 1, 2
),
FULF AS (
  SELECT
    G.ITEM_NBR,
    G.STORE_NBR,
    F.MAX_SS_QTY AS MAXSS,
    F.MIN_SS_QTY AS MINSS,
    F.MIN_SS_SUPPLY_DAY_QTY AS SS_COV,
	ZEROIFNULL(EVENT.SS_ADJUST_INV_DAY_QTY) AS SS_TEMP,
	G.SAFETY_EACH_QTY AS SSGANADOR
  FROM MX_CF_REPL_VM.GRS_FCST_DEMAND AS G
  	LEFT JOIN MX_CF_REPL_VM.GRS_FULFILLMENT_PARM AS F
		ON G.ITEM_NBR = F.ITEM_NBR AND G.STORE_NBR = F.STORE_NBR
  	LEFT JOIN EVENT
		ON F.SS_EVENT_NAME = EVENT.SS_EVENT_NAME
  WHERE G.FCST_DATE = DATE
)
SELECT
  FULF.ITEM_NBR,
  ITEMS.OLD_NBR,
  FULF.STORE_NBR,
  NEGOCIO,
  
  ZEROIFNULL(CAST(BPR.SSPRESS AS DECIMAL(19, 2))) AS SSPRESS,
  ZEROIFNULL(CAST(BPR.BASE_PRESS AS DECIMAL(19, 2))) AS BASE_PRESS,
  ZEROIFNULL(CAST((SSPRESS + BASE_PRESS) AS DECIMAL(19, 2))) AS SSPRESS_TOT,
  ZEROIFNULL(FULF.SS_COV * FCST.AVG_DLY_FCST) AS SSCOV,
  ZEROIFNULL(FULF.SS_TEMP * FCST.AVG_DLY_FCST) AS SSTEMP,
  ZEROIFNULL(SSCOV + SSTEMP) AS SSCOV_TOT,
  ZEROIFNULL(CAST(FULF.MINSS AS DECIMAL(19, 2))) AS MIN_SS,
  ZEROIFNULL(CAST(FULF.MAXSS AS DECIMAL(19, 2))) AS MAX_SS,
  ZEROIFNULL(CAST(FULF.SSGANADOR AS DECIMAL(19, 2))) AS SS_GANADOR,
  CASE
    WHEN SS_GANADOR = MAX_SS THEN 'MAX_SS'
    WHEN SS_GANADOR = MIN_SS THEN 'MIN_SS'
    WHEN SS_GANADOR BETWEEN (SSPRESS - 0.2) AND (SSPRESS + 0.2) THEN 'SSPRESS'
    WHEN SS_GANADOR BETWEEN (BASE_PRESS - 0.05) AND (BASE_PRESS + 0.05) THEN 'BASE_PRESS'
    WHEN SS_GANADOR BETWEEN (SSPRESS_TOT - 0.05) AND (SSPRESS_TOT + 0.05) THEN 'SSPRESS_TOT'
    WHEN SS_GANADOR BETWEEN (SSCOV - 0.2) AND (SSCOV + 0.2) THEN 'SSCOV'
    WHEN SS_GANADOR BETWEEN (SSTEMP - 0.2) AND (SSTEMP + 0.2) THEN 'SSTEMP'
    WHEN SS_GANADOR BETWEEN (SSCOV_TOT - 0.2) AND (SSCOV_TOT + 0.2) THEN 'SSCOV_TOT'
    WHEN SS_GANADOR = SSPRESS THEN 'SSPRESS'
    WHEN SS_GANADOR = (SSPRESS + BASE_PRESS) THEN 'SSPRESS_TOT'
    WHEN SSCOV BETWEEN (SS_GANADOR * 0.9) AND (SS_GANADOR * 1.1) THEN 'SSCOV'
    WHEN SSTEMP BETWEEN (SS_GANADOR * 0.9) AND (SS_GANADOR * 1.1) THEN 'SSTEMP'
    WHEN SSCOV_TOT BETWEEN (SS_GANADOR * 0.9) AND (SS_GANADOR * 1.1) THEN 'SSCOV_TOT'
    ELSE 'OTRO'
  END AS GANADOR
FROM
	FULF
	LEFT JOIN BPR
		ON FULF.ITEM_NBR = BPR.ITEM_NBR AND FULF.STORE_NBR = BPR.STORE_NBR
	LEFT JOIN FCST
		ON FULF.ITEM_NBR = FCST.ITEM_NBR AND FULF.STORE_NBR = FCST.STORE_NBR
	INNER JOIN STORES
		ON FULF.STORE_NBR = STORES.STORE_NBR
	INNER JOIN ITEMS
		ON FULF.ITEM_NBR = ITEMS.ITEM_NBR
