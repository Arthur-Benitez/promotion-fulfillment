WITH
--SUBCONSULTAS SECUNDARIAS
FULF AS (
  SELECT
    ITEM_NBR,
    STORE_NBR,
    MAX_SS_QTY,
    MIN_SS_QTY,
    MIN_SS_SUPPLY_DAY_QTY,
    SS_EVENT_NAME
  FROM
    MX_CF_REPL_VM.GRS_FULFILLMENT_PARM
),
ITEMS AS (
  SELECT ITEM_NBR FROM MX_CF_VM.ITEM WHERE OLD_NBR IN (?OLD_NBRS)
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
PRE_FCST AS (
  SELECT
    A.STORE_NBR,
		A.ITEM_NBR,
		A.WM_YR_WK,
		SUM(A.SALES_FCST_EACH_QTY) AS WK_FCST_QTY
	FROM
		MX_CF_VM.STORE_ITEM_FCST_WK_CONV A
	INNER JOIN
	  (SELECT DISTINCT WM_YR_WK FROM MX_CF_VM.CALENDAR_DAY WHERE GREGORIAN_DATE = CURRENT_DATE) CURRWK
	ON A.FCST_WM_YR_WK = CURRWK.WM_YR_WK
	WHERE
	  A.WM_YR_WK >= ('?WK_INICIO')
    AND A.WM_YR_WK <= ('?WK_FINAL')
	GROUP BY 1, 2, 3
),
--SUBCONSULTAS PRIMARIAS
FCST AS (
  SELECT
  ITEMS.ITEM_NBR,
  PRE_FCST.STORE_NBR,
  (AVG(PRE_FCST.WK_FCST_QTY) / 7) AS AVG_DLY_FCST
  FROM
    PRE_FCST
  INNER JOIN ITEMS ON PRE_FCST.ITEM_NBR = ITEMS.ITEM_NBR
  GROUP BY 1, 2
),
SSG AS (
  SELECT
    ITEMS.ITEM_NBR,
    T3.STORE_NBR AS STORE_NBR,
    T3.SAFETY_EACH_QTY AS SSGanador
  FROM
    MX_CF_REPL_VM.GRS_FCST_DEMAND T3
  INNER JOIN ITEMS ON ITEMS.ITEM_NBR = T3.ITEM_NBR
  WHERE
    T3.FCST_DATE = DATE
),
SST AS (
  SELECT DISTINCT
    ITEMS.ITEM_NBR,
    T2.DEST_STORE_NBR AS STORE_NBR,
    SUM(T2.PRESENTATION_QTY) AS SSPress
  FROM
    MX_CF_REPL_VM.GRS_PRESENTATION_PARM T2
  INNER JOIN ITEMS ON ITEMS.ITEM_NBR = T2.ITEM_NBR
  WHERE
    SS_PRESN_SOURCE_NAME <> 'DEFAULT'
    AND EFF_DATE <= DATE
    AND EXPIRE_DATE > DATE
  GROUP BY 1, 2
),
BPR AS (
  SELECT
    ITEM_NBR,
    DEST_STORE_NBR AS STORE_NBR,
    MAX(PRESENTATION_QTY) AS Base_Press
  FROM
    MX_CF_REPL_VM.GRS_PRESENTATION_PARM
  WHERE
    SS_PRESN_SOURCE_NAME = 'DEFAULT'
    AND EFF_DATE <= DATE
    AND (EXPIRE_DATE > DATE OR EXPIRE_DATE = '1970-01-01')
  GROUP BY 1, 2
),
MAX_SS AS (
  SELECT
    ITEMS.ITEM_NBR,
    FULF.STORE_NBR,
    FULF.MAX_SS_QTY AS MAXSS,
    FULF.MIN_SS_QTY AS MINSS,
    FULF.MIN_SS_SUPPLY_DAY_QTY AS SSCOV
  FROM
    FULF
  INNER JOIN ITEMS ON ITEMS.ITEM_NBR = FULF.ITEM_NBR
),
SSC AS (
  SELECT
    FULF.ITEM_NBR,
    FULF.STORE_NBR,
    ZEROIFNULL(EVENT.SS_ADJUST_INV_DAY_QTY) AS SSTEMP
  FROM
    FULF
  LEFT JOIN EVENT ON FULF.SS_EVENT_NAME = EVENT.SS_EVENT_NAME
)
SELECT
  SSG.ITEM_NBR,
  SSG.STORE_NBR,
  NEGOCIO,
  
  ZEROIFNULL(CAST(SST.SSPRESS AS DECIMAL(19, 2))) AS SSPRESS,
  ZEROIFNULL(CAST(BPR.BASE_PRESS AS DECIMAL(19, 2))) AS BASE_PRESS,
  ZEROIFNULL(CAST((SSPRESS + BASE_PRESS) AS DECIMAL(19, 2))) AS SSPRESS_TOT,
  ZEROIFNULL(MAX_SS.SSCOV * AVG_DLY_FCST) AS SSCOV,
  ZEROIFNULL(SSC.SSTEMP * AVG_DLY_FCST) AS SSTEMP,
  ZEROIFNULL(SSCOV + SSTEMP) AS SSCOV_TOT,
  ZEROIFNULL(CAST(MAX_SS.MINSS AS DECIMAL(19, 2))) AS MIN_SS,
  ZEROIFNULL(CAST(MAX_SS.MAXSS AS DECIMAL(19, 2))) AS MAX_SS,
  ZEROIFNULL(CAST(SSG.SSGANADOR AS DECIMAL(19, 2))) AS SS_GANADOR,
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
    --WHEN SS_GANADOR = (SSPRESS / 2) THEN 'SSPRESS'
    WHEN SSCOV BETWEEN (SS_GANADOR * 0.9) AND (SS_GANADOR * 1.1) THEN 'SSCOV'
    WHEN SSTEMP BETWEEN (SS_GANADOR * 0.9) AND (SS_GANADOR * 1.1) THEN 'SSTEMP'
    WHEN SSCOV_TOT BETWEEN (SS_GANADOR * 0.9) AND (SS_GANADOR * 1.1) THEN 'SSCOV_TOT'
    ELSE 'OTRO'
  END AS GANADOR
FROM
  SSG
LEFT  JOIN SST ON    SSG.ITEM_NBR = SST.ITEM_NBR    AND SSG.STORE_NBR = SST.STORE_NBR
LEFT  JOIN BPR ON    SSG.ITEM_NBR = BPR.ITEM_NBR    AND SSG.STORE_NBR = BPR.STORE_NBR
LEFT  JOIN MAX_SS ON SSG.ITEM_NBR = MAX_SS.ITEM_NBR AND SSG.STORE_NBR = MAX_SS.STORE_NBR
LEFT  JOIN SSC ON    SSG.ITEM_NBR = SSC.ITEM_NBR    AND SSG.STORE_NBR = SSC.STORE_NBR
LEFT  JOIN FCST ON   SSG.ITEM_NBR = FCST.ITEM_NBR   AND SSG.STORE_NBR = FCST.STORE_NBR
INNER JOIN STORES ON                                    SSG.STORE_NBR = STORES.STORE_NBR
