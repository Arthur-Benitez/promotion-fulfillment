WITH
ITEMS AS (
	SELECT
		ITEM_NBR,
		OLD_NBR,
		REPL_GROUP_NBR
	FROM
		MX_CF_VM.ITEM
	WHERE
		OLD_NBR IN (?OLD_NBRS)
		AND ITEM_STATUS_CODE = 'A'
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
),
FUT_STR_ITEM_QTY AS (
	SELECT
		A.GRS_PROMOTION_ID,
		ITEMS.ITEM_NBR,
		ITEMS.OLD_NBR,
		SUBSTR(A.GRS_LOC_ID, 12, 4) AS STORE_NBR,
		A.OVRD_PRESENTATION_QTY AS SSPRES
	FROM
		MX_CF_REPL_VM.GRS_DMDUNIT_PROMO AS A
		INNER JOIN ITEMS ON (A.REPL_GROUP_NBR = ITEMS.REPL_GROUP_NBR)
		INNER JOIN STORES ON (SUBSTR(GRS_LOC_ID, 12, 4) = STORES.STORE_NBR)
),
FUT_PROMO_DET AS (
	SELECT
		GRS_PROMOTION_ID,
		PROMO_CREATE_DATE,
		PROMO_START_DATE,
		(PROMO_START_DATE + (PROMO_DURATION_NBR / 1440)) AS PROMO_END_DATE,
		PROMO_PRIORITY_NBR,
		ADDITIVE_SWITCH_CNT
	FROM
		MX_CF_REPL_VM.GRS_PROMOTION
	WHERE
		PROMO_STATUS_TYPE_CD IN (7, 9)
		AND PROMOTION_ID NOT LIKE 'DEFAULT'
		AND PROMOTION_ID NOT LIKE 'ARR-PLAN'
		AND PROMOTION_ID NOT LIKE 'Discontinuation PROFILE'
		AND PROMOTION_ID NOT LIKE 'APER%'
		--Starts before end date and ends after start date (all the ones overlaping the study period)
		AND (PROMO_START_DATE <= '?END_DATE' AND PROMO_END_DATE >= '?START_DATE')
),
FUT_STR_ITEM_PRO AS (
	SELECT DISTINCT
		A.ITEM_NBR,
		A.OLD_NBR,
		A.STORE_NBR,
		A.SSPRES,
		B.PROMO_START_DATE,
		B.PROMO_END_DATE,
		B.PROMO_CREATE_DATE,
		B.PROMO_PRIORITY_NBR,
		B.ADDITIVE_SWITCH_CNT
	FROM
		FUT_STR_ITEM_QTY AS A
		INNER JOIN FUT_PROMO_DET AS B ON
			A.GRS_PROMOTION_ID = B.GRS_PROMOTION_ID
),
FUT_STR_ITEM_PRO_DAY AS (
	SELECT
		CAL.GREGORIAN_DATE,
		PROMO.*
	FROM
		FUT_STR_ITEM_PRO AS PROMO
		RIGHT JOIN MX_CF_VM.CALENDAR_DAY AS CAL
			ON CAL.GREGORIAN_DATE BETWEEN PROMO.PROMO_START_DATE AND PROMO.PROMO_END_DATE
	WHERE
		CAL.GREGORIAN_DATE BETWEEN ('?START_DATE') AND ('?END_DATE')
),
FUT_ADD_TRUE AS (
	SELECT
		GREGORIAN_DATE,
		ITEM_NBR,
		OLD_NBR,
		STORE_NBR,
		SUM(SSPRES) AS SSPRES
	FROM
		FUT_STR_ITEM_PRO_DAY
	WHERE
		ADDITIVE_SWITCH_CNT = 1
	GROUP BY 1, 2, 3, 4
),
FUT_ADD_FALSE AS (
	SELECT
		GREGORIAN_DATE,
		ITEM_NBR,
		OLD_NBR,
		STORE_NBR,
		SSPRES
	FROM (
		SELECT
			ROW_NUMBER() OVER(
				PARTITION BY ITEM_NBR, STORE_NBR, GREGORIAN_DATE
				ORDER BY PROMO_PRIORITY_NBR, PROMO_CREATE_DATE DESC
			) AS FILA,
			FUT_STR_ITEM_PRO_DAY.*
		FROM
			FUT_STR_ITEM_PRO_DAY
		WHERE
			ADDITIVE_SWITCH_CNT = 0
	) A
	WHERE
		FILA = 1
),
FUT_SSPRES AS (
	SELECT
		ITEM_NBR,
		OLD_NBR,
		STORE_NBR,
		AVG(SSPRES) AS SSPRES
	FROM (
		SELECT
			ITEM_NBR,
			OLD_NBR,
			STORE_NBR,
			GREGORIAN_DATE,
			SUM(SSPRES) AS SSPRES
		FROM (
			SELECT * FROM FUT_ADD_TRUE
			UNION ALL
			SELECT * FROM FUT_ADD_FALSE
		) A
		GROUP BY 1, 2, 3, 4
	) A
	GROUP BY 1, 2, 3
)
SELECT
  FULF.ITEM_NBR,
  ITEMS.OLD_NBR,
  FULF.STORE_NBR,
  STORES.NEGOCIO,
  
  ZEROIFNULL(CAST(BPR.SSPRESS AS DECIMAL(19, 2))) AS SSPRESS,
  ZEROIFNULL(CAST(FUT_SSPRES.SSPRES AS DECIMAL(19, 2))) AS SSPRESS_FUT,
  ZEROIFNULL(CAST(BPR.BASE_PRESS AS DECIMAL(19, 2))) AS BASE_PRESS,
  ZEROIFNULL(CAST((SSPRESS + BASE_PRESS) AS DECIMAL(19, 2))) AS SSPRESS_TOT,
  ZEROIFNULL(CAST((SSPRESS_FUT + BASE_PRESS) AS DECIMAL(19, 2))) AS SSPRESS_FUT_TOT,
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
	LEFT JOIN FUT_SSPRES
		ON FULF.ITEM_NBR = FUT_SSPRES.ITEM_NBR AND FULF.STORE_NBR = FUT_SSPRES.STORE_NBR
	INNER JOIN STORES
		ON FULF.STORE_NBR = STORES.STORE_NBR
	INNER JOIN ITEMS
		ON FULF.ITEM_NBR = ITEMS.ITEM_NBR
