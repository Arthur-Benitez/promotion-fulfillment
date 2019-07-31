WITH
ITEMS AS (
	SELECT
		ITEM_NBR,
		OLD_NBR,
		REPL_GROUP_NBR
	FROM
		MX_CF_VM.ITEM
	WHERE
		OLD_NBR IN (9516207)
		--OLD_NBR IN (?OLD_NBRS)
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
		NEGOCIO IN ('SUPERCENTER')
		--NEGOCIO IN ('?NEGOCIOS')
),
STR_ITEM_QTY AS (
	SELECT
		GRS_PROMOTION_ID,
		ITEM_NBR,
		OLD_NBR,
		SUBSTR(GRS_LOC_ID, 12, 4) AS STORE_NBR,
		OVRD_PRESENTATION_QTY AS SSPRES
	FROM
		MX_CF_REPL_VM.GRS_DMDUNIT_PROMO A
		INNER JOIN ITEMS ON (A.REPL_GROUP_NBR = ITEMS.REPL_GROUP_NBR)
		INNER JOIN STORES ON (SUBSTR(GRS_LOC_ID, 12, 4) = STORES.STORE_NBR)
	WHERE
		OVRD_PRESENTATION_QTY > 0
),
PROMO_DET AS (
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
		AND (PROMO_START_DATE <= '2019-08-07' AND PROMO_END_DATE >= '2019-08-01')
		--AND (PROMO_START_DATE <= '?END_DATE' AND PROMO_END_DATE >= '?START_DATE')
),
STR_ITEM_PRO AS (
	SELECT
		ITEM_NBR,
		OLD_NBR,
		STORE_NBR,
		SSPRES,
		PROMO_CREATE_DATE,
		PROMO_PRIORITY_NBR,
		ADDITIVE_SWITCH_CNT
	FROM
		STR_ITEM_QTY
		INNER JOIN PROMO_DET ON
			STR_ITEM_QTY.GRS_PROMOTION_ID = PROMO_DET.GRS_PROMOTION_ID
),
STR_ITEM_PRO_DAY AS (
	SELECT
		CAL.GREGORIAN_DATE,
		STR_ITEM_PRO.*
	FROM
		STR_ITEM_PRO
		RIGHT JOIN (
			SELECT
				GREGORIAN_DATE
			FROM
				MX_CF_VM.CALENDAR_DAY
			WHERE
				'2019-08-01' <= GREGORIAN_DATE
				AND GREGORIAN_DATE <= '2019-08-07'
				--'?START_DATE' <= GREGORIAN_DATE
				--AND GREGORIAN_DATE <= '?END_DATE'
		) CAL
			ON 1=1
),
ADD_TRUE AS (
	SELECT
		GREGORIAN_DATE,
		ITEM_NBR,
		OLD_NBR,
		STORE_NBR,
		SUM(SSPRES) AS SSPRES
	FROM
		STR_ITEM_PRO_DAY
	WHERE
		ADDITIVE_SWITCH_CNT = 1
	GROUP BY 1, 2, 3, 4
),
ADD_FALSE AS (
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
			STR_ITEM_PRO_DAY.*
		FROM
			STR_ITEM_PRO_DAY
		--WHERE
			--ADDITIVE_SWITCH_CNT = 0
	) A
	WHERE
		FILA = 1
)
SELECT
	ITEM_NBR,
	OLD_NBR,
	STORE_NBR,
	AVG(SSPRES) AS SSPRES_AVG
FROM (
	SELECT
		ITEM_NBR,
		OLD_NBR,
		STORE_NBR,
		GREGORIAN_DATE,
		SUM(SSPRES) AS SSPRES
	FROM (
		SELECT * FROM ADD_TRUE
		UNION ALL
		SELECT * FROM ADD_FALSE
	) A
	GROUP BY 1, 2, 3, 4
) A
GROUP BY 1, 2, 3
ORDER BY 1, 2, 3
