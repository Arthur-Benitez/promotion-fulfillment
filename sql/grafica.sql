WITH
T2 AS
  (SELECT
		ITEM_NBR,
		OLD_NBR
	FROM
		MX_CF_VM.ITEM_DESC
	WHERE
	  OLD_NBR IN (?OLD_NBRS)
		AND STATUS_CODE IN ('A')
		AND ORDBK_FLAG IN ('Y')
		AND CANCEL_WHEN_OUT_FLAG IN ('N')
		AND ITM_MBM_CODE IN ('M', 'I')),

T3 AS
  (SELECT STORE_NBR, OPEN_STATUS FROM MX_CF_VM.STORE_INFO WHERE OPEN_STATUS NOT IN (0, 3, 7, 6, 8)),

T4 AS
  (SELECT DISTINCT
	  STORE_NBR,
		TRAIT_NBR,
		CASE WHEN TRAIT_NBR=297 THEN 'SUPERCENTER'
		  WHEN TRAIT_NBR=9 THEN 'BODEGA'
		  WHEN TRAIT_NBR=11 THEN 'SUPERAMA'
		  WHEN TRAIT_NBR=1312 THEN 'MIBODEGA'
		  WHEN TRAIT_NBR=969 THEN 'BAE'
		  WHEN TRAIT_NBR=136 THEN 'MEDIMART'
		  ELSE 'OTRO'
		END AS NEGOCIO
  FROM
	  MX_CF_VM.TRAIT_STORE
	WHERE
	  TRAIT_NBR IN (11, 297, 1312, 969, 136, 9)
	  AND NEGOCIO LIKE ('?NEGOCIO')),

T5 AS
  (SELECT PRIME_XREF_ITEM_NBR, ITEM_NBR	FROM MX_CF_VM.ITEM WHERE OLD_NBR IN (?OLD_NBRS)),

T6 AS
  (SELECT DISTINCT WM_YR_WK FROM MX_CF_VM.CALENDAR_DAY WHERE GREGORIAN_DATE = CURRENT_DATE),

T7 AS
  (SELECT
    A.STORE_NBR,
		A.ITEM_NBR,
		A.WM_YR_WK,
		SUM(A.SALES_FCST_EACH_QTY) AS WK_FCST_QTY
	FROM
		MX_CF_VM.STORE_ITEM_FCST_WK_CONV A
	INNER JOIN T6 ON A.FCST_WM_YR_WK = T6.WM_YR_WK
	GROUP BY 1, 2, 3),
	
TG AS
  (SELECT DISTINCT
		T1.STORE_NBR,
		T2.OLD_NBR,
		T2.ITEM_NBR,
		T4.NEGOCIO
	FROM
		MX_CF_VM.INFOREM_MANAGED_SKU T1
	INNER JOIN T2 ON T1.ITEM_NBR = T2.ITEM_NBR
	INNER JOIN T3 ON T1.STORE_NBR = T3.STORE_NBR
	INNER JOIN T4 ON T1.STORE_NBR = T4.STORE_NBR
	WHERE
		CARRY_OPTION IN ('R')
		AND CARRIED_STATUS IN ('R')),

FCST AS
  (SELECT
    T5.PRIME_XREF_ITEM_NBR,
		T7.STORE_NBR,
		T7.WM_YR_WK,
		T7.WK_FCST_QTY
	FROM
		T5
	INNER JOIN T7 ON T7.ITEM_NBR = T5.ITEM_NBR),

POS AS
  (SELECT
    T2.WM_YR_WK,
		T2.STORE_NBR,
		T5.PRIME_XREF_ITEM_NBR,
		SUM(CASE WHEN T2.WKLY_QTY < 0 THEN 0 ELSE T2.WKLY_QTY END) AS WKLY_QTY
	FROM
		MX_CF_VM.SKU_DLY_POS AS T2
	INNER JOIN T5	ON T2.ITEM_NBR = T5.ITEM_NBR
	INNER JOIN T6 ON (T2.WM_YR_WK < T6.WM_YR_WK AND T2.WM_YR_WK >= T6.WM_YR_WK - 200)
	WHERE
		T2.REPORT_CODE NOT IN (8)
	GROUP BY 1, 2, 3)

SELECT DISTINCT
  TG.OLD_NBR,
  TG.NEGOCIO,
  FCST.WM_YR_WK,
  'Forecast' AS "type",
  SUM(FCST.WK_FCST_QTY) AS WKLY_QTY
FROM
	TG
LEFT JOIN FCST	ON (FCST.PRIME_XREF_ITEM_NBR = TG.ITEM_NBR AND FCST.STORE_NBR = TG.STORE_NBR)
GROUP BY 1, 2, 3, 4

UNION ALL

SELECT DISTINCT
  TG.OLD_NBR,
  TG.NEGOCIO,
  POS.WM_YR_WK,
  'Ventas' AS "type",
  SUM(POS.WKLY_QTY) AS WKLY_QTY
FROM
	TG
INNER JOIN POS ON (POS.PRIME_XREF_ITEM_NBR = TG.ITEM_NBR AND POS.STORE_NBR = TG.STORE_NBR)
GROUP BY 1, 2, 3

ORDER BY 1, 2, 3
