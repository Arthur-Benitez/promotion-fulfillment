WITH
OLDS AS
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

TDAS AS
  (SELECT STORE_NBR FROM MX_CF_VM.STORE_INFO WHERE OPEN_STATUS NOT IN (0, 3, 7, 6, 8)),

NEGO AS
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
	  AND NEGOCIO IN ('?NEGOCIO')),

XREF AS
  (SELECT PRIME_XREF_ITEM_NBR, ITEM_NBR	FROM MX_CF_VM.ITEM WHERE OLD_NBR IN (?OLD_NBRS)),

CURRWK AS
  (SELECT DISTINCT WM_YR_WK FROM MX_CF_VM.CALENDAR_DAY WHERE GREGORIAN_DATE = CURRENT_DATE),

PRE_FCST AS
  (SELECT
    A.STORE_NBR,
		A.ITEM_NBR,
		A.WM_YR_WK,
		SUM(A.SALES_FCST_EACH_QTY) AS WK_FCST_QTY
	FROM
		MX_CF_VM.STORE_ITEM_FCST_WK_CONV A
	INNER JOIN CURRWK ON A.FCST_WM_YR_WK = CURRWK.WM_YR_WK
	GROUP BY 1, 2, 3),
	
TG AS
  (SELECT DISTINCT
		A.STORE_NBR,
		OLDS.OLD_NBR,
		OLDS.ITEM_NBR,
		NEGO.NEGOCIO
	FROM
		MX_CF_VM.INFOREM_MANAGED_SKU A
	INNER JOIN OLDS ON A.ITEM_NBR = OLDS.ITEM_NBR
	INNER JOIN TDAS ON A.STORE_NBR = TDAS.STORE_NBR
	INNER JOIN NEGO ON A.STORE_NBR = NEGO.STORE_NBR
	WHERE
		CARRY_OPTION IN ('R')
		AND CARRIED_STATUS IN ('R')),

FCST AS
  (SELECT
    XREF.PRIME_XREF_ITEM_NBR,
		PRE_FCST.STORE_NBR,
		PRE_FCST.WM_YR_WK,
		PRE_FCST.WK_FCST_QTY
	FROM
		XREF
	INNER JOIN PRE_FCST ON PRE_FCST.ITEM_NBR = XREF.ITEM_NBR),

POS AS
  (SELECT
    VTAS.WM_YR_WK,
		VTAS.STORE_NBR,
		XREF.PRIME_XREF_ITEM_NBR,
		SUM(CASE WHEN VTAS.WKLY_QTY < 0 THEN 0 ELSE VTAS.WKLY_QTY END) AS WKLY_QTY
	FROM
		MX_CF_VM.SKU_DLY_POS AS VTAS
	INNER JOIN XREF	ON VTAS.ITEM_NBR = XREF.ITEM_NBR
	INNER JOIN CURRWK ON (VTAS.WM_YR_WK < CURRWK.WM_YR_WK AND VTAS.WM_YR_WK >= CURRWK.WM_YR_WK - 200)
	WHERE
		VTAS.REPORT_CODE NOT IN (8)
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
