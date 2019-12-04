WITH STORES AS (
  SELECT DISTINCT
    TR.STORE_NBR,
    T.STORE_NAME,
		TR.TRAIT_NBR,
		T.OPEN_STATUS,
		T.OPEN_DATE,
		T.STATE,
		CASE WHEN TR.TRAIT_NBR = 297 THEN 'SUPERCENTER'
		  WHEN TR.TRAIT_NBR = 9 THEN 'BODEGA'
		  WHEN TR.TRAIT_NBR = 11 THEN 'SUPERAMA'
		  WHEN TR.TRAIT_NBR = 1312 THEN 'MIBODEGA'
		  WHEN TR.TRAIT_NBR = 969 THEN 'BAE'
		  WHEN TR.TRAIT_NBR = 136 THEN 'MEDIMART'
		  ELSE 'OTRO'
		END AS NEGOCIO
  FROM
  	MX_CF_VM.TRAIT_STORE AS TR
    INNER JOIN MX_CF_VM.STORE_INFO AS T
      ON TR.STORE_NBR = T.STORE_NBR
	WHERE
	  NEGOCIO IN (?NEGOCIOS)
	  AND (?COND_STR)
	  AND TR.TRAIT_NBR IN (11, 297, 1312, 969, 136, 9)
    AND T.OPEN_STATUS NOT IN (0, 3, 7, 6, 8)
),

CIDS AS (
  SELECT
    DEPT_NBR,
    REPL_GROUP_NBR,
    ITEM_NBR
  FROM
    MX_CF_VM.ITEM A
  WHERE
  	(TRIM(DEPT_NBR)||'.'||TRIM(REPL_GROUP_NBR)) IN (
      SELECT DISTINCT
        (TRIM(DEPT_NBR)||'.'||TRIM(REPL_GROUP_NBR))
      FROM
        MX_CF_VM.ITEM
      WHERE
        OLD_NBR IN (?OLD_NBRS)
    )
),

ITEMS_INFO AS (
	SELECT
		I.ITEM_NBR,
		I.PRIME_XREF_ITEM_NBR,
		I.OLD_NBR,
		CAST ((I.VENDOR_NBR * 1000) + (I.VENDOR_DEPT_NBR * 10) + (I.VENDOR_SEQ_NBR) AS INTEGER) AS VENDOR9_NBR,
		I.REPL_GROUP_NBR,
		I.MDSE_CATG_NBR,
		I.SHLF_LIFE_DAYS_QTY,
		CASE
			WHEN I.BRAND_ID IN (
				312625,312627,312624,312633,312626,312629,312631,312630, 312634,312635,312628,312632,315263,
				317731,312648,314450,321588,314614,316405,328877,328870,328774,332303,335021,335023,35347
				)
			THEN 'MARCAS ESTRATEGICAS'
			ELSE 'REGULAR'
		END AS MARCAS_ESTRATEGICAS,	
		ID.ACCT_DEPT_NBR,
		ID.VENDOR_NBR,
		ID.VENDOR_NAME,
		ID.UPC,
		ID.PRIMARY_DESC,
		ID.SIZE_DESC,
		ID.COST,
		ID.VNPK_QTY,
		ID.WHPK_QTY,
		ID.STATUS_CODE,
		ID.TYPE_CODE,
		ID.ITM_MBM_CODE,
		ID.ORDBK_FLAG,
		ID.ORDER_BOOK_SEQUENCE_NUMBER AS SUB_TIPO,
		ID.CANCEL_WHEN_OUT_FLAG,
		ID.ITEM_STORE_TYPE,
		ID.FINELINE,
		SUBSTR(ID.FINELINE,1,2) AS CATEGORY_NBR,
		ID.EFFECTIVE_DATE				
	FROM
		MX_CF_VM.ITEM I
		INNER JOIN MX_CF_VM.ITEM_DESC ID
			ON I.ITEM_NBR = ID.ITEM_NBR
		INNER JOIN CIDS
			ON I.ITEM_NBR = CIDS.ITEM_NBR
	WHERE
		ID.STATUS_CODE = 'A'
		AND ID.ORDBK_FLAG = 'Y'
		AND ID.CANCEL_WHEN_OUT_FLAG = 'N'
		AND ID.ITM_MBM_CODE IN ('M', 'I')
),

INV AS (
	SELECT
		CIDS.REPL_GROUP_NBR,
		CIDS.DEPT_NBR,
		STORES.STORE_NBR,
		INV.CARRY_OPTION,
		INV.CARRIED_STATUS,
		SUM(CAST(GREATEST(INV.ON_HAND_1_QTY, 0.00) AS DECIMAL (28,2))) AS OHQTY,
		SUM(CAST(GREATEST(INV.IN_TRANSIT_QTY, 0.00) AS DECIMAL (28,2))) AS ITQTY,
		SUM(CAST(GREATEST(INV.IN_WHS_QTY, 0.00) AS DECIMAL (28,2))) AS IWQTY,
		SUM(CAST(GREATEST(INV.ON_ORDER_QTY, 0.00) AS DECIMAL (28,2))) AS OOQTY
	FROM
		MX_CF_VM.INFOREM_MANAGED_SKU INV
		INNER JOIN CIDS
			ON INV.ITEM_NBR = CIDS.ITEM_NBR
		INNER JOIN STORES
			ON INV.STORE_NBR = STORES.STORE_NBR
	WHERE
		INV.CARRY_OPTION IN ('R')
		AND INV.CARRIED_STATUS IN ('R')
	GROUP BY
		1, 2, 3, 4, 5
),

BASE AS (
	SELECT DISTINCT
		COMBS.SOURCE_DC_NBR AS DC_NBR,
		DC.DC_NAME,
		CIDS.ITEM_NBR,
		CIDS.REPL_GROUP_NBR,
		CIDS.DEPT_NBR,
		ST.STORE_NBR,
		ST.STORE_NAME,
		ST.OPEN_STATUS,
		ST.STATE,
		ST.NEGOCIO
	FROM
		MX_CF_REPL_VM.GRS_FULFILLMENT_PARM AS COMBS
		INNER JOIN CIDS
			ON COMBS.ITEM_NBR = CIDS.ITEM_NBR
		INNER JOIN STORES ST
			ON COMBS.STORE_NBR = ST.STORE_NBR
		LEFT JOIN WW_CORE_DIM_VM.DC_DIM AS DC
			ON COMBS.SOURCE_DC_NBR = DC.DC_NBR
	WHERE
		DC.COUNTRY_CODE IN ('MX')
		AND DC.CURRENT_IND IN ('Y')
),

VENDOR_STATUS AS (
	SELECT
		CAST ((VENDOR_NBR * 1000) + (DEPT_NBR * 10) + (VENDOR_SEQ_NBR) AS INTEGER) AS VENDOR9,
		JDA_VNDR_STAT_CD AS GRS_STATUS,
		VENDOR_TYPE_CD
	FROM
		MX_CF_REPL_VM.GRS_VENDOR_AGREEMENT
	GROUP BY 
		1, 2, 3
),

PRE_POS AS (
	SELECT
		CIDS.REPL_GROUP_NBR,
		CIDS.DEPT_NBR,
		STORES.STORE_NBR,
		SALES.WM_YR_WK,
		SUM(GREATEST(SALES.WKLY_QTY, 0)) AS WKLY_QTY				
	FROM 
		MX_CF_VM.SKU_DLY_POS AS SALES
		INNER JOIN CIDS
			ON FCST.ITEM_NBR = CIDS.ITEM_NBR
		INNER JOIN STORES
			ON FCST.STORE_NBR = STORES.STORE_NBR
	WHERE
		SALES.REPORT_CODE NOT IN (8)
		AND SALES.WM_YR_WK >= ('?WK_INICIO')
		AND SALES.WM_YR_WK <= ('?WK_FINAL')
	GROUP BY 1, 2, 3, 4
),

POS AS (
	SELECT
		REPL_GROUP_NBR,
		DEPT_NBR,
		STORE_NBR,	
		SUM(WKLY_QTY) / (7.000 * (SELECT COUNT(DISTINCT WM_YR_WK)	FROM MX_CF_VM.CALENDAR_DAY WHERE WM_YR_WK BETWEEN (?WK_INICIO) AND (?WK_FINAL))) AS AVG_DLY_QTY 
	FROM
		PRE_POS
	GROUP BY 1, 2, 3
),

SELECT
	(TRIM(BASE.DEPT_NBR)||'.'||TRIM(II.OLD_NBR)||'.'||TRIM(BASE.NEGOCIO)) AS DISPLAY_KEY,
	BASE.ITEM_NBR,
	BASE.STORE_NBR,
	II.VENDOR9_NBR,
	II.VENDOR_NAME,
	ZEROIFNULL(VS.GRS_STATUS) AS STATUS_GRS,
	BASE.REPL_GROUP_NBR,
	BASE.DEPT_NBR,
	BASE.DC_NBR,
	BASE.DC_NAME,
	II.OLD_NBR,
	II.PRIMARY_DESC,
	II.SIZE_DESC,
	II.FINELINE,
	II.COST,
	II.VNPK_QTY,
	II.WHPK_QTY,
	BASE.STORE_NAME,
	BASE.NEGOCIO,
	BASE.STATE,
	BASE.OPEN_STATUS,
	II.STATUS_CODE,
	INV.CARRY_OPTION,
	INV.CARRIED_STATUS,
	II.TYPE_CODE,
	II.ITM_MBM_CODE,
	II.CANCEL_WHEN_OUT_FLAG,
	II.ORDBK_FLAG,
	II.SUB_TIPO,
	II.CATEGORY_NBR,
	ZEROIFNULL(INV.OHQTY) AS OH_QTY,
	ZEROIFNULL(POS.AVG_DLY_QTY) AS AVG_DLY_POS
FROM
	BASE
	INNER JOIN ITEMS_INFO II
		ON BASE.ITEM_NBR = II.ITEM_NBR
	LEFT JOIN VENDOR_STATUS VS
		ON II.VENDOR9_NBR = VS.VENDOR9
	LEFT JOIN INV
		ON BASE.REPL_GROUP_NBR = INV.REPL_GROUP_NBR AND BASE.DEPT_NBR = INV.DEPT_NBR AND BASE.STORE_NBR = INV.STORE_NBR
	LEFT JOIN POS
		ON BASE.REPL_GROUP_NBR = POS.REPL_GROUP_NBR AND BASE.DEPT_NBR = POS.DEPT_NBR AND BASE.STORE_NBR = POS.STORE_NBR
WHERE
	DISPLAY_KEY IN (?KEY)
