SELECT DISTINCT

DISPLAY_KEY,
ACCT_DEPT_NBR,
VENDOR9_NBR,
VENDOR_NAME,
CASE WHEN GRS_STATUS IS NULL THEN 0 ELSE GRS_STATUS END AS STATUS_GRS,
CID,
TG.ITEM_NBR,
TG.OLD_NBR,
PRIMARY_DESC,
SIZE_DESC,
TG.FINELINE,
COST,
VNPK_QTY,
WHPK_QTY,
TG.STORE_NBR,
STORE_NAME,
NEGOCIO,
STATE,
OPEN_STATUS,
STATUS_CODE,
CARRY_OPTION,
CARRIED_STATUS,
TYPE_CODE,
ITM_MBM_CODE,
CANCEL_WHEN_OUT_FLAG,
ORDBK_FLAG,
SUB_TIPO,
CATEGORY_NBR,
CUMLT,

SUM(FCST.AVG_DLY_FCST) AS AVG_DLY_FCST
															

FROM

										(
																				SELECT DISTINCT
																				TRIM(T3.ACCT_DEPT_NBR)||'.'||TRIM(T3.OLD_NBR)||'.'||TRIM(T5.NEGOCIO) AS DISPLAY_KEY,
																				TRIM(T3.ACCT_DEPT_NBR)||'.'||TRIM(SUBSTR(T3.FINELINE,1,2)) AS LLAVEDEPCAT,
																				TRIM(T3.UPC)||TRIM(T1.STORE_NBR) AS LLAVEUPC,
																				
																				T3.ACCT_DEPT_NBR,
																				T3.VENDOR_NBR,
																				CAST ((T2.VENDOR_NBR*1000)+(T2.VENDOR_DEPT_NBR*10)+(T2.VENDOR_SEQ_NBR) AS INTEGER ) AS VENDOR9_NBR,
																				T3.VENDOR_NAME,
																				T3.UPC,
																				T2.REPL_GROUP_NBR AS CID,
																				T3.ITEM_NBR,
																				T3.OLD_NBR,
																				T3.PRIMARY_DESC,
																				T3.SIZE_DESC,
																				T3.COST,
																				T3.VNPK_QTY,
																				T3.WHPK_QTY,
																				T3.STATUS_CODE,
																				T3.TYPE_CODE,
																				T3.ITM_MBM_CODE,
																				T3.ORDBK_FLAG,
																				T3.ORDER_BOOK_SEQUENCE_NUMBER AS SUB_TIPO,
																				T3.CANCEL_WHEN_OUT_FLAG,
																				T3.ITEM_STORE_TYPE,
																				T3.FINELINE,
																				SUBSTR(T3.FINELINE,1,2) AS CATEGORY_NBR,
																				T2.MDSE_CATG_NBR,
																				T3.EFFECTIVE_DATE,			
																				T2.SHLF_LIFE_DAYS_QTY,
																				CASE WHEN T2.BRAND_ID IN (312625,312627,312624,312633,312626,312629,312631,312630, 312634,312635,312628,312632,315263,317731,312648,314450,321588,314614,316405,328877,328870,328774,332303,335021,335023,35347) THEN 'MARCAS ESTRATEGICAS' ELSE 'REGULAR' END AS MARCAS_ESTRATEGICAS,																																																																																																																																	
																				
																				T1.STORE_NBR,
																				T4.STORE_NAME,
																				T5.NEGOCIO,
																				T4.OPEN_STATUS,
																				T4.OPEN_DATE,
																				T4.STATE,
																				T6.CVE_NIELSEN,
																				T6.PROTOTYPE_NBR,
																				T1.CARRY_OPTION,
																				T1.CARRIED_STATUS,
																				SUM(CAST (CASE WHEN T1.ON_HAND_1_QTY<0.00 THEN  0.00 ELSE  T1.ON_HAND_1_QTY END AS DECIMAL (28,2))) AS OHQTY,
																				SUM(CAST (CASE WHEN T1.ON_HAND_1_QTY<0.00 THEN  0.00 ELSE  T1.ON_HAND_1_QTY*T3.COST END AS DECIMAL (28,2))) AS OHCOST,
																				
																				SUM(CAST (CASE WHEN T1.IN_TRANSIT_QTY<0.00 THEN  0.00 ELSE  T1.IN_TRANSIT_QTY END AS DECIMAL (28,2))) AS ITQTY,
																				SUM(CAST (CASE WHEN T1.IN_TRANSIT_QTY<0.00 THEN  0.00 ELSE  T1.IN_TRANSIT_QTY*T3.COST END AS DECIMAL (28,2))) AS ITCOST,
																				
																				SUM(CAST (CASE WHEN T1.IN_WHS_QTY<0.00 THEN  0.00 ELSE  T1.IN_WHS_QTY END AS DECIMAL (28,2))) AS IWQTY,
																				SUM(CAST (CASE WHEN T1.IN_WHS_QTY<0.00 THEN  0.00 ELSE  T1.IN_WHS_QTY*T3.COST END AS DECIMAL (28,2))) AS IWCOST,
																				
																				SUM(CAST (CASE WHEN T1.ON_ORDER_QTY<0.00 THEN  0.00 ELSE  T1.ON_ORDER_QTY END AS DECIMAL (28,2))) AS OOQTY,
																				SUM(CAST (CASE WHEN T1.ON_ORDER_QTY<0.00 THEN  0.00 ELSE  T1.ON_ORDER_QTY*T3.COST END AS DECIMAL (28,2))) AS OOCOST
																				
																				
																				FROM
																				MX_CF_VM.INFOREM_MANAGED_SKU	T1,
																				MX_CF_VM.ITEM T2,
																				MX_CF_VM.ITEM_DESC T3,
																				MX_CF_VM.STORE_INFO T4,
																				WM_AD_HOC.T_TDAS T6,
																				(SELECT DISTINCT STORE_NBR, TRAIT_NBR, CASE WHEN TRAIT_NBR=297 THEN 'SUPERCENTER' WHEN TRAIT_NBR=9 THEN 'BODEGA' WHEN TRAIT_NBR=11 THEN 'SUPERAMA' WHEN TRAIT_NBR=1312 THEN 'MIBODEGA' WHEN TRAIT_NBR=969 THEN 'BAE'WHEN TRAIT_NBR=136 THEN 'MEDIMART' ELSE 'OTRO' END AS NEGOCIO FROM MX_CF_VM.TRAIT_STORE WHERE TRAIT_NBR IN (11,297,1312,969,136,9)) T5 
																				
																				
																				WHERE
																				T1.ITEM_NBR=T2.ITEM_NBR
																				AND T1.ITEM_NBR=T3.ITEM_NBR
																				AND T4.STORE_NBR = T1.STORE_NBR
																				AND T4.STORE_NBR=T5.STORE_NBR
																				AND T4.STORE_NBR=T6.WM_TIENDA
																				AND DISPLAY_KEY IN (	?KEY)
																				--AND T3.ACCT_DEPT_NBR IN (13)
																				--AND T3.OLD_NBR IN ( 1301978 	)
																				--AND T5.NEGOCIO LIKE ('BAE')
																				AND CARRY_OPTION IN ('R')
																				AND CARRIED_STATUS IN ('R')
																				AND OPEN_STATUS NOT IN (0,3,7,6,8)
																				--AND T4.STORE_NBR IN (?STORE_NBR)
																				
																				--AND T2.REPL_GROUP_NBR IN (?CID)
																				
																				
																				GROUP BY
																				1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39
																				
										) AS TG
										
										LEFT JOIN
																																
										(
	
																				SELECT
																				CAST ((VENDOR_NBR*1000)+(DEPT_NBR*10)+(VENDOR_SEQ_NBR) AS INTEGER ) AS VENDOR9,
																				JDA_VNDR_STAT_CD AS GRS_STATUS,
																				VENDOR_TYPE_CD
																				
																				FROM
																				MX_CF_REPL_VM.GRS_VENDOR_AGREEMENT
																				
																				GROUP BY 
																				1,2,3
																	
										) AS VENDOR_STATUS
																															
										ON (VENDOR_STATUS.VENDOR9=TG.VENDOR9_NBR)
										
										
										LEFT JOIN
										
										(

																				SELECT

																				STORE_NBR,
																				PRIME_XREF_ITEM_NBR,
																		    AVG(	WK_FCST_QTY) AS AVG_DLY_FCST
																				
																				
																				FROM 
																				
																				(
																				
																														SELECT
																														
																														T1.WM_YR_WK,
																														T2.STORE_NBR,
																														--I.OLD_NBR,
																														--I.ITEM_NBR,
																														I.PRIME_XREF_ITEM_NBR,
																														T2.WK_FCST_QTY
																														
																														
																														
																														
																														FROM
																														
																													  MX_CF_VM.CALENDAR_DAY T1,
																														(SELECT STORE_NBR, ITEM_NBR, WM_YR_WK, SUM(SALES_FCST_EACH_QTY)AS WK_FCST_QTY  FROM MX_CF_VM.STORE_ITEM_FCST_WK_CONV  GROUP BY 1,2,3  WHERE FCST_WM_YR_WK = (SELECT DISTINCT WM_YR_WK FROM MX_CF_VM.CALENDAR_DAY WHERE GREGORIAN_DATE= CURRENT_DATE)  ) T2,
																														MX_CF_VM.ITEM I,
																														MX_CF_VM.ITEM_DESC T3
																														
																														WHERE
																														T1.WM_YR_WK=T2.WM_YR_WK
																														AND T2.ITEM_NBR=I.ITEM_NBR
																														AND T2.ITEM_NBR=T3.ITEM_NBR
																														AND T1.WM_YR_WK >=('?WK_INICIO')
																														AND T1.WM_YR_WK <=('?WK_FINAL')
																														--AND I.OLD_NBR IN ( 1344075 )
																														--AND T2.STORE_NBR IN ( 1019 )
																														
																													

																					) AS BF

										GROUP BY 1,2
																				
																													
										) AS FCST
										
										ON (FCST.PRIME_XREF_ITEM_NBR = TG.ITEM_NBR AND FCST.STORE_NBR = TG.STORE_NBR)																																																																																																																																																													
										
										LEFT JOIN 
										
										(

																				SELECT 
																				
																				WMT_ITEM_NBR,
																				SUBSTR(LOC,12,4)  AS LOC_NBR,
																				CASE WHEN LEFT(LOC_NBR,1) = '0' THEN SUBSTR(LOC_NBR,2,3) ELSE LOC_NBR END (NAMED STORE_NBR),
																				CUMLT
																				
																				
																				FROM WM_AD_HOC.TABLALT
																				
																				--WHERE
																			 	--WMT_ITEM_NBR IN (?OLD_NBR)

										) AS CLT
										
										ON (TG.STORE_NBR=CLT.STORE_NBR AND TG.OLD_NBR=CLT.WMT_ITEM_NBR)


GROUP BY 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29						
