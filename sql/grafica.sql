WITH

-- Tiendas validas
STORE AS (
  SELECT DISTINCT
	  TR.STORE_NBR,
		TR.TRAIT_NBR,
		CASE WHEN TR.TRAIT_NBR=297 THEN 'SUPERCENTER'
		  WHEN TR.TRAIT_NBR=9 THEN 'BODEGA'
		  WHEN TR.TRAIT_NBR=11 THEN 'SUPERAMA'
		  WHEN TR.TRAIT_NBR=1312 THEN 'MIBODEGA'
		  WHEN TR.TRAIT_NBR=969 THEN 'BAE'
		  WHEN TR.TRAIT_NBR=136 THEN 'MEDIMART'
		  ELSE 'OTRO'
		END AS NEGOCIO
  FROM MX_CF_VM.TRAIT_STORE AS TR
    INNER JOIN MX_CF_VM.STORE_INFO AS T
      ON TR.STORE_NBR = T.STORE_NBR
	WHERE
	  TR.TRAIT_NBR IN (11, 297, 1312, 969, 136, 9)
	  AND NEGOCIO IN (?NEGOCIO)
    AND T.OPEN_STATUS NOT IN (0, 3, 7, 6, 8)
),

-- Articulos referenciados asociados a los old_nbrs (para sacar las ventas cuando cambian el old_nbr)
-- Aqui *NO* se debe aplicar los filtros de validez
XREF AS (
  SELECT
    A.PRIME_XREF_ITEM_NBR,
    ITEM_NBR
  FROM
    MX_CF_VM.ITEM A
    INNER JOIN (
      SELECT
        PRIME_XREF_ITEM_NBR
      FROM
        MX_CF_VM.ITEM
      WHERE
        OLD_NBR IN (?OLD_NBRS)
      ) AS B
      ON A.PRIME_XREF_ITEM_NBR = B.PRIME_XREF_ITEM_NBR
),

CURRWK AS (
  SELECT DISTINCT WM_YR_WK
  FROM MX_CF_VM.CALENDAR_DAY
  WHERE GREGORIAN_DATE = CURRENT_DATE
),

-- Tabla con combinaciones activas actualmente (se le pegan las ventas historicas del PRIME al item_nbr actual)
-- Aqui *SI* se debe aplicar los filtros de validez, porque solo aplica a los actuales
TG AS (
  SELECT DISTINCT
    A.STORE_NBR,
    XREF.PRIME_XREF_ITEM_NBR,
    I.OLD_NBR,
    I.PRIMARY_DESC,
    ST.NEGOCIO
  FROM
    MX_CF_VM.INFOREM_MANAGED_SKU AS A
    INNER JOIN MX_CF_VM.ITEM_DESC AS I
      ON A.ITEM_NBR = I.ITEM_NBR
    INNER JOIN STORE AS ST
      ON A.STORE_NBR = ST.STORE_NBR
    INNER JOIN XREF
      ON A.ITEM_NBR = XREF.ITEM_NBR
  WHERE
		I.STATUS_CODE IN ('A')
		AND I.ORDBK_FLAG IN ('Y')
		AND I.CANCEL_WHEN_OUT_FLAG IN ('N')
		AND I.ITM_MBM_CODE IN ('M', 'I')
    AND A.CARRY_OPTION IN ('R')
    AND A.CARRIED_STATUS IN ('R')
),

-- Tabla de forecast (se agrupa por PRIME para que coincida con historico de ventas)
FCST AS (
  SELECT
		XREF.PRIME_XREF_ITEM_NBR,
    A.STORE_NBR,
		A.WM_YR_WK,
		SUM(A.SALES_FCST_EACH_QTY) AS WK_FCST_QTY
	FROM
		MX_CF_VM.STORE_ITEM_FCST_WK_CONV AS A
    INNER JOIN CURRWK ON A.FCST_WM_YR_WK = CURRWK.WM_YR_WK
    INNER JOIN XREF ON A.ITEM_NBR = XREF.ITEM_NBR
	GROUP BY 1, 2, 3
),
	
-- Tabla de ventas (se agrupa por PRIME por si cambiaron el item_nbr de un articulo)
POS AS (
  SELECT
		XREF.PRIME_XREF_ITEM_NBR,
		VTAS.STORE_NBR,
    VTAS.WM_YR_WK,
		SUM(CASE WHEN VTAS.WKLY_QTY < 0 THEN 0 ELSE VTAS.WKLY_QTY END) AS WKLY_QTY
	FROM
		MX_CF_VM.SKU_DLY_POS AS VTAS
    	INNER JOIN XREF
      	ON VTAS.ITEM_NBR = XREF.ITEM_NBR
    	INNER JOIN CURRWK
      	ON VTAS.WM_YR_WK < CURRWK.WM_YR_WK
	WHERE
		VTAS.REPORT_CODE NOT IN (8)
	GROUP BY 1, 2, 3
),

-- Numero de tiendas historicas por combinacion (se usa tabla de OH porque la de ventas no registra ventas = 0)
STORE_COUNTS AS (
  SELECT
    XREF.PRIME_XREF_ITEM_NBR,
    ST.NEGOCIO,
    OH.WM_YR_WK,
    COUNT(DISTINCT OH.STORE_NBR) AS N_STORES
  FROM  MX_CF_VM.REPL_SKU_WKLY_INV AS OH
    INNER JOIN XREF ON OH.ITEM_NBR = XREF.ITEM_NBR
    INNER JOIN STORE AS ST ON OH.STORE_NBR = ST.STORE_NBR
  GROUP BY 1,2,3
)

SELECT DISTINCT
  TG.OLD_NBR,
  TG.PRIMARY_DESC,
  TG.NEGOCIO,
  FCST.WM_YR_WK,
  'Forecast' AS "type",
  -- Como siempre hay fcst, podemos hacer esto aqui
  COUNT(DISTINCT FCST.STORE_NBR) AS N_STORES,
  SUM(FCST.WK_FCST_QTY) AS WKLY_QTY
FROM TG
  INNER JOIN FCST	ON (FCST.PRIME_XREF_ITEM_NBR = TG.PRIME_XREF_ITEM_NBR AND FCST.STORE_NBR = TG.STORE_NBR)
GROUP BY 1, 2, 3, 4, 5

UNION ALL

SELECT DISTINCT
  TG.OLD_NBR,
  TG.PRIMARY_DESC,
  TG.NEGOCIO,
  POS.WM_YR_WK,
  'Ventas' AS "type",
  -- Como la base de ventas no tiene registros para ventas cero, hay que contar las tiendas en OH
  SC.N_STORES,
  SUM(POS.WKLY_QTY) AS WKLY_QTY
FROM TG
  INNER JOIN POS ON (POS.PRIME_XREF_ITEM_NBR = TG.PRIME_XREF_ITEM_NBR AND POS.STORE_NBR = TG.STORE_NBR)
  INNER JOIN STORE_COUNTS SC
    ON (
      SC.PRIME_XREF_ITEM_NBR = TG.PRIME_XREF_ITEM_NBR
      AND SC.NEGOCIO = TG.NEGOCIO
      AND SC.WM_YR_WK = POS.WM_YR_WK
    )
GROUP BY 1, 2, 3, 4, 5, 6

ORDER BY 1, 2, 3
