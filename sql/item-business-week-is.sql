WITH
XREF AS (
  SELECT
    PRIME_XREF_ITEM_NBR,
    ITEM_NBR
    -- REPL_GROUP_NBR,
    -- ITEM1_DESC
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
)
SELECT
	INST.VP,
	INST.DIVISION,
	INST.DEP_NBR,
	INST.CATEGORY_NBR,
	CAT.CATEGORY_NAME,
	XREF.PRIME_XREF_ITEM_NBR,
	--INST.CID,
	--INST.ITEM_NBR,
	--INST.OLD_NBR,
	INST.FORMATO,
	INST.WM_YR_WK,
	SUM(INST.CATALOGO) AS CATALOGO,
	SUM(INST.FALT_OH) AS FALT_OH,
	SUM(INST.FALT_OH_IT) AS FALT_OH_IT,
	SUM(INST.FALT_OH_IT_IW) AS FALT_OH_IT_IW,
	SUM(INST.FALT_TTL) AS FALT_TTL,
	1 - (SUM(INST.FALT_OH) * 1.0000 / SUM(INST.CATALOGO) * 1.0000) AS IS_OH,
	1 - (SUM(INST.FALT_TTL) * 1.0000 / SUM(INST.CATALOGO) * 1.0000) AS IS_TTL	

FROM
	MR101_WM_AD_HOC.IS_ACUM_ITEM_VF AS INST
	INNER JOIN XREF
		ON XREF.ITEM_NBR = INST.ITEM_NBR
	LEFT JOIN WM_AD_HOC.CATEGORY_NAME AS CAT
		ON INST.DEP_NBR = CAT.DEPT_NBR AND INST.CATEGORY_NBR = CAT.CATEGORY_NBR

WHERE
	INST.FORMATO IN (?NEGOCIOS)
	AND INST.WM_YR_WK >= (?WK_INI_STUD)
	AND INST.WM_YR_WK <= (?WK_FIN_STUD)

GROUP BY
	1, 2, 3, 4, 5, 6, 7, 8
ORDER BY
	1, 2, 3, 4, 5, 6, 7, 8
