SELECT
	B.OLD_NBR,
	C.NEGOCIO,	
	A.STORE_NBR,
	SS_PRESS,
	SSPRESS,
	BASE_PRESS,
	SS_PRESS_TOT,
	SSCOV_PIEZAS AS SSCOV,
	SSTEMP_PIEZAS AS SSTEMP,
	SSCOV_TOT_PIEZAS AS SSCOV_TOT,
	MIN_SS,
	MAX_SS,
	GANADOR,
	SS_GANADOR

FROM
	WM_AD_HOC.SAFETY_TDA A,
	MX_CF_VM.ITEM B,
	(
	SELECT  DISTINCT
		T1.Store_nbr,
		T1.Trait_Nbr,
		CASE
			WHEN T1.Trait_Nbr = 297 THEN 'SUPERCENTER'
			WHEN T1.Trait_Nbr = 9 THEN 'BODEGA'
			WHEN T1.Trait_Nbr = 11 THEN 'SUPERAMA'
			WHEN T1.Trait_Nbr = 1312 THEN 'MIBODEGA'
			WHEN T1.Trait_Nbr IN (136,969) THEN 'BAE'
			ELSE 'OTRO'
		END AS NEGOCIO
	
	FROM
		MX_CF_VM.TRAIT_STORE T1
	
	WHERE
		T1.Trait_Nbr IN (11,297,1312,969,136,9)
	) AS C

WHERE
	A.ITEM_NBR = B.ITEM_NBR AND
	A.STORE_NBR = C.STORE_NBR AND
	B.OLD_NBR IN (?OLD_NBRS) AND
	C.NEGOCIO IN ('?NEGOCIOS')