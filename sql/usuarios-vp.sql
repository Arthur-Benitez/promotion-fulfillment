SELECT DISTINCT
	WM_VP_NOM AS "vp",
	LOWER(WM_RESURTIDOR_ID) AS "user",
	WM_RESURTIDOR AS "name"
FROM wm_ad_hoc.tabla_resurtidor
ORDER BY 1,2,3
;