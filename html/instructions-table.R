instructions_table <- tribble(
  ~columna, ~descripcion, ~puede_cambiar_en_una_misma_promo,
  'feature_name',	'Nombre único de la promo (hasta 22 caracteres, sin espacios)', 'No',
  'user',	'Usuario de red que cargará las promos', 'No',
  'dept_nbr',	'Número de departamento (sin ceros a la izquierda)', 'No',
  'negocio', 'Formato de negocio (BAE, BODEGA, MIBODEGA, SUPERAMA o SUPERCENTER)', 'No',
  'old_nbr', 'Número de artículo', 'Sí',
  'max_feature_qty',	'Máximas piezas totales el el display (entre todos los artículos)', 'No',
  'max_ddv', 'Tope máximo de días de venta por artículo', 'Sí',
  'fcst_or_sales',	'"F" para usar forecast y "S" para usar ventas', 'No',
  'semana_ini',	'Semana Walmart inicial para calcular forecast o ventas (debe ser a pasado si fcst_or_sales = "S" y a futuro si es = "F")', 'No',
  'semana_fin',	'Semana Walmart final para calcular forecast o ventas (debe ser a pasado si fcst_or_sales = "S" y a futuro si es = "F")', 'No',
  'StartDate',	'Fecha de inicio de validez de la promo. Hay que especificar el formato de la fecha al subir el archivo.', 'No',
  'EndDate',	'Fecha de fin de validez de la promo. Hay que especificar el formato de la fecha al subir el archivo.', 'No'
)
names(instructions_table) <- c('Columna', 'Descripción', 'Puede cambiar en una misma promo?')