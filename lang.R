
lang <- list(
  ## UI
  app_name = 'Promo Fulfillment',
  promo = 'Promo',
  login = 'Login',
  logout = 'Logout',
  user = 'Usuario',
  password = 'Contraseña',
  run = 'Correr',
  reset = 'Reset',
  items = 'Items',
  browse = 'Archivo...',
  browse_empty = 'Seleccionar un archivo',
  download = 'Descargar',
  download_template = 'Descargar formato',
  tab_input = 'Entradas',
  tab_output_histogram = 'Resultados (alcance)',
  tab_output_summary = 'Resultados (resumen)',
  tab_output_table = 'Resultados (tabla)',
  feature = 'Promoción',
  summary_groups = 'Nivel de detalle',
  ## Validation
  need_items_file = 'Cargar un archivo de items para comenzar.',
  need_auth = 'Iniciar sesión para continuar.',
  need_input_format = 'Descarga el formato de ejemplo para más información.',
  need_run = 'Click en *Correr* para empezar.',
  need_query_result = 'El query falló :(',
  need_final_result = 'Los cálculos fallaron :(',
  need_select_feature = 'Selecciona una promoción para continuar.',
  ## Misc
  running_query = 'Ejecutando consulta...',
  running_computations = 'Realizando cálculos...'
)

lang$need_valid_input <- paste0('El archivo de items no está en el formato correcto.\n', lang$need_input_format)