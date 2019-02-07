
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
  tab_input = 'Entradas',
  tab_output = 'Resultados',
  ## Validation
  need_items_file = 'Cargar un archivo de items para comenzar.',
  need_auth = 'Iniciar sesión para continuar.',
  need_input_format = 'Recuerda que debe ser un Excel (xlsx) con una sola hoja y XXXX',
  need_run = 'Click en *Correr* para empezar.',
  need_query_result = 'El query falló :(',
  need_final_result = 'Los cálculos fallaron :(',
  ## Misc
  running_query = 'Ejecutando consulta...',
  running_computations = 'Realizando cálculos...'
)

lang$need_valid_input <- paste0('El archivo de items no está en el formato correcto.\n', lang$need_input_format)