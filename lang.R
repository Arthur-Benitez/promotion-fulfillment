require(purrr)

lang <- list(
  ## login
  user = 'Usuario',
  users = 'Usuarios',
  user_name = 'Usuario',
  tribu = 'Tribu',
  puesto = 'Puesto',
  all = 'Todos',
  password = 'Contraseña',
  role = 'Permisos',
  session = 'Sesión',
  platform = 'Plataforma',
  recover_password = 'Recuperar contraseña',
  login = 'Iniciar sesión',
  logout = 'Cerrar sesión',
  logout_timeout_info = 'Minutos hasta que se cierre la sesión automáticamente.',
  wrong_user_or_password = 'Usuario o contraseña incorrectos',
  old_password = 'Contraseña anterior',
  new_password_1 = 'Contraseña nueva',
  new_password_2 = 'Repetir contraseña nueva',
  button = 'Aplicar',
  passwords_must_match = 'La contraseña nueva debe ser igual en ambos recuadros',
  password_updated_successfully = 'Contraseña actualizada',
  wrong_current_password = 'Contraseña actual incorrecta',
  add = 'Crear',
  update_password = 'Actualizar contraseña',
  update_role = 'Actualizar permisos',
  delete = 'Eliminar',
  add_success = 'Usuario creado',
  update_password_success = 'Contraseña actualizada',
  update_role_success = 'Permisos actualizados',
  delete_success = 'Usuario eliminado',
  unknown_action = 'Acción desconocida',
  add_error = 'Error al crear usuario. Verifique que el usuario no exista y que tenga permisos menores a los de usted',
  update_password_error = 'Error al actualizar contraseña. Verifique que el usuario no exista y que tenga permisos menores a los de usted',
  update_role_error = 'Error al actualizar permisos. Verifique que el usuario exista y que sus nuevos permisos sean válidos e inferiores a los de usted',
  delete_error = 'Error al eliminar usuario. El usuario no existe o usted no tiene permiso para eliminarlo',
  manage_users = 'Administrar usuarios',
  action = 'Acción',
  selected_user = 'Usuarios disponibles',
  ## UI
  app_name = 'Promo Fulfillment',
  promo = 'Calculadora de Promociones',
  version_text = 'Versión de la aplicación',
  login = 'Login',
  logout = 'Logout',
  access_denied = 'Acceso denegado',
  auto_logout_title = 'La sesión se cerró por inactividad',
  password_update = 'Actualizar contraseña',
  management = 'Administración',
  button = 'Aplicar',
  run = 'Generar exhibiciones',
  reset = 'Reset',
  ok = 'Aceptar',
  cancel = 'Cancelar',
  error = 'Error',
  success = 'Éxito',
  warning = 'Advertencia',
  alert = 'Alertas',
  done = 'Listo',
  yes = 'Sí',
  no = 'No',
  hour = 'Horas',
  day = 'Días',
  week = 'Semanas',
  month = 'Meses',
  year = 'Años',
  help_title = 'Ayuda',
  user_level_icon = '¡Hola %s!',
  ## Compute Promotions
  compute_promotions_inputs = 'Inputs',
  compute_promotions_basic_intructions = 'Uso básico',
  compute_promotions_columns_glossary = 'Glosario de columnas',
  compute_promotions_inputs_title = 'Controles principales. Para más ayuda haz click en "(?) Instrucciones".',
  compute_promotions_computation_parameters = 'Exhibiciones',
  compute_promotions_shelves = 'Muebles',
  compute_promotions_computation_parameters_title = 'Parámetros que afectan el cálculo de las exhibiciones.',
  compute_promotions_impact_parameters = 'Impacto',
  compute_promotions_impact_parameters_title = 'Parámetros que afectan el cálculo de impacto.',
  compute_promotions_stores_lists = 'Tiendas Especiales',
  date_format = 'Formato de fechas en los archivos descargados',
  grafica_ventas = 'Combinación a visualizar',
  no_info = 'SIN INFORMACIÓN',
  sales_graph_selector_title = 'Mostrar gráfica informativa de ventas y forecast.',
  sales_graph_agg = 'Sumarización',
  sales_graph_agg_names = c('avg' = 'Promedio por tienda', 'sum' = 'Total'),
  sales_summary_groups = 'Nivel de la gráfica',
  graph_toggle = 'Gráfica de ventas/forecast',
  items = 'Items',
  browse = 'Archivo...',
  browse_empty = 'Seleccionar un archivo',
  download = 'Descargar',
  download_summary = 'Resumen',
  download_header = 'HEADER',
  download_detail = 'DETAIL',
  download_template = 'Descargar formato',
  show_instructions = 'Mostrar instrucciones',
  tab_input = 'Entradas',
  tab_output_histogram = 'Alcance',
  tab_output_summary = 'Resultados',
  tab_output_item_details = 'Artículos',
  tab_output_table = 'Cálculos',
  tab_input_graph = 'Tabla & Gráfica',
  tab_stores_lists = 'Tiendas especiales',
  feature = 'Promoción',
  good_features = 'Exhibiciones sin problemas',
  partial_features = 'Exhibiciones sin información',
  risky_features = 'Exhibiciones en riesgo',
  risky_combinations = 'Combinaciones en riesgo',
  failed_combinations = 'Combinaciones sin información',
  quantity_histogram = 'Alcance a exhibición',
  histogram_selection = 'Histograma a desplegar',
  dispersion_histogram = 'Dispersión de inventario',
  bin_number = 'Número de barras',
  dispersion_bin_selection = 'Distribución de barras',
  dispersion_fixed_bins = 'Barras estándar',
  dispersion_calculated_bins = 'Barras móviles',
  dispersion_histogram_stock_toggle = 'Inventario graficado',
  dispersion_histogram_stock_toggle_title = '¿Mostrar la dispersión de inventario sólo de la promoción o total (promoción + OH actual)?',
  summary_groups = 'Nivel de detalle',
  running_query = 'Ejecutando consulta...',
  running_computations = 'Realizando cálculos...',
  plotting = 'Cargando gráfica de ventas...',
  item_error = 'Esta combinación no tiene información :(',
  
  min_feature_qty_toggle = 'Lógica mínimo',
  min_feature_qty_toggle_title = '¿Qué hacer con las tiendas que no llegan a min_feature_qty con los días de venta?',
  min_feature_qty_toggle_names = c('No hacer nada', 'Conservadora (qty < min ==> qty := 0)', 'Agresiva (qty < min ==> qty := min)'),
  
  sspres_benchmark_toggle = 'SSPres Comparativo',
  sspres_benchmark_toggle_title = '¿Contra qué SSPres comparar las sugerencias?',
  sspres_benchmark_toggle_names = c(
    'Comparar vs SSPres futuro',
    'Comparar vs SSPres actual',
    'No comparar (SSPres = 0)'
  ),
  
  impact_toggle = 'Acción SSPres',
  impact_toggle_title = '¿Qué hacer con el SSPres cargado?',
  impact_toggle_names = c('Sumar', 'Sustituir'),
  
  ## Validation
  need_items_file = 'Cargar un archivo de items para comenzar.',
  need_auth = 'Iniciar sesión para continuar.',
  need_input_format = 'Descarga el formato de ejemplo para más información.',
  need_run = 'Click en *Correr* para empezar.',
  need_finish_running = 'Corriendo query...',
  need_query_result = 'El query falló :(',
  need_final_result = 'Los cálculos fallaron :(',
  need_select_feature = 'Selecciona una promoción para continuar.',
  need_alert_existence = 'No hay alertas para mostrar.',
  ## Variables
  dc = 'CEDIS',
  store_nbr = 'Tienda',
  cid = 'CID',
  old_nbr = 'Artículo',
  feature_name = 'Promoción',
  business = 'Negocio',
  departamento = 'Departamento',
  ## Usage Stats
  usage_stats = 'Utilización',
  refresh = 'Actualizar logs',
  update_user_info = 'Actualizar info de usuarios',
  max_days_to_load = 'Número de días a cargar',
  kpi = 'KPI',
  message = 'Evento',
  graph_daily_tab = 'Tendencia',
  graph_daily_x = 'Eje temporal (eje x)',
  graph_daily_split = 'Separar (color)',
  graph_top_tab = 'Top',
  graph_top_split = 'Separar (color)',
  graph_top_x = 'Entidad (eje x)',
  graph_top_nbar = 'Top N',
  n_users = 'Usuarios activos',
  p_active_users = 'Porcentaje de usuarios activos en la VP',
  n_sessions = 'Sesiones únicas',
  n_actions = 'Acciones',
  title_error = 'Error',
  graph_top_clearance = 'Nivel de permisos',
  graph_time_tab = 'Desempeño',
  graph_time_title = 'Tiempo de ejecución en segundos',
  graph_time_event = 'Evento',
  graph_time_event_names = c('Query', 'Gráfica de ventas'),
  detail_table_tab = 'Detalle',
  data = 'Datos',
  rrp_sync_info = 'Información de RRP y estatus de sincronización en GS1',
  db2_password = 'Contraseña de HOST / DB2',
  update_rrp = 'Actualizar info de RRP'
)

lang$need_valid_input <- paste0('El archivo de items no está en el formato correcto.\n', lang$need_input_format)
lang$no_stores_lists <- paste0('Tu archivo no tiene listas de tiendas especiales para incluir o excluir, por lo que las promociones se calcularán para todas las tiendas válidas de los formatos especificados.\n', lang$need_input_format)
lang$email_name <- 'Squad SCH Performance'
lang$email <- 'squad_sch_perf@email.wal-mart.com'
lang$email_subject <- 'Solicitud de activación de Promo Fulfillment'
lang$email_body <- 'Hola, %0A%0AMe gustaría activar mi cuenta en la aplicación Promo Fulfillment, mi usuario es:%0A%0A[Escribe aquí tu usuario de Windows.]'
lang$emailto <- paste0('mailto:', lang$email_name, '<', lang$email, '>', '?subject=', lang$email_subject, '&body=', lang$email_body)

## Necesario para que siempre se lean bien los encodings. Si no, de repente hay textos que no se interpretan bien...
lang <- map(lang, enc2native)
