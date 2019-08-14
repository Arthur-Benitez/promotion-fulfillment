

# Funciones ---------------------------------------------------------------

## Generar espeficicación de columnas para readr
generate_cols_spec <- function(column_info, columns) {
  types <- remap_names(columns = columns, column_info = column_info, to_col = 'type')
  excel_types <- case_when(
    types %in% c('numeric', 'date', 'datetime') ~ types,
    types %in% c('character') ~ 'text',
    TRUE ~ 'text'
  )
  tibble(
    name = columns,
    type = types,
    excel_type = excel_types
  )
}

## Decide que alerta mostrar
alert_param <- function(good_features, empty_features, timestamp) {
  if (length(good_features) == 0){
    title1 <- lang$error
    text1 <- sprintf('No se encontró información para los parámetros especificados, favor de revisar que sean correctos. Exhibiciones que fallaron: %s', paste(empty_features, collapse  = ', '))
    type1 <- 'error'
    message1 <- 'DOWNLOAD FAILED'
  } else if (length(good_features) > 0 && length(empty_features) > 0){
    title1 <- lang$warning
    text1 <- sprintf('Se descargó la información de las exhibiciones: %s en %s, pero no se encontró información bajo los parámetros especificados para las siguientes exhibiciones: %s', paste(good_features, collapse  = ', '), format_difftime(difftime(Sys.time(), timestamp)), paste(empty_features, collapse  = ', '))
    type1 <- 'warning'
    message1 <- 'DOWNLOAD PARTIALLY FAILED'
  } else {
    title1 <- lang$success
    text1 <- sprintf('La información fue descargada de Teradata en %s.', format_difftime(difftime(Sys.time(), timestamp)))
    type1 <- 'success'
    message1 <- 'DOWNLOAD SUCCESSFUL'
  }
  return(list(title = title1, text = text1, type = type1, message = message1))
}

## Leer entrada
parse_input <- function(input_file, gl, calendar_day, date_format = '%Y-%m-%d') {
  if (tools::file_ext(input_file) != 'xlsx') {
    return('¡Cuidado! De ahora en adelante, el template de carga debe estar en formato de Excel (.xlsx). Te sugerimos que descarges el formato ejemplo de nuevo.')
  }
  tryCatch({
    column_info <- gl$cols[gl$cols$is_input, ]
    nms <- names(readxl::read_excel(input_file, sheet = 1, n_max = 0))
    if (!all(column_info$pretty_name %in% nms)) {
      return(sprintf('Las siguientes columnas faltan en el archivo de entrada: %s', paste(setdiff(column_info$pretty_name, nms), collapse = ', ')))
    }
    previous_nms <- nms
    nms <- remap_names(columns = nms, column_info, from_col = 'pretty_name', to_col = 'name') %>% 
      setNames(previous_nms)
    col_types <- generate_cols_spec(column_info, nms)
    x <- read_excel(
      path = input_file,
      sheet = 1,
      col_names = TRUE,
      col_types = col_types$excel_type
      ) %>% 
      plyr::rename(nms) %>%  
      .[column_info$name] %>% 
      mutate_at(
        col_types$name[col_types$type %in% c('date')],
        as.Date
      ) %>% 
      mutate(
        fcst_or_sales = toupper(fcst_or_sales),
        display_key = paste(dept_nbr, old_nbr, negocio, sep = '.'),
        split_var = paste(semana_ini, semana_fin, fcst_or_sales, sep = '-')
      )
    val <- validate_input(x, gl = gl, calendar_day = calendar_day)
    if (isTRUE(val)) {
      return(x)
    } else {
      return(val)
    }
  }, error = function(e){
    return('Error leyendo el archivo')
  })
}


## Validar inputs
validate_input <- function(data, gl, calendar_day) {
  column_info <- gl$cols[gl$cols$is_input, ]
  if (
    ## Condiciones básicas
    !is.data.frame(data) ||
    nrow(data) == 0 ||
    length(setdiff(column_info$name, names(data))) > 0
  ) {
    return(FALSE)
  } else {
    tryCatch({
      current_wk <- calendar_day %>% 
        filter(today() >= date) %>% 
        filter(date == max(date)) %>% 
        pull(wm_yr_wk)
      
      cond <- tribble(
        ~message, ~passed,
        ## Checar que no haya valores faltantes
        'No puede haber valores faltantes (blanks). Esto se debe comúnmente a que la fecha está almacenada como texto en Excel. Asegúrate de que Excel reconozca las fechas.',
        !anyNA(data),
        ## Checar que feature_name sea de longitid <= 22 caracteres (para que en total sean <= 40 para GRS)
        'feature_name no puede tener más de 22 caracteres',
        all(nchar(data$feature_name) <= 22),
        ## Checar que feature_name no tenga espacios
        'feature_name no puede tener espacios',
        all(!str_detect(data$feature_name, ' ')),
        ## Checar las columnas que deben ser constantes por feature
        sprintf('Las siguientes columnas no deben variar para el mismo feature_name: %s',
                paste(gl$feature_const_cols, collapse = ', ')),
        data %>% 
          group_by(feature_name) %>% 
          summarise_at(gl$feature_const_cols, list(~length(unique(.)))) %>% 
          ungroup() %>% 
          select_at(gl$feature_const_cols) %>% 
          equals(1) %>% 
          all(),
        ## Checar los negociosn
        sprintf('Los formatos de negocio deben ser uno de: %s', paste(gl$negocios, collapse = ', ')),
        all(data$negocio %in% gl$negocios),
        ## No se deben repetir artículos por feature
        'No se deben repetir artículos por feature_name',
        data %>% 
          group_by(feature_name) %>% 
          summarise(n_dups = sum(duplicated(old_nbr))) %>% 
          pull(n_dups) %>% 
          sum() %>% 
          equals(0),
        ## Checar que el tipo sea F ó S
        'fcst_or_sales debe ser F ó S',
        all(toupper(data$fcst_or_sales) %in% c('F', 'S')),
        ## Máximo de renglones
        sprintf('El archivo de entrada no puede tener más de %d renglones', gl$max_input_rows),
        nrow(data) <= gl$max_input_rows,
        ## Máximo de queries a correr
        sprintf('El archivo de entrada no puede tener más de %d combinaciones semana_ini-semana_fin-fcst_or_sales',
                gl$max_input_queries),
        length(unique(data$split_var)) <= gl$max_input_queries,
        ## Semana ini <= semana fin
        'semana_ini debe ser menor o igual a semana_fin',
        with(data, all(semana_ini <= semana_fin)),
        ## Checar que las semanas de forecast estén en el futuro
        sprintf('El rango de fechas de forecast debe estar en el futuro (semana_ini >= %d)', current_wk),
        with(data, all(fcst_or_sales == 'S') || all(semana_ini[fcst_or_sales == 'F'] >= current_wk)),
        ## Checar que las semanas de ventas estén en el pasado
        sprintf('El rango de fechas de ventas debe estar en el pasado (semana_fin < %d)', current_wk),
        with(data, all(fcst_or_sales == 'F') || all(semana_fin[fcst_or_sales == 'S'] < current_wk)),
        ## Checar que StartDate <= EndDate
        sprintf('Se debe cumplir que %s < StartDate <= EndDate', Sys.Date()),
        with(data, all(Sys.Date() <= StartDate & StartDate <= EndDate)),
        ## Checar que Priority sea un entero entre 1 y 100
        sprintf('Priority debe ser un entero entre 1 y 100'),
        with(data, all(Priority == as.integer(Priority) & between(Priority, 1, 100))),
        ## Checar que max_feature_qty sea estrictamente positivo
        'max_feature_qty debe ser un entero mayor o igual a 1.',
        with(data, all(max_feature_qty >= 1)),
        ## Checar que min_feature_qty esté entre 1 y max_feature_qty
        'min_feature_qty debe ser un entero entre 1 y max_feature_qty.',
        with(data, all(1 <= min_feature_qty & min_feature_qty <= max_feature_qty))
      )
      failed_idx <- which(!cond$passed)
      if (length(failed_idx) == 0) {
        return(TRUE)
      } else {
        return(cond$message[failed_idx[[1]]])
      }
      return(all(cond))
    }, error = function(e){
      return('Las pruebas fallaron')
    })
  }
}

## Función para guardar los archivos relevantes
save_files <- function(data_files, gl, credentials) {
  user_download_history <- file.path(gl$app_deployment_environment, 'download_history', credentials$user)
  if (!dir.exists(user_download_history)) {
    dir.create(user_download_history, recursive = TRUE)
  }
  saveRDS(data_files, file = file.path(user_download_history, sprintf('data_files_%s.rds', format(Sys.time(), "%Y-%m-%d_%H-%M-%S", tz = 'America/Mexico_City'))))
}

## Correr query
prepare_query <- function(query, keys, old_nbrs, wk_inicio, wk_final) {
  query %>% 
    str_replace_all('\\?KEY', paste0("'", paste(keys, collapse = "','"), "'")) %>%
    str_replace_all('\\?OLD_NBRS', paste(old_nbrs, collapse = ",")) %>%
    str_replace_all('\\?WK_INICIO', as.character(wk_inicio)) %>% 
    str_replace_all('\\?WK_FINAL', as.character(wk_final)) %>% 
    str_subset('^\\s*--', negate = TRUE) %>%  #quitar lineas de comentarios
    stringi::stri_trans_general('ASCII') %>% # quitar no ASCII porque truena en producción
    paste(collapse = '\n')
}
run_query_once <- function(ch, input_data, connector = 'production-connector') {
  wk_inicio <- unique(input_data$semana_ini)
  wk_final <- unique(input_data$semana_fin)
  type <- toupper(unique(input_data$fcst_or_sales))
  if (type == 'F') {
    query <- readLines('sql/exhibiciones-fcst.sql')
    value <- sym('avg_dly_fcst')
  } else if (type == 'S') {
    query <- readLines('sql/exhibiciones-pos.sql')
    value <- sym('avg_dly_pos')
  } else { ## No debería pasar si los datos están validados
    return(NULL)
  }
  query <- prepare_query(
    query = query,
    keys = input_data$display_key,
    old_nbrs = input_data$old_nbr,
    wk_inicio = wk_inicio,
    wk_final = wk_final
  )
  tryCatch({
    res <- sql_query(
      ch = ch,
      connector = connector,
      query = query,
      stringsAsFactors = FALSE
    )
    if (!is.data.frame(res)) {
      stop('Query failed.')
    }
    res <- res %>% 
      as_tibble() %>% 
      set_names(tolower(names(.))) %>% 
      mutate_if(is.factor, as.character) %>% 
      rename(avg_dly_pos_or_fcst = !!value)
    input_data %>% 
      left_join(res)
  }, error = function(e){
    NULL
  })
}
run_query <- function(ch, input_data, connector = 'production-connector') {
  res <- input_data %>% 
    split(., .$split_var) %>% 
    map(safely(function(x){
      run_query_once(ch, x, connector)
    })) %>% 
    map('result') %>% 
    keep(is.data.frame)
  if (length(res) > 0) {
    res <- bind_rows(res)
  } else {
    res <- NULL
  }
  res
}


## Query para descargar las ventas y forecast para la grafica
get_graph_data <- function(ch, input, calendar_day) {
  
  old_nbrs <- unique(input$old_nbr)
  negocios <- unique(input$negocio)
  query_graph <- readLines('sql/grafica.sql') %>% 
    str_replace_all('\\?OLD_NBRS', paste(old_nbrs, collapse = ",")) %>%
    str_replace_all('\\?NEGOCIO', paste0("'", paste(negocios, collapse = "','"), "'")) %>%
    str_subset('^\\s*--', negate = TRUE) %>%  #quitar lineas de comentarios
    stringi::stri_trans_general('ASCII') %>% # quitar no ASCII porque truena en producción
    paste(collapse = '\n')
  
  tryCatch({
    graph_table <- sql_query(
      ch = ch,
      connector = 'production-connector',
      query = query_graph,
      stringsAsFactors = FALSE
    )
    if (!is.data.frame(graph_table)) {
      stop("Graph query failed.")
    }
    graph_table <- graph_table %>% 
      set_names(tolower(names(.))) %>%
      as_tibble() %>%
      mutate_if(is.factor, as.character) %>%
      arrange(wm_yr_wk) %>% 
      left_join(calendar_day)
  }, error = function(e){
    1
  })
}

search_ss_once <- function(ch, input_data_ss, connector = 'production-connector') {
  old_nbrs <- paste(unique(input_data_ss$old_nbr), collapse = ",")
  negocios <- paste(unique(input_data_ss$negocio), collapse = "','")
  start_date <- as.character(unique(input_data_ss$StartDate))
  end_date <- as.character(unique(input_data_ss$EndDate))
  query_ss <- readLines('sql/ss-item-str.sql') %>%
    str_replace_all('\\?OLD_NBRS', old_nbrs) %>%
    str_replace_all('\\?NEGOCIOS', negocios) %>%
    str_replace_all('\\?START_DATE', start_date) %>% 
    str_replace_all('\\?END_DATE', end_date) %>% 
    str_subset('^\\s*--', negate = TRUE) %>%  #quitar lineas de comentarios
    stringi::stri_trans_general('ASCII') %>% # quitar no ASCII porque truena en producción
    paste(collapse = '\n')
  
  tryCatch({
    query_ss_res <- sql_query(
      ch = ch,
      connector = connector,
      query = query_ss,
      stringsAsFactors = FALSE
    )
    if (!is.data.frame(query_ss_res)) {
      stop("SS Query failed.")
    }
    query_ss_res <- query_ss_res %>% 
      as_tibble() %>% 
      set_names(tolower(names(.))) %>% 
      mutate_if(is.factor, as.character) %>% 
      left_join(
        input_data_ss %>% select(feature_name, old_nbr),
        by = 'old_nbr'
      )
  }, error = function(e){
    NULL
  })
}

## Query para buscar los SS actuales
search_ss <- function(ch, input_data_ss, connector = 'production-connector') {
  res <- input_data_ss %>% 
    mutate(split_var2 = paste(StartDate, EndDate, sep = '-')) %>%
    split(., .$split_var2) %>% 
    map(safely(function(x){
      search_ss_once(ch, x, connector)
    })) %>% 
    map('result') %>% 
    keep(is.data.frame)
  if (length(res) > 0) {
    res <- bind_rows(res)
  } else {
    res <- NULL
  }
  res
}

## Determinar nuevo SS ganador en cantidad
compare_ss_qty <- function(sspress_tot, sscov_tot, min_ss, max_ss) {
  pmin(pmax(sspress_tot, sscov_tot, min_ss), max_ss)
}

## Determinar nuevo SS ganador en nombre
compare_ss_name <- function(sspress_tot, sscov_tot, min_ss, max_ss, sspress, base_press, sscov, sstemp, win_qty) {
  win_ss = case_when(
        win_qty == max_ss ~ "MAX_SS",
        win_qty == min_ss ~ "MIN_SS",
       (win_qty == sspress_tot & sspress == 0) ~ "BASE_PRESS",
       (win_qty == sspress_tot & base_press == 0) ~ "SSPRESS",
        win_qty == sspress_tot ~ "SSPRESS_Tot",
       (win_qty == sscov_tot & sscov == 0) ~ "SSTEMP",
       (win_qty == sscov_tot & sstemp == 0) ~ "SSCOV",
        win_qty == sscov_tot ~ "SSCOV_Tot"
      )
  return(win_ss)
}

## Checar que el query haya regresado algo
check_query_result_is_empty <- function(result, input) {
  new_vars <- setdiff(names(result), names(input))
  all(is.na(result[new_vars]))
}

## Regresar nombre de displays que no tuvieron info
get_empty_features <- function(result, input) {
  result %>% 
    group_by(feature_name) %>% 
    nest() %>% 
    mutate(
      is_empty = map_lgl(data, ~check_query_result_is_empty(.x, input))
    ) %>% 
    select(feature_name, is_empty)
}

## Lógica en R
perform_computations <- function(data, data_ss = NULL, min_feature_qty_toggle = 'none', sspres_benchmark_toggle = 'none', impact_toggle = 'swap') {
  initial_columns <- names(data)
  ##Transformaciones de distribución
  data <- data %>% 
    group_by(feature_name, store_nbr) %>% 
    mutate(
      avg_dly_pos_or_fcst = ifelse(
        is.na(avg_dly_pos_or_fcst) | avg_dly_pos_or_fcst <= 0,
        0.01,
        avg_dly_pos_or_fcst
      ),
      feature_perc_pos_or_fcst = avg_dly_pos_or_fcst / sum(avg_dly_pos_or_fcst),
      ## Cantidades sin reglas
      feature_qty_req_min = feature_perc_pos_or_fcst * min_feature_qty,
      feature_qty_req = feature_perc_pos_or_fcst * max_feature_qty,
      feature_ddv_req = feature_qty_req / avg_dly_pos_or_fcst,
      ## Topar max DDV
      feature_ddv_pre = pmin(feature_ddv_req, max_ddv),
      feature_qty_pre = feature_ddv_pre * avg_dly_pos_or_fcst,
      feature_qty_pre_tot = sum(feature_qty_pre),
      ## Aplicar regla del mínimo
      feature_qty_fin = case_when(
        min_feature_qty_toggle == 'none' ~ feature_qty_pre,
        min_feature_qty_toggle == 'round_down' ~ ifelse(feature_qty_pre_tot < min_feature_qty,
                                                        0,
                                                        feature_qty_pre),
        min_feature_qty_toggle == 'round_up' ~ ifelse(feature_qty_pre_tot < min_feature_qty,
                                                      feature_qty_req_min,
                                                      feature_qty_pre)
      ),
      feature_ddv_fin = feature_qty_fin / avg_dly_pos_or_fcst,
      store_cost = cost * feature_qty_fin,
      vnpk_fin = feature_qty_fin / vnpk_qty
    ) %>% 
    ungroup() %>% 
    select(
      feature_name,
      store_nbr,
      dept_nbr,
      negocio,
      old_nbr,
      dc_nbr,
      dc_name,
      primary_desc,
      min_feature_qty,
      max_feature_qty,
      max_ddv,
      semana_ini,
      semana_fin,
      fcst_or_sales,
      avg_dly_pos_or_fcst,
      starts_with('feature_'),
      everything()
    ) %>% 
    arrange(feature_name, store_nbr, old_nbr)
  
  new_columns <- setdiff(names(data), initial_columns)
  data <- data %>%
    mutate_at(new_columns, list(~replace_na(., 0)))
  
  if (is.null(data_ss) || nrow(data_ss) == 0) {
    data <- data %>%
      mutate(
        sspress = 0,
        sspress_fut = 0,
        base_press = 0,
        sspress_tot = 0,
        sspress_fut_tot = 0,
        sscov = 0,
        sstemp = 0,
        sscov_tot = 0,
        min_ss = 0,
        max_ss = NA, # se sustituye después
        ganador = NA, # se sustituye después
        ss_ganador = 0
      )
  } else {
    data <- data %>%
      left_join(data_ss, by = c("feature_name", "store_nbr", "negocio", "old_nbr", "item_nbr"))
  }
  
  if (sspres_benchmark_toggle == 'none') {
    data$comp_sspress <- 0
  } else if (sspres_benchmark_toggle == 'current') {
    data$comp_sspress <- data$sspress
  } else if (sspres_benchmark_toggle == 'future') {
    data$comp_sspress <- data$sspress_fut
  }
  
  data <- data %>%
    replace_na(list(ganador = "Unknown", max_ss = 999999999)) %>%
    mutate_at(vars(contains("ss")), list(~round(replace_na(., 0), digits = 0))) %>%
    mutate(
      comp_sspress_tot = base_press + comp_sspress,
      comp_ss_winner_qty = compare_ss_qty(comp_sspress_tot, sscov_tot, min_ss, max_ss),
      comp_ss_winner_name = compare_ss_name(comp_sspress_tot, sscov_tot, min_ss, max_ss, comp_sspress, base_press, sscov, sstemp, comp_ss_winner_qty),
      
      new_sspress = case_when(
        impact_toggle == 'swap' ~ feature_qty_fin,
        impact_toggle == 'add' ~ feature_qty_fin + comp_sspress
      ),
      new_sspress_tot = new_sspress + base_press,
      ss_winner_qty = compare_ss_qty(new_sspress_tot, sscov_tot, min_ss, max_ss),
      ss_winner_name = compare_ss_name(new_sspress_tot, sscov_tot, min_ss, max_ss, new_sspress, base_press, sscov, sstemp, ss_winner_qty),
      
      impact_qty = ss_winner_qty - comp_ss_winner_qty,
      impact_cost = impact_qty * cost,
      impact_ddv = impact_qty / avg_dly_pos_or_fcst,
      impact_vnpk = impact_qty / vnpk_qty
    )
  return(data)
}

## Tabla de resumen
summarise_data <- function(data, group = c('feature_name', 'cid')) {
  ## Checks
  stopifnot(is.null(group) || all(group %in% c('feature_name', 'store_nbr', 'cid', 'dc_nbr')))
  ## Cambios a combinaciones específicas
  extra_groups <- list(
    'cid' = c('old_nbr', 'primary_desc'),
    'dc_nbr' = c('dc_name'),
    'store_nbr' = c('store_name')
  )
  group <- c(group, unlist(extra_groups[intersect(names(extra_groups), group)]))
  if (is.null(group)) {
    group <- 'feature_name'
    data <- data %>% 
      mutate(feature_name = 'Total')
  }
  ## Grupos de tabla de salida
  group_order <- c('feature_name', 'store_nbr', 'store_name', 'cid', 'old_nbr', 'primary_desc', 'dc_nbr', 'dc_name')
  grp <- group_order[group_order %in% group]
  ## Variables numéricas de tabla de salida
  vv <- c('avg_dly_sales', 'avg_dly_forecast', 'min_feature_qty', 'max_feature_qty', 'max_ddv', 'total_cost', 'total_impact_cost', 'total_qty', 'total_impact_qty', 'total_ddv', 'total_impact_ddv', 'total_vnpk', 'total_impact_vnpk')
  if ('store_nbr' %in% grp) {
    val_vars <- vv
  } else {
    val_vars <- c('n_stores', vv)
  }
  ## Sumarizar
  data_summary <- data  %>%
    group_by(!!!syms(grp)) %>%
    summarise(
      n_stores = n_distinct(store_nbr),
      ## Las ventas ya son promedio, así que sumándolas dan las ventas promedio de una entidad más grande
      avg_dly_sales = ifelse(any(fcst_or_sales == 'S'), sum(avg_dly_pos_or_fcst[fcst_or_sales=='S'], na.rm = TRUE), NA_real_),
      avg_dly_forecast = ifelse(any(fcst_or_sales == 'F'), sum(avg_dly_pos_or_fcst[fcst_or_sales=='F'], na.rm = TRUE), NA_real_),
      min_feature_qty = mean(min_feature_qty, na.rm = TRUE),
      max_feature_qty = mean(max_feature_qty, na.rm = TRUE),
      max_ddv = sum(max_ddv * avg_dly_pos_or_fcst, na.rm = TRUE) / sum(avg_dly_pos_or_fcst, na.rm = TRUE),
      total_cost = sum(store_cost, na.rm = TRUE),
      total_impact_cost = sum(impact_cost, na.rm = TRUE),
      total_qty = sum(feature_qty_fin, na.rm = TRUE),
      total_impact_qty = sum(impact_qty, na.rm = TRUE),
      total_ddv = sum(feature_qty_fin, na.rm = TRUE) / sum(avg_dly_pos_or_fcst, na.rm = TRUE),
      total_impact_ddv = sum(impact_qty, na.rm = TRUE) / sum(avg_dly_pos_or_fcst, na.rm = TRUE),
      total_vnpk = sum(vnpk_fin, na.rm = TRUE),
      total_impact_vnpk = sum(impact_vnpk, na.rm = TRUE)
    ) %>% 
    group_by(!!!syms(grp)) %>% # Agrupar para guardar la info de los grupos afuera y fijar las columnas correctamente
    arrange(!!!syms(grp)) %>% 
    select(!!!syms(grp), !!!syms(val_vars))
  
  return(data_summary)
}

## Tabla de histograma
generate_quantity_histogram_data <- function(output_filtered_data, bin_size = 0.2) {
  res <- output_filtered_data %>% 
    summarise_data(group = c('feature_name', 'store_nbr')) %>% 
    ungroup() %>% 
    mutate(
      perc_max_feature_qty = round(total_qty / max_feature_qty, 5)
    )
  
  max_bin <- bin_size * ceiling(max(res$perc_max_feature_qty) / bin_size)
  cut_values <- seq(0, max(max_bin, 0), by = bin_size)
  cut_labels <- paste(
    scales::percent(head(cut_values, -1), accuracy = 1),
    scales::percent(cut_values[-1], accuracy = 1),
    sep = ' - '
  )
  
  res %>% 
    mutate(
      perc_max_feature_qty_bin = cut(perc_max_feature_qty,
                                     breaks = cut_values,
                                     labels = cut_labels,
                                     include.lowest = TRUE),
      temp_cost = total_cost, # Creadas para evitar name clashes en el summarise
      temp_qty = total_qty,
      temp_avg_store_dly_pos_or_fcst = coalesce(avg_dly_sales, avg_dly_forecast)
    ) %>% 
    group_by(perc_max_feature_qty_bin) %>% 
    summarise(
      n_stores = n(),
      total_cost = sum(temp_cost),
      avg_store_cost = mean(temp_cost),
      total_qty = sum(temp_qty),
      avg_store_qty = mean(temp_qty),
      fcst_or_sales = ifelse(any(is.na(avg_dly_sales)), "F", "S"),
      avg_store_dly_pos_or_fcst = mean(temp_avg_store_dly_pos_or_fcst),
      min_feature_qty = mean(min_feature_qty),
      max_feature_qty = mean(max_feature_qty),
      total_ddv = sum(temp_qty) / sum(temp_avg_store_dly_pos_or_fcst),
      max_ddv = sum(max_ddv * temp_avg_store_dly_pos_or_fcst) / sum(temp_avg_store_dly_pos_or_fcst)
    ) %>% 
    ungroup() %>% 
    mutate(
      p_stores = n_stores / sum(n_stores)
    ) %>% 
    right_join(tibble(perc_max_feature_qty_bin = factor(cut_labels, levels = cut_labels)), by = 'perc_max_feature_qty_bin') %>% 
    replace(., is.na(.), 0) %>% 
    select(perc_max_feature_qty_bin, n_stores, p_stores, everything())
}

## Tabla de histograma
generate_dispersion_histogram_data <- function(output_filtered_data, bins = 5) {
  res <- output_filtered_data %>% 
    summarise_data(group = c('feature_name', 'store_nbr'))
  
  max_ddv <- mean(res$max_ddv)
  cut_values <- round(c(seq(0, max_ddv, length.out = bins - 1), 2 * max_ddv, Inf))
  cut_labels <- paste(
    head(cut_values, -1),
    cut_values[-1],
    sep = ' - '
  ) %>% 
    replace(list = length(.), sprintf('+%s', round(2 * max_ddv)))
  
  res %>% 
    mutate(
      total_ddv = round(total_ddv, 1),
      ddv_bin = cut(total_ddv,
                    breaks = cut_values,
                    labels = cut_labels,
                    include.lowest = TRUE),
      temp_cost = total_cost, # Creadas para evitar name clashes en el summarise
      temp_qty = total_qty
    ) %>% 
    group_by(ddv_bin) %>% 
    summarise(
      n_stores = n(),
      total_cost = sum(temp_cost),
      avg_store_cost = mean(temp_cost),
      total_qty = sum(temp_qty),
      avg_store_qty = mean(temp_qty),
      fcst_or_sales = ifelse(any(is.na(avg_dly_sales)), "F", "S"),
      avg_store_dly_pos_or_fcst = mean(coalesce(avg_dly_sales, avg_dly_forecast)),
      min_feature_qty = mean(min_feature_qty),
      max_feature_qty = mean(max_feature_qty)
    ) %>% 
    ungroup() %>% 
    mutate(
      p_stores = n_stores / sum(n_stores)
    ) %>% 
    right_join(tibble(ddv_bin = factor(cut_labels, levels = cut_labels)), by = 'ddv_bin') %>% 
    replace(., is.na(.), 0) %>% 
    select(ddv_bin, n_stores, p_stores, everything())
}

## Generar el nombre de la promo para GRS
generate_promo_name <- function(dept_nbr, user, feature_name) {
  sprintf('MX_D%d_PF_%s_%s', dept_nbr, toupper(user), feature_name)
}

## Generar el id de tienda en formato para GRS
generate_loc_id <- function(store_nbr) {
  sprintf('MX_WMT_ST_%s', str_pad(store_nbr, 5, 'left', '0'))
}

## Generar el HEADER.csv para cargar al sistema
generate_header <- function(input_data, impact_toggle = 'swap') {
  input_data %>% 
    transmute(
      `*Promotion` = generate_promo_name(dept_nbr, user, feature_name),
      Description = '',
      StartDate = StartDate,
      EndDate = EndDate,
      ApprovedSw = 'TRUE',
      AdditiveSw = case_when(
        impact_toggle == 'swap' ~ 'TRUE',
        impact_toggle == 'add' ~ 'TRUE'
      ),
      `CLEANSE HIST` = 'TRUE',
      `REPLACE PRES/DISPLAY` = 'FALSE',
      Priority,
      LiftType = 0,
      Cal = 'DMDWK',
      Lift = 0
    ) %>% 
    distinct() %>% 
    arrange(`*Promotion`)
}

## Generar el DETAIL.csv para cargar al sistema
generate_detail <- function(output_data) {
  output_data %>% 
    transmute(
      `*Promotion` = generate_promo_name(dept_nbr, user, feature_name),
      `*StartDate` = StartDate,
      `*CID DMDUNIT NBR` = cid,
      `*DMDGroup` = '-',
      `*Loc` = generate_loc_id(store_nbr),
      `PRESENTATION PCT` = 0,
      `PRESENTATION QTY ` = round(feature_qty_fin),
      `OFFSET START DAYS` = 0,
      `OFFSET END DAYS` = 0,
      `SPMTL ORDER QTY` = 0,
      `DISPLAY QTY` = 0,
    )
}

## Generar input de ejemplo que siempre funcione
generate_sample_input <- function(calendar_day, column_info) {
  fcst_wks <- calendar_day %>% 
    filter(date >= Sys.Date() + 0 & date <= Sys.Date() + 28) %>% 
    pull(wm_yr_wk) %>%
    range()
  sales_wks <- calendar_day %>% 
    filter(date >= Sys.Date() - 90 & date <= Sys.Date() - 60) %>% 
    pull(wm_yr_wk) %>%
    range()
  info <- tibble(
    feature_name = c('MARUCHAN', 'MARUCHAN', 'MARUCHAN', 'MARUCHAN', 'MARUCHAN_MINI', 'MARUCHAN_MINI'),
    user = 'm1234xy',
    dept_nbr = 95,
    negocio = 'BAE',
    old_nbr = c(9506783, 9506804, 9574857, 9506748, 9574857, 9506748),
    primary_desc_temp = c('MARUCHAN CAMARON', 'MARUCHAN CMRONCHILE', 'MARUCHAN CMRONHBNER', 'MARUCHAN POLLO', 'MARUCHAN CMRONHBNER', 'MARUCHAN POLLO'),
    min_feature_qty = c(600, 600, 600, 600, 300, 300),
    max_feature_qty = c(3000, 3000, 3000, 3000, 2000, 2000),
    max_ddv = 30,
    fcst_or_sales = c('S', 'S', 'S', 'S', 'F', 'F'),
    semana_ini = c(rep(sales_wks[1], 4), rep(fcst_wks[1], 2)),
    semana_fin = c(rep(sales_wks[2], 4), rep(fcst_wks[2], 2)),
    StartDate = c(rep(Sys.Date() + 7, 4), rep(Sys.Date() + 14, 2)),
    EndDate = c(rep(Sys.Date() + 35, 4), rep(Sys.Date() + 49, 2)),
    Priority = 12
  )
  names(info) <- remap_names(names(info), column_info, to_col = 'pretty_name')
  return(info)
}

# Server ------------------------------------------------------------------

computePromotionsServer <- function(input, output, session, credentials) {
  
  ## Calendario para validar inputs
  calendar_day <- read_tsv('data/calendar-day.tsv', col_types = 'nD')
  
  ## Valores reactivos para usar en observadores
  r <- reactiveValues(
    ch = NULL,
    is_open = FALSE,
    auth_trigger = 0,
    items_file = NULL,
    items = NULL,
    is_running = FALSE,
    query_was_tried = FALSE,
    reset_trigger = 0,
    final_result_trigger = 0
  )
  
  ## UI
  output$items_ui <- renderUI({
    input$reset
    ns <- session$ns
    fileInput(ns('items'), label = lang$items, buttonLabel = lang$browse, placeholder = lang$browse_empty)
  })
  
  output$auth_ui <- renderUI({
    ns <- session$ns
    actionButton(ns('auth'), label = lang$login, icon = icon('sign-out-alt'),
                 style="color: #fff; background-color: #3BC136;")
  })
  
  ## Login a Teradata
  observeEvent(input$auth, {
    r$is_open <- tryCatch({
      odbcGetInfo(r$ch)
      TRUE
    }, error = function(e){FALSE})
    r$auth_trigger <- r$auth_trigger + 1
  })
  observeEvent(r$auth_trigger, {
    ns <- session$ns
    if (is.null(r$ch) || !r$is_open) {
      shinyalert(
        type = 'info',
        title = 'Iniciando sesión...',
        closeOnEsc = FALSE,
        closeOnClickOutside = FALSE,
        showCancelButton = FALSE,
        showConfirmButton = FALSE
      )
      tryCatch({
        flog.info(toJSON(list(
          session_info = msg_cred(credentials()),
          message = 'ATTEMPTING TO LOG IN INTO TERADATA',
          details = list(
            user = input$user
          )
        )))
        r$ch <- odbcDriverConnect(sprintf("Driver={Teradata};DBCName=WM3;AUTHENTICATION=ldap;AUTHENTICATIONPARAMETER=%s", paste0(input$user, '@@', input$password)))
        odbcGetInfo(r$ch) ## Truena si no se abrió la conexión
        r$is_open <- TRUE
        output$auth_ui <- renderUI({
          actionButton(ns('auth'), label = lang$logout, icon = icon('sign-out-alt'),
                       style="color: #fff; background-color: #f42e2e;")
        })
        flog.info(toJSON(list(
          session_info = msg_cred(credentials()),
          message = 'TERADATA LOGIN SUCCESSFUL',
          details = list(
            user = input$user
          )
        )))
        shinyalert::closeAlert()
      }, error = function(e){
        shinyalert(
          type = "error",
          title = lang$error,
          text = "El usuario o la contraseña no son válidos",
          closeOnClickOutside = TRUE
        )
        flog.warn(toJSON(list(
          session_info = msg_cred(credentials()),
          message = 'TERADATA LOGIN FAILED',
          details = list(
            user = input$user
          )
        )))
      })
    } else {
      odbcClose(r$ch)
      r$is_open <- FALSE
      
      updateActionButton(session, 'auth', label = lang$login, icon = icon('sign-in-alt'))
      updateTextInput(session, 'user', value = '')
      updateTextInput(session, 'password', value = '')
      output$auth_ui <- renderUI({
        actionButton(ns('auth'), label = lang$login, icon = icon('sign-out-alt'),
                     style="color: #fff; background-color: #3BC136;")
      })
      flog.info(toJSON(list(
        session_info = msg_cred(credentials()),
        message = 'TERADATA LOGOUT SUCCESSFUL',
        details = list(
          user = input$user
        )
      )))
      r$reset_trigger <- r$reset_trigger + 1
    }
  }, ignoreInit = TRUE)
  
  ## Leer input
  observeEvent(input$items, {
    ## Resetear primero
    r$items_file <- NULL
    r$items <- NULL
    graph_table(NULL)
    query_result(NULL)
    r$query_was_tried <- NULL
    ## Luego actualizar items
    r$items_file <- input$items$datapath
  })
  observe({
    req(r$items_file, input$date_format)
    flog.info(toJSON(list(
      session_info = msg_cred(credentials()),
      message = 'PARSING INPUT FILE',
      details = list(
        file = r$items_file
      )
    )))
    val <- parse_input(r$items_file, gl = gl, calendar_day = calendar_day, date_format = input$date_format)
    if (!is.data.frame(val)) {
      shinyalert(
        type = "error", 
        title = lang$error,
        text = val,
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE
      )
      r$items <- NULL
      flog.error(toJSON(list(
        session_info = msg_cred(credentials()),
        message = 'PARSING INPUT FILE FAILED',
        details = list(
          file = r$items_file,
          reason = val
        )
      )))
    } else {
      r$items <- val
      flog.info(toJSON(list(
        session_info = msg_cred(credentials()),
        message = 'DONE PARSING INPUT FILE',
        details = list(
          file = r$items_file
        )
      )))
    }
  })
  
  graph_table <- reactiveVal(NULL)
  sales_graph_flag <- reactiveVal(FALSE)
  sales_graph_trigger <- reactiveVal(0)
  items_changed_toggle <- reactiveVal(FALSE)
  # graph_toggle <- debounce(reactive(input$graph_toggle), millis = 2000)
  observeEvent(r$items, {
    ## Esto sirve para detectar si lo que cambió fue r$items o el botón de
    ## mostrar gráfica. Si sólo cambió el botón, no hace falta correr el query
    ## de nuevo
    items_changed_toggle(TRUE)
  })
  observe({
    req(isTRUE(items_changed_toggle()))
    if (input$graph_toggle) {
      if (isolate(sales_graph_flag()) == FALSE) {
        sales_graph_flag(TRUE)
        invalidateLater(500)
      } else {
        sales_graph_flag(FALSE)
        items_changed_toggle(FALSE)
        isolate(r$is_running <- TRUE)
        # Correr query para descargar info para gráfica y asignar a variable
        isolate(sales_graph_trigger(sales_graph_trigger() + 1))
      }
    }
  })
  
  observeEvent(sales_graph_trigger(), {
    req(sales_graph_trigger() > 0)
    flog.info(toJSON(list(
      session_info = msg_cred(credentials()),
      message = 'RUNNING SALES GRAPH QUERY',
      details = list()
    )))
    is_dev <- !is.null(r$ch)
    items <- r$items
    usr <- input$user
    pwd <- input$password
    future({
      # init_log(log_dir)
      if (is_dev) {
        future_ch <- RODBC::odbcDriverConnect(sprintf("Driver={Teradata};DBCName=WM3;AUTHENTICATION=ldap;AUTHENTICATIONPARAMETER=%s", paste0(usr, '@@', pwd)))
      } else {
        future_ch <- NULL
      }
      get_graph_data(ch = future_ch, input = items, calendar_day = calendar_day)
    }) %...>% 
      graph_table()
  })
  
  output$input_table <- renderDT({
    shiny::validate(
        shiny::need(!is.null(r$items_file), lang$need_items_file) %then%
        shiny::need(!is.null(r$items), lang$need_valid_input)
    )
    r$items[intersect(names(r$items), gl$cols$name[gl$cols$is_input])] %>%
      generate_basic_datatable(gl$cols, scrollX = TRUE, scrollY = ifelse(input$graph_toggle, '150px', '500px'))
  })
  
  ## Apagar bandera r$is_running
  observeEvent(graph_table(), {
    r$is_running <- FALSE
  })
  output$input_grafica_ventas <- renderUI({
    req(r$items)
    req(is.data.frame(graph_table()))
    req(isTRUE(input$graph_toggle))
    ns <- session$ns
    info <- graph_table() %>%
      distinct_at(c('old_nbr', 'negocio', 'primary_desc'))
    choices <- r$items %>% 
      left_join(info, by = c('old_nbr', 'negocio')) %>% 
      transmute(
        name = paste0(feature_name, ' - ',negocio, ' - ', ifelse(is.na(primary_desc), lang$no_info, primary_desc), ' (', old_nbr, ')'),
        combinacion = paste(feature_name, '::', old_nbr, '-', negocio)
      ) %>%
      distinct() %>% 
      deframe()
    tags$div(
      class = 'inline-inputs',
      tags$div(
        style = 'margin-right: 20px',
        selectInput(ns('input_grafica_ventas'), lang$grafica_ventas, choices = choices, width = '500px')
      ),
      selectInput(
        ns('agg_grafica_ventas'),
        lang$agg_grafica_ventas,
        choices = c('avg', 'sum') %>% set_names(lang$agg_grafica_ventas_names)
      )
    )
  })
  
  ## Grafica reactiva
  output$grafica_ventas <- renderPlotly({
    if (identical(graph_table(), 1)) {
      flog.warn(toJSON(list(
        session_info = msg_cred(credentials()),
        message = 'SALES GRAPH QUERY FAILED',
        details = list()
      )))
    }
    
    shiny::validate(
      shiny::need(r$is_open || gl$app_deployment_environment == 'prod', '') %then%
        shiny::need(!is.null(r$items) && isTRUE(input$graph_toggle), '') %then%
        shiny::need(!is.null(graph_table()), lang$plotting) %then%
        shiny::need(is.data.frame(graph_table()), lang$need_query_result) %then%
        shiny::need(input$input_grafica_ventas, '')
    )
    flog.info(toJSON(list(
      session_info = msg_cred(credentials()),
      message = 'GENERATING SALES GRAPH',
      details = list()
    )))
    
    df <- graph_table() %>% 
      filter(paste(old_nbr, '-', negocio) == str_replace(input$input_grafica_ventas, ".+ :: ", '')) %>% 
      na.omit()
    if (nrow(df) == 0) {
      plot_ly() %>%
        add_text(x = 0, y = 0, text = lang$item_error, textfont = list(size = 40)) %>%
        layout(
          title = list(
            text = lang$error,
            font = list(size = 30)
          ),
          xaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
          ),
          yaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
          ),
          margin = list(t = 60)
        )
    } else {
      forecast <- df %>% filter(type == "Forecast")
      ventas <- df %>% filter(type == "Ventas")
      df <- bind_rows(ventas,
                      forecast %>% head(1) %>% mutate(type = "Ventas"),
                      forecast) %>% 
        mutate(
          wkly_qty = case_when(
            input$agg_grafica_ventas == 'avg' ~ wkly_qty / n_stores,
            input$agg_grafica_ventas == 'sum' ~ wkly_qty,
            TRUE ~ wkly_qty / n_stores
          ),
          dly_qty = wkly_qty / 7
        ) %>% 
        arrange(wm_yr_wk)
      
      # Lineas verticales de la gráfica
      lines <- df$date %>% 
        year() %>% 
        unique() %>% 
        sort() %>% 
        .[-1] %>% 
        paste0("-01-01") %>% 
        ymd() %>% 
        map(function(fc){ # Lineas verticales para los cambios de año
          list(
            x0 = fc,
            x1 = fc,
            y0 = 0,
            y1 = 1.1 * max(df$wkly_qty),
            line = list(color = "black", dash = "dash"),
            type = "line"
          )
        }) %>% 
        c(list(list( # Linea vertical para el presente
          x0 = max(calendar_day$date[calendar_day$date <= Sys.Date()]),
          x1 = max(calendar_day$date[calendar_day$date <= Sys.Date()]),
          y0 = 0,
          y1 = 1.1 * max(df$wkly_qty),
          line = list(color = "black"),
          type = "line"
        )))
      # La gráfica
      plot_ly(
        data = df, 
        x = ~date,
        hoverinfo = 'text'
      ) %>%
        add_lines(
          y = ~wkly_qty,
          color = ~type,
          colors = (c('blue', 'orange') %>% setNames(c('Ventas', 'Forecast'))),
          text = ~sprintf(
            "Fecha: %s<br>Semana WM: %s<br>%s %s: %s<br>%s %s: %s",
            date,
            wm_yr_wk,
            type,
            ifelse(type == 'Ventas', 'semanales', 'semanal'),
            scales::comma(wkly_qty, accuracy = 1),
            type,
            ifelse(type == 'Ventas', 'diarias', 'diario'),
            scales::comma(dly_qty, accuracy = 0.1)
          )
        ) %>% 
        add_lines(
          y = ~sell_price,
          name = 'Precio',
          line = list(
            color = 'green',
            dash = 'dash'
          ),
          yaxis = 'y2',
          text = ~sprintf(
            "Precio: $%s",
            scales::comma(sell_price, accuracy = 0.01)
          )
        ) %>% 
        layout(
          title = list(
            text = sprintf("Ventas semanales en piezas (%s)", lang$agg_grafica_ventas_names[input$agg_grafica_ventas])
            #x = 0.07
          ),
          xaxis = list(
            title = '',
            type = 'date',
            tickformat = "%d %b %y",
            ticks = 'outside'
          ),
          yaxis = list(
            title = 'Piezas',
            exponentformat = "none"
          ),
          yaxis2 = list(
            title = 'Pesos',
            tickformat = '$,.2f',
            showgrid = FALSE,
            overlaying = 'y',
            #nticks = 6,
            side = 'right'
          ),
          legend = list(
            x = 0,
            y = 1.05,
            orientation = 'h'
          ),
          margin = list(r = -2),
          hovermode = 'compare',
          shapes = lines
        )
    }
  })
  
  output$grafica_ventas_completa <- renderUI({
    req(isTRUE(input$graph_toggle))
    ns <- session$ns
    tagList(
      uiOutput(ns('input_grafica_ventas')),
      plotlyOutput(ns('grafica_ventas'), height = gl$plotly_height) %>% withSpinner(type = 8)
    )
  })
  
  ## Seleccionar pestaña de output para que se vea el loader
  rr <- reactiveVal(0)
  tik <- reactiveVal(NULL)
  observeEvent(input$run, {
    req(r$is_open || gl$app_deployment_environment == 'prod')
    if (is.null(tik()) || as.numeric(difftime(Sys.time(), tik(), units = 'secs')) >= 30) {
      tik(Sys.time())
      ns <- session$ns
      if (is.null(r$items)) {
        r$query_was_tried <- FALSE
      } else {
        updateTabItems(session, 'io', selected = 'output_summary')
        output$output_table <- renderDT(NULL)
        r$query_was_tried <- TRUE
        r$is_running <- TRUE
        rr(rr() + 1)
      }
    }
  })
  
  ## Correr query
  query_result <- reactiveVal()
  observeEvent(rr(), {
    flog.info(toJSON(list(
      session_info = msg_cred(credentials()),
      message = 'RUNNING QUERY',
      details = list()
    )))
    time1 <- Sys.time()
    shinyalert(
      type = 'info',
      title = 'Calculando...',
      text = sprintf('Hora de inicio: %s', format(time1, "%X", tz = 'America/Mexico_City')),
      closeOnEsc = FALSE,
      showCancelButton = FALSE,
      showConfirmButton = FALSE
    )
    ## Hay que leer los valores reactivos AFUERA de future()
    ## Ver: https://cran.r-project.org/web/packages/future/vignettes/future-4-issues.html
    is_dev <- !is.null(r$ch)
    items <- r$items
    usr <- input$user
    pwd <- input$password
    future({
      # init_log(log_dir)
      if (is_dev) {
        ## Las conexiones no se pueden exportar a otros procesos de R, así que se tiene que generar una nueva conexión
        ## Ver: https://cran.r-project.org/web/packages/future/vignettes/future-4-issues.html
        future_ch <- RODBC::odbcDriverConnect(sprintf("Driver={Teradata};DBCName=WM3;AUTHENTICATION=ldap;AUTHENTICATIONPARAMETER=%s", paste0(usr, '@@', pwd)))
      } else {
        future_ch <- NULL
      }
      list(
        timestamp = time1,
        data = run_query(future_ch, items, 'production-connector'),
        data_ss = search_ss(future_ch, items, 'WM3')
      )
    }) %...>% 
      query_result()
  }, ignoreInit = TRUE)
  
  ## Hacer cálculos
  ### Mostrar alertas y checar info
  good_features_rv <- reactiveVal()
  observeEvent(query_result(), {
    req(query_result()$data)
    r$is_running <- FALSE
    feature_info <- get_empty_features(query_result()$data, isolate(r$items))
    good_features <- with(feature_info, feature_name[!is_empty])
    empty_features <- with(feature_info, feature_name[is_empty])
    alert_info <- alert_param(good_features, empty_features, query_result()$timestamp)
    shinyalert::shinyalert(
      type = alert_info$type,
      title = alert_info$title,
      text = alert_info$text,
      closeOnClickOutside = TRUE,
      timer = 10000
    )
    flog.info(toJSON(list(
      session_info = msg_cred(isolate(credentials())),
      message = alert_info$message,
      details = list()
    )))
    good_features_rv(good_features)
    r$final_result_trigger <- r$final_result_trigger + 1
  })
  ### Ahora sí cálculos
  final_result <- eventReactive({
    input$min_feature_qty_toggle
    input$sspres_benchmark_toggle
    input$impact_toggle
    r$final_result_trigger
  }, {
    req(r$final_result_trigger > 0)
    req(query_result()$data)
    if (length(good_features_rv()) > 0) {
      flog.info(toJSON(list(
        session_info = msg_cred(isolate(credentials())),
        message = 'PERFORMING COMPUTATIONS',
        details = list()
      )))
      good_data <- query_result()$data %>% 
        filter(feature_name %in% good_features_rv())
      purrr::safely(perform_computations)(
        data = good_data,
        data_ss = query_result()$data_ss,
        min_feature_qty_toggle = input$min_feature_qty_toggle,
        sspres_benchmark_toggle = input$sspres_benchmark_toggle,
        impact_toggle = input$impact_toggle
      )$result
    } else {
      NULL
    }
  })
  
  ## Validaciones
  need_input_ready <- reactive({
    shiny::need(r$is_open || gl$app_deployment_environment == 'prod', lang$need_auth) %then%
      shiny::need(!is.null(r$items_file), lang$need_items_file) %then%
      shiny::need(!is.null(r$items), lang$need_valid_input)
  })
  need_query_ready <- reactive({
    shiny::need(r$query_was_tried, lang$need_run) %then%
      shiny::need(is.data.frame(query_result()$data), lang$need_query_result) %then%
      shiny::need(is.data.frame(final_result()), lang$need_final_result)
  })
  need_histogram_ready <- reactive({
    shiny::need(!is.null(quantity_histogram_data()) || !is.null(dispersion_histogram_data()), lang$need_final_result) %then%
      shiny::need(nchar(input$output_feature_select) > 0, lang$need_select_feature)
  })
  
  ## Tabla de salida
  output$detail_table <- renderDT({
    shiny::validate(
      need_input_ready() %then%
        need_query_ready()
    )
    tryCatch({
      percent_columns <- c('feature_perc_pos_or_fcst')
      final_result() %>%
        select(
          feature_name,
          old_nbr,
          primary_desc,
          store_nbr,
          negocio,
          everything()
        ) %>% 
        transform_columns(gl$cols) %>% 
        datatable(
          extensions = c('FixedColumns', 'KeyTable'),
          filter = 'top',
          options = list(
            fixedColumns = list(leftColumns = 5),
            keys = TRUE,
            scrollX = TRUE,
            scrollY = '500px',
            pageLength = 100
          ),
          colnames = remap_names(names(.), gl$cols, to_col = 'pretty_name'),
          callback = build_callback(remap_names(names(.), gl$cols, to_col = 'description'))
        ) %>%
        format_columns(gl$cols)
    }, error = function(e){
      NULL
    })
  })
  
  summary_table <- reactive({
    req(final_result())
    purrr::safely(summarise_data)(final_result(), input$summary_groups)$result
  })
  
  output$summary_table <- renderDT({
    shiny::validate(
      need_input_ready() %then%
        need_query_ready()
    )
    tryCatch({
      summary_table() %>% 
        transform_columns(gl$cols) %>% 
        datatable(
          extensions = c('FixedColumns', 'KeyTable'),
          filter = 'top',
          options = list(
            fixedColumns = list(leftColumns = length(group_vars(.)) + 1),
            keys = TRUE,
            scrollX = TRUE,
            scrollY = '500px',
            pageLength = 100
          ),
          colnames = remap_names(names(.), gl$cols, to_col = 'pretty_name'),
          callback = build_callback(remap_names(names(.), gl$cols, to_col = 'description'))
        ) %>%
        format_columns(gl$cols)
    }, error = function(e){
      NULL
    })
  })
  
  output$output_feature_select_ui <- renderUI({
    req(final_result())
    ns <- session$ns
    choices <- final_result() %>% 
      pull(feature_name) %>%
      unique() %>% 
      sort()
    selectInput(
      inputId = ns('output_feature_select'),
      label = lang$feature,
      choices = choices
    )
  })
  
  ## Tabla de alcance
  final_results_filt <- reactive({
    req(final_result())
    req(input$output_feature_select)
    final_result() %>% 
      filter(feature_name == input$output_feature_select)
  })
  quantity_histogram_data <- reactive({
    req(final_results_filt())
    generate_quantity_histogram_data(final_results_filt(), bin_size = input$quantity_histogram_bin_size)
  })
  dispersion_histogram_data <- reactive({
    req(final_results_filt())
    generate_dispersion_histogram_data(final_results_filt(), bins = input$dispersion_histogram_bin_size)
  })
  
  ## Histograma de alcance
  quantity_histogram <- reactive({
    shiny::validate(
      need_input_ready() %then%
        need_query_ready() %then%
        need_histogram_ready()
    )
    tryCatch({
      mfq <- unique(final_results_filt()$max_feature_qty)
      quantity_histogram_data() %>% 
        mutate(
          label_y = n_stores + 0.03 * max(n_stores),
          label = scales::percent(p_stores),
          text = sprintf('Tiendas: %s (%s)<br>Costo total: %s<br>Costo promedio: %s<br>Cant. total: %s<br>Cant. promedio: %s<br>%s promedio: %s', scales::comma(n_stores, accuracy = 1), scales::percent(p_stores), scales::comma(total_cost, accuracy = 1), scales::comma(avg_store_cost, accuracy = 1), scales::comma(total_qty, accuracy = 1), scales::comma(avg_store_qty, accuracy = 1), ifelse(first(fcst_or_sales) == 'F', 'Forecast', 'Venta'), scales::comma(avg_store_dly_pos_or_fcst, accuracy = 1))
        ) %>% 
        plot_ly(x = ~perc_max_feature_qty_bin, y = ~n_stores, text = ~text, hoverinfo = 'text', type = 'bar', name = NULL) %>% 
        add_text(y = ~label_y, text = ~label, name = NULL) %>% 
        plotly::layout(
          title = 'Alcance a piezas máximas por tienda',
          xaxis = list(title = sprintf('Alcance (%% de Max. Feature Qty. = %s)', scales::comma(mfq))),
          yaxis = list(title = 'Número de tiendas', separators = '.,'),
          showlegend = FALSE
        )
    }, error = function(e){
      NULL
    })
  })
  
  dispersion_histogram <- reactive({
    shiny::validate(
      need_input_ready() %then%
        need_query_ready() %then%
        need_histogram_ready()
    )
    tryCatch({
      mfq <- unique(final_results_filt()$max_feature_qty)
      dispersion_histogram_data() %>% 
        mutate(
          label_y = n_stores + 0.03 * max(n_stores),
          label = scales::percent(p_stores),
          text = sprintf('Tiendas: %s (%s)<br>Costo total: %s<br>Costo promedio: %s<br>Cant. total: %s<br>Cant. promedio: %s<br>%s promedio: %s', scales::comma(n_stores, accuracy = 1), scales::percent(p_stores), scales::comma(total_cost, accuracy = 1), scales::comma(avg_store_cost, accuracy = 1), scales::comma(total_qty, accuracy = 1), scales::comma(avg_store_qty, accuracy = 1), ifelse(first(fcst_or_sales) == 'F', 'Forecast', 'Venta'), scales::comma(avg_store_dly_pos_or_fcst, accuracy = 1))
        ) %>% 
        plot_ly(x = ~ddv_bin, y = ~n_stores, text = ~text, hoverinfo = 'text', type = 'bar', name = NULL) %>% 
        add_text(y = ~label_y, text = ~label, name = NULL) %>% 
        plotly::layout(
          title = 'Dispersión de Inventario',
          xaxis = list(title = 'Rango de días de venta por tienda (DDV)'),
          yaxis = list(title = 'Número de tiendas', separators = '.,'),
          showlegend = FALSE
        )
    }, error = function(e){
      NULL
    })
  })
  
  ## Tabla de alcance (output)
  quantity_histogram_table <- reactive({
    needs <- need_input_ready() %then%
      need_query_ready() %then%
      need_histogram_ready()
    shiny::validate(
       shiny::need(is.null(needs), '')
    )
    tryCatch({
      quantity_histogram_data() %>% 
        transform_columns(gl$cols) %>% 
        datatable(
          extensions = c('Buttons', 'FixedColumns', 'KeyTable'),
          filter = 'none',
          options = list(
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel'),
            fixedColumns = list(leftColumns = 2),
            keys = TRUE,
            scrollX = TRUE,
            scrollY = '200px',
            pageLength = 20
          ),
          colnames = remap_names(names(.), gl$cols, to_col = 'pretty_name'),
          callback = build_callback(remap_names(names(.), gl$cols, to_col = 'description'))
        ) %>%
        format_columns(gl$cols)
    }, error = function(e){
      NULL
    })
  })
  
  dispersion_histogram_table <- reactive({
    needs <- need_input_ready() %then%
      need_query_ready() %then%
      need_histogram_ready()
    shiny::validate(
      shiny::need(is.null(needs), '')
    )
    tryCatch({
      dispersion_histogram_data() %>% 
        transform_columns(gl$cols) %>% 
        datatable(
          extensions = c('Buttons', 'FixedColumns', 'KeyTable'),
          filter = 'none',
          options = list(
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel'),
            fixedColumns = list(leftColumns = 2),
            keys = TRUE,
            scrollX = TRUE,
            scrollY = '200px',
            pageLength = 20
          ),
          colnames = remap_names(names(.), gl$cols, to_col = 'pretty_name'),
          callback = build_callback(remap_names(names(.), gl$cols, to_col = 'description'))
        ) %>%
        format_columns(gl$cols)
    }, error = function(e){
      NULL
    })
  })
  
  observeEvent(input$histogram_selection, {
    ns <- session$ns
    if (input$histogram_selection == 'quantity') {
      output$histogram_slider <- renderUI(sliderInput(ns('quantity_histogram_bin_size'), lang$bin_size, min = 0.05, max = 0.5, value = 0.10, step = 0.05))
      output$feature_histogram <- renderPlotly(quantity_histogram())
      output$feature_histogram_table <- renderDT(server = FALSE, quantity_histogram_table())
    } else if (input$histogram_selection == 'dispersion') {
      output$histogram_slider <- renderUI(sliderInput(ns('dispersion_histogram_bin_size'), lang$bin_number, min = 2, max = 20, value = 5, step = 1))
      output$feature_histogram <- renderPlotly(dispersion_histogram())
      output$feature_histogram_table <- renderDT(dispersion_histogram_table())
    }
  })
  
  ## Reset con botón
  observeEvent(input$reset, {
    r$reset_trigger <- r$reset_trigger + 1
  })
  observeEvent(r$reset_trigger, {
    ## Esto es necesario porque al resetear la UI de input$items, no cambia el datapath
    r$items_file <- NULL
    r$items <- NULL
    graph_table(NULL)
    query_result(NULL)
    r$query_was_tried <- NULL
  })
  
  ## Descargar cálculos
  output$download_ui <- renderUI({
    req(final_result())
    ns <- session$ns
    downloadButton(ns('download'), label = lang$download, icon = icon('download'))
  })
  output$download <- downloadHandler(
    filename = function() {
      sprintf('estrategias_%s.csv', Sys.Date())
    },
    content = function(file) {
      flog.info(toJSON(list(
        session_info = msg_cred(credentials()),
        message = 'DOWNLOADING COMPUTATIONS FILE',
        details = list()
      )))
      write_excel_csv(final_result(), path = file, na = '')
    },
    contentType = 'text/csv'
  )
  
  ## Descargar resumen
  output$download_summary_ui <- renderUI({
    req(summary_table())
    ns <- session$ns
    downloadButton(ns('download_summary'), label = lang$download_summary, icon = icon('download'))
  })
  output$download_summary <- downloadHandler(
    filename = function() {
      sprintf('resumen_%s.csv', Sys.Date())
    },
    content = function(file) {
      flog.info(toJSON(list(
        session_info = msg_cred(credentials()),
        message = 'DOWNLOADING SUMMARY FILE',
        details = list()
      )))
      write_excel_csv(summary_table(), path = file, na = '')
    },
    contentType = 'text/csv'
  )
  
  ## Descargar template
  output$download_template <- downloadHandler(
    filename = 'promo-fulfillment-template.xlsx',
    content = function(file) {
      x <- generate_sample_input(calendar_day, gl$cols)
      openxlsx::write.xlsx(x, file = file, sheetName = "pf-template", append = FALSE, row.names = FALSE)
    },
    # Excel content type
    contentType = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
  )
  
  ## Mostrar instrucciones
  observeEvent(input$show_instructions, {
    flog.info(toJSON(list(
      session_info = msg_cred(credentials()),
      message = 'SHOWING INSTRUCTIONS',
      details = list()
    )))
    glossary_table <- gl$cols %>% 
      arrange(pretty_name) %>% 
      select(pretty_name, description, is_input, is_constant_by_feature, name) %>% 
      mutate_at(
        vars(is_input, is_constant_by_feature),
        ~fct_explicit_na(factor(ifelse(., lang$yes, lang$no)), '-')
      ) %>% 
      rename_all(~paste0('var_', .))
    showModal(modalDialog(
      size = 'l',
      easyClose = TRUE,
      title = NULL,
      tags$div(
        class = 'modal-tabbox',
        tags$h1('Instrucciones'),
        tabBox(
          width = 12,
          tabPanel(
            title = 'Uso básico',
            includeHTML('html/instructions-basic-usage.html')
          ),
          tabPanel(
            title = lang$compute_promotions_computation_parameters,
            includeHTML('html/instructions-computation.html')
          ),
          tabPanel(
            title = lang$compute_promotions_impact_parameters,
            includeHTML('html/instructions-impact.html')
          ),
          tabPanel(
            title = 'Glosario de columnas',
            renderDT({ 
              generate_basic_datatable(glossary_table, gl$cols)
            })
          )
        )
      ),
      modalButton(NULL, icon = icon('times')),
      footer = NULL
    ))
  }, ignoreInit = TRUE)
  
  ## Variables reactivas para poder reusar
  header <- reactive({
    req(r$items)
    r$items %>% 
      generate_header(impact_toggle = input$impact_toggle)
  })
  detail <- reactive({
    req(final_result())
    final_result() %>% 
      generate_detail()
  })
  ## Descargar HEADER
  output$download_header_ui <- renderUI({
    req(r$items)
    ns <- session$ns
    downloadButton(ns('download_header'), label = lang$download_header, icon = icon('download'))
  })
  output$download_header <- downloadHandler(
    filename = sprintf('HEADER_%s.csv', Sys.Date()),
    content = function(file) {
      flog.info(toJSON(list(
        session_info = msg_cred(credentials()),
        message = 'DOWNLOADING HEADER FILE',
        details = list()
      )))
     header() %>% 
       mutate(
         StartDate = format(StartDate, input$date_format),
         EndDate = format(EndDate, input$date_format),
       ) %>% 
       write_excel_csv(path = file, na = '')
    },
    contentType = 'text/csv'
  )
  
  ## Descargar DETAIL
  output$download_detail_ui <- renderUI({
    req(final_result())
    ns <- session$ns
    downloadButton(ns('download_detail'), label = lang$download_detail, icon = icon('download'))
  })
  output$download_detail <- downloadHandler(
    filename = sprintf('DETAIL_%s.csv', Sys.Date()),
    content = function(file) {
      flog.info(toJSON(list(
        session_info = msg_cred(credentials()),
        message = 'DOWNLOADING DETAIL FILE',
        details = list()
      )))
      data_files <- list(
        header = header(),
        detail = detail(),
        calculations = final_result(),
        items = r$items,
        params = list(
          impact_toggle = input$impact_toggle,
          min_feature_qty_toggle = input$min_feature_qty_toggle,
          sspres_benchmark_toggle = input$sspres_benchmark_toggle
        )
      )
      save_files(data_files = data_files, gl = gl, credentials = credentials())
      detail() %>% 
        mutate(
          `*StartDate` = format(`*StartDate`, input$date_format)
        ) %>% 
        write_excel_csv(path = file, na = '')
    },
    contentType = 'text/csv'
  )
  
  ## Regresar conexión dev para que se use en otros módulos
  reactive(reactiveValuesToList(r))
}


# UI ----------------------------------------------------------------------

computePromotionsUI <- function(id) {
  ns <- shiny::NS(id)
  
  if (gl$app_deployment_environment == 'dev') {
    login <- tagList(
      h3(lang$login),
      textInput(ns('user'), lang$user),
      passwordInput(ns('password'), lang$password),
      uiOutput(ns('auth_ui'))
    )
  } else {
    login <- NULL
  }
  
  fluidRow(
    box(
      width = 2,
      login,
      h3(
        lang$compute_promotions_inputs,
        title = lang$compute_promotions_inputs_title
      ),
      uiOutput(ns('items_ui')),
      tags$div(
        class = 'form-group',
        actionButton(ns('show_instructions'), label = '', title = lang$show_instructions, icon = icon('question-circle'), class = 'input-icon'),
        downloadButton(ns('download_template'), label = '', title = lang$download_template, icon = icon('download'), class = 'input-icon'),
        actionButton(ns('run'), label = '', title = lang$run, icon = icon('play'), class = 'input-icon'),
        actionButton(ns('reset'), label = '', title = lang$reset, icon = icon('redo-alt'), class = 'input-icon')
      ),
      tags$div(
        class = 'form-group',
        title = lang$input_grafica_ventas_title,
        shinyWidgets::materialSwitch(ns('graph_toggle'), tags$b(lang$graph_toggle), value = FALSE, status = 'info')
      ),
      h3(
        lang$compute_promotions_computation_parameters,
        title = lang$compute_promotions_computation_parameters_title
      ),
      tags$div(
        title = lang$min_feature_qty_toggle_title,
        selectInput(
          ns('min_feature_qty_toggle'),
          label = lang$min_feature_qty_toggle,
          choices = c('none', 'round_down', 'round_up') %>%
            set_names(lang$min_feature_qty_toggle_names)
        )
      ),
      h3(
        lang$compute_promotions_impact_parameters,
        title = lang$compute_promotions_impact_parameters_title
      ),
      tags$div(
        title = lang$sspres_benchmark_toggle_title,
        selectInput(
          ns('sspres_benchmark_toggle'),
          label = lang$sspres_benchmark_toggle,
          choices = c(
            'future',
            'current',
            'none'
            ) %>% 
            set_names(lang$sspres_benchmark_toggle_names)
        )
      ),
      tags$div(
        title = lang$impact_toggle_title,
        selectInput(
          ns('impact_toggle'),
          label = lang$impact_toggle,
          choices = c('add', 'swap') %>% 
            set_names(lang$impact_toggle_names)
        )
      )
    ),
    tabBox(
      id = ns('io'),
      selected = NULL,
      width = 10,
      tabPanel(
        value = 'input_table',
        title = lang$tab_input,
        uiOutput(ns('grafica_ventas_completa')),
        DTOutput(ns('input_table'))
      ),
      tabPanel(
        value = 'output_summary',
        title = lang$tab_output_summary,
        tags$div(
          class = 'inline-inputs',
          selectInput(
            ns('summary_groups'),
            label = lang$summary_groups,
            choices = c('feature_name', 'cid', 'store_nbr', 'dc_nbr') %>% 
              set_names(c(lang$feature_name, lang$cid, lang$store_nbr, lang$dc)),
            selected = c('feature_name', 'cid'),
            multiple = TRUE
          ),
          tags$div(
            class = 'form-group inline-inputs',
            style = 'margin-right: 10px; margin-left: 10px;',
            tags$div(
              class = 'inline-button-wrapper',
              uiOutput(ns('download_summary_ui'))
            ),
            tags$div(
              class = 'inline-button-wrapper',
              uiOutput(ns('download_header_ui'))
            ),
            tags$div(
              class = 'inline-button-wrapper',
              uiOutput(ns('download_detail_ui'))
            )
          ),
          selectInput(ns('date_format'), lang$date_format, c('yyyy-mm-dd' = '%Y-%m-%d',
                                                             'dd/mm/yyyy' = '%d/%m/%Y',
                                                             'mm/dd/yyyy' = '%m/%d/%Y'))
        ),
        DTOutput(ns('summary_table')) %>% withSpinner(type = 8)
      ),
      tabPanel(
        value = 'output_histogram',
        title = lang$tab_output_histogram,
        tags$div(
          class = 'form-group inline-inputs',
          tags$div(
            class = 'form-group',
            style = 'margin-right: 25px;',
            selectInput(
              ns('histogram_selection'),
              label = lang$histogram_selection,
              choices = c('quantity', 'dispersion') %>% 
                set_names(c(lang$quantity_histogram, lang$dispersion_histogram))
            )
          ),
          uiOutput(ns('output_feature_select_ui')),
          tags$div(
            class = 'form-group',
            style = 'margin-left: 30px;',
            uiOutput(ns('histogram_slider'))
          )
        ),
        plotlyOutput(ns('feature_histogram'), height = gl$plotly_height) %>% withSpinner(type = 8),
        DTOutput(ns('feature_histogram_table'))
      ),
      tabPanel(
        value = 'output_detail',
        title = lang$tab_output_table,
        uiOutput(ns('download_ui')),
        DTOutput(ns('detail_table')) %>% withSpinner(type = 8)
      )
    )
  )
}
