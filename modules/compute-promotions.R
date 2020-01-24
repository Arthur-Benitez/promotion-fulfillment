

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
alert_param <- function(combs_info, timestamp) {
  feature_info <- combs_info %>% 
    group_by(feature_name) %>% 
    summarise(
      any_empty = any(is_empty),
      all_empty = all(is_empty),
      any_measures_empty = any(measures_empty),
      any_default_pieces_applies = any(default_pieces_applies)
    )
  good_features <- with(feature_info, feature_name[!any_empty & (!any_measures_empty | any_default_pieces_applies)])
  partial_features <- with(feature_info, feature_name[(any_empty | (any_measures_empty & !any_default_pieces_applies)) & !all_empty])
  empty_features <- with(feature_info, feature_name[all_empty])
  
  if (length(good_features) > 0 && length(partial_features) == 0 && length(empty_features) == 0) {
    title1 <- lang$success
    text1 <- sprintf('La información fue descargada de Teradata en %s.', format_difftime(difftime(Sys.time(), timestamp)))
    type1 <- 'success'
    message1 <- 'DOWNLOAD SUCCESSFUL'
    table1 <- NULL
  } else if (length(good_features) > 0 || length(partial_features) > 0) {
    partial_combs <- combs_info %>% 
      filter(feature_name %in% partial_features & (is_empty | measures_empty)) %>%
      group_by(feature_name) %>% 
      summarise(
        sum_text = ifelse(
          length(old_nbr) > 3,
          sprintf('%s (%s, ...)', first(feature_name), paste0(old_nbr[1:3], collapse = ', ')),
          sprintf('%s (%s)', first(feature_name), paste0(old_nbr, collapse = ', '))
        )
      ) %>% 
      pull(sum_text)
    
    if (length(empty_features) > 0) {
      empty_list <- c(paste(empty_features, '(todos)'), partial_combs)
    } else {
      empty_list <- partial_combs
    }
    
    if (length(empty_list) > 5) {
      empty_list_displayed <- c(empty_list[1:5], '...') 
    } else {
      empty_list_displayed <- empty_list
    }
    
    title1 <- lang$warning
    text1 <- sprintf(
      '<div style="text-align:left;">
      Hubo problemas descargando la información para las siguientes combinaciones de exhibición-artículo: <br><ul><li>%s</li></ul>
      Las exhibiciones con al menos una combinación en la lista anterior serán completamente omitidas de los resultados.
      Te sugerimos <b>revisar que el departamento y formato que ingresaste para ellas sean correctos.</b> Para más información, revisa la tabla de "Combinaciones en conflicto" en la pestaña de Resultados.
      </div>',
      paste0(empty_list_displayed, collapse  = '</li><li>'))
    
    table1 <- combs_info %>% 
      filter(feature_name %in% partial_features & (is_empty | measures_empty)) %>% 
      mutate(old_nbr = as.character(old_nbr)) %>% 
      bind_rows(tibble(feature_name = empty_features, old_nbr = "Todos", is_empty = TRUE)) %>% 
      mutate(
        reason = case_when(
        is_empty ~ 'No se encontró información.',
        measures_empty ~ 'Una o más de las medidas del artículo están vacías.',
        TRUE ~ 'Otra razón.'
        ),
        solution = case_when(
          is_empty ~ 'Revisar que el departamento y formato sean correctos y que el artículo esté activo y resurtible.',
          measures_empty ~ 'Añadir medidas faltantes en el OIF / Usar el mueble predeterminado en piezas y dejar en blanco el mueble deseado.',
          TRUE ~ 'Sin solución recomendada.'
        )
      ) %>% 
      select(feature_name, old_nbr, reason, solution)
    type1 <- 'warning'
    message1 <- 'DOWNLOAD PARTIALLY FAILED'
  } else if (length(good_features) == 0 && length(partial_features) == 0 && length(empty_features) >= 0) {
    title1 <- lang$error
    text1 <- 'No se encontró información para ninguna de las promociones ingresadas, favor de revisar que sean correctos los datos.'
    type1 <- 'error'
    message1 <- 'DOWNLOAD FAILED'   
    table1 <- NULL
  }
  return(list(title = title1, text = text1, type = type1, message = message1, good_features = good_features, table = table1))
}

## Leer entrada
parse_input <- function(input_file, gl, calendar_day, date_format = '%Y-%m-%d') {
  if (tools::file_ext(input_file) != 'xlsx') {
    return('¡Cuidado! El template de carga debe estar en formato de Excel (.xlsx). Te sugerimos que descarges el formato ejemplo de nuevo.')
  }
  tryCatch({
    column_info <- gl$cols[gl$cols$is_input, ]
    required_cols <- column_info[column_info$is_required_input, ]
    nms <- names(readxl::read_excel(input_file, sheet = 1, n_max = 0))
    if (!all(required_cols$pretty_name %in% nms)) {
      return(sprintf('Las siguientes columnas faltan en el archivo que ingresaste: %s, puedes agregarlas manualmente o descargar el template de ejemplo.', paste(setdiff(required_cols$pretty_name, nms), collapse = ', ')))
    }
    nms <- remap_names(columns = nms, column_info, from_col = 'pretty_name', to_col = 'name')
    col_types <- generate_cols_spec(column_info, nms)
    items <- read_excel(
      path = input_file,
      sheet = 1,
      col_names = TRUE,
      col_types = col_types$excel_type
      ) %>% 
      magrittr::set_names(nms) %>%  
      init_col(c('white_list', 'black_list')) %>% 
      .[column_info$name] %>% 
      mutate_at(
        col_types$name[col_types$type %in% c('date')],
        as.Date
      ) %>% 
      mutate(
        fcst_or_sales = toupper(fcst_or_sales),
        display_key = paste(dept_nbr, old_nbr, negocio, sep = '.'),
        split_var = paste(semana_ini, semana_fin, fcst_or_sales, white_list, black_list, sep = '-')
      ) %>% 
      mutate_at(
        vars(shelf, default_shelf),
        ~toupper(str_replace(., '_', ' '))
      )
    
    stores_lists <- tryCatch({
      read_excel(
        path = input_file,
        sheet = 2,
        col_names = TRUE
      ) %>%
        map(~.x[!is.na(.x)])
    }, error = function(e) {
      return(NULL)
    })
    val <- validate_input(data = items, stores_lists = stores_lists, gl = gl, calendar_day = calendar_day)
    if (isTRUE(val)) {
      return(list(items = items, stores_lists = stores_lists))
    } else {
      return(val)
    }
  }, error = function(e){
    return('Error leyendo el archivo')
  })
}


## Validar inputs
validate_input <- function(data, stores_lists = NULL, gl, calendar_day) {
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
        data %>% 
          select(setdiff(names(.), column_info$name[column_info$allow_na])) %>% 
          anyNA() %>% 
          not(),
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
          summarise_at(setdiff(gl$feature_const_cols, group_vars(.)), list(~length(unique(.)))) %>% 
          select(-feature_name) %>%
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
        ## Checar que min_feature_perc esté entre 0 y 1
        'min_feature_perc debe ser un número entre 0 y 1 que represente un porcentaje de la capacidad máxima del mueble.',
        with(data, all(0 <= min_feature_perc & min_feature_perc <= 1)),
        ## Todas las listas usadas deben existir
        'Alguna de las columnas con las tiendas especiales a incluir o excluir no existe.',
        (is.null(stores_lists) && all(is.na(data$white_list)) && all(is.na(data$black_list))) || (all(discard(unique(data$white_list), is.na) %in% names(stores_lists)) && all(discard(unique(data$black_list), is.na) %in% names(stores_lists))),
        ## Verificar que todos los datos en las columnas de tiendas sean números
        'Las columnas de tiendas especiales deben contener sólo números.',
        stores_lists %>% 
          map_lgl(~typeof(.x) == 'double') %>% 
          all(),
        ## Checar los tipos de muebles deseados
        sprintf('El mueble debe ser uno de: %s', paste(gl$shelves, collapse = ', ')),
        all(data$shelf %in% gl$shelves | is.na(data$shelf)),
        ## Checar los datos de muebles default
        sprintf('El mueble predeterminado no puede estar vacío al mismo tiempo que el mueble deseado, especifica al menos uno. El mueble predeterminado debe ser uno de: %s, o bien, la cantidad de piezas que se desea simular como máximo de capacidad en el mueble.', paste(gl$shelves, collapse = ', ')),
        data %>% 
          mutate(
            validation = 
              !is.na(shelf) | 
              !is.na(as.numeric(default_shelf)) | 
              default_shelf %in% gl$shelves
          ) %>% 
          pull(validation) %>% 
          all
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
prepare_query <- function(query, keys, old_nbrs, negocios, wk_inicio, wk_final, white_list, black_list) {
  # Condición para sustituir el string en el query
  if (is.null(white_list)) {
    if (is.null(black_list)) {
      cond_str <- '1=1'
    } else {
      cond_str <- sprintf('T.STORE_NBR NOT IN (%s)', paste(black_list, collapse = ","))
    }
  } else {
    cond_str <- sprintf('T.STORE_NBR IN (%s)', paste(white_list, collapse = ","))
  }
  
  query %>% 
    str_replace_all('\\?KEY', paste0("'", paste(keys, collapse = "','"), "'")) %>%
    str_replace_all('\\?OLD_NBRS', paste(old_nbrs, collapse = ",")) %>%
    str_replace_all('\\?WK_INICIO', as.character(wk_inicio)) %>% 
    str_replace_all('\\?WK_FINAL', as.character(wk_final)) %>% 
    str_replace_all('\\?COND_STR', cond_str) %>% 
    str_replace_all('\\?NEGOCIOS', paste0("'", paste(unique(negocios), collapse = "','"), "'")) %>% 
    str_subset('^\\s*--', negate = TRUE) %>%  #quitar lineas de comentarios
    stringi::stri_trans_general('ASCII') %>% # quitar no ASCII porque truena en producción
    paste(collapse = '\n')
}
run_query_once <- function(ch, input_data, white_list, black_list, connector = 'production-connector') {
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
    negocios = input_data$negocio,
    wk_inicio = wk_inicio,
    wk_final = wk_final,
    white_list = white_list,
    black_list = black_list
  )
  tryCatch({
    res <- sql_query(
      ch = ch,
      connector = connector,
      query = query,
      stringsAsFactors = FALSE
    ) %>% 
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
run_query <- function(ch, input_data, stores_lists, connector = 'production-connector') {
  res <- input_data %>% 
    split(., .$split_var) %>% 
    map(safely(function(x){
      white_list <- stores_lists[[unique(x$white_list)]]
      black_list <- stores_lists[[unique(x$black_list)]]
      run_query_once(ch, x, white_list, black_list, connector)
    })) %>% 
    map('result') %>% 
    keep(~is.data.frame(.x) && !all(is.na(.x$store_nbr)) && nrow(.x) > 0) %>% 
    lapply(function(x) {mutate_at(x, vars(size_desc), as.character)})
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
    ) %>% 
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
    ) %>% 
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

## Regresar nombre de displays que no tuvieron info
get_empty_combs <- function(result, input) {
  measures_cols <- result %>% 
    select(contains('length_qty'), contains('height_qty'), contains('width_qty')) %>% 
    names()
  result %>% 
    group_by(feature_name, old_nbr) %>% 
    nest() %>% 
    mutate(
      is_empty = map_lgl(data, ~all(is.na(.x[setdiff(names(result), names(input))]))),
      measures_empty = map_lgl(data, ~any(is.na(.x[measures_cols]))),
      default_pieces_applies = map_lgl(data, ~!is.na(as.numeric(first(.x$default_shelf))) & is.na(first(.x$shelf)))
    ) %>% 
    select(feature_name, old_nbr, is_empty, measures_empty, default_pieces_applies)
}

## Cálculo de número de anaqueles basado en las alturas
calculate_shelves_number <- function(height, reduced_shelf_height, extra_space, max_shelves) {
  stacks <- ceiling((reduced_shelf_height - extra_space * max_shelves) / (height * max_shelves))
  shelves <- floor(reduced_shelf_height / (extra_space + height * stacks))
  shelves
}

## Cálculo de máxima cantidad (pzas / rrp)
create_capacity_columns <- function(data, prefix, measures, waste_space, extra_space) {
  initial_columns <- names(data)
  data <- data %>% 
    mutate(
      reduced_height = alto_cm - waste_space,
      max_shelves = 7,
      shelves_number = calculate_shelves_number(!!measures$height, reduced_height, extra_space, max_shelves),
      ah = round(reduced_height - (shelves_number * extra_space), digits = 2),
      lh = round(ah / shelves_number, digits = 2),
      avail_space = round((ancho_cm * profundo_cm * reduced_height) - (shelves_number * extra_space * ancho_cm * profundo_cm), digits = 2),
      tiers_per_shelf = floor(lh / !!measures$height),
      tiers_ttl = round(tiers_per_shelf * shelves_number),
      length_qty = floor(ancho_cm / !!measures$length),
      width_qty = floor(profundo_cm / !!measures$width),
      max_qty = round(tiers_ttl * width_qty * length_qty)
    ) %>% 
    select(-c(reduced_height, max_shelves))
  new_names <- setdiff(names(data), initial_columns)
  new_modified_names <- c(initial_columns, paste0(prefix, new_names))
  set_names(data, new_modified_names)
}

## Usa las medidas de los muebles para obtener las piezas máximas
perform_spatial_computations <- function(data) {
  finger_space <- 2.54
  tray_space <- 4.4958
  extra_space <- finger_space + tray_space
  # La bases de datos de los muebles trae para las chimeneas, las medidas de la parte superior de la chimenea (100 * 61 * 150), por lo que aquí debe calcularse sólo la parte baja, que equivale a un pallet o a una base estándar (100 * 122 * 150). Pero se usan 140cm de altura porque la suma debe ser de 2.9m y la parte alta que viene de la base de datos tienen una altura de 150cm en vez de 140cm como debiera ser.
  chimney_width <- 122
  chimney_length <- 100
  chimney_height <- 140
  waste_space <- 11
  item_measures <- syms(c(length = 'item_length_qty', height = 'item_height_qty', width = 'item_width_qty'))
  whpk_measures <- syms(c(length = 'whpk_length_qty', height = 'whpk_height_qty', width = 'whpk_width_qty'))
  
  data %>% 
    create_capacity_columns('no_rrp_', item_measures, waste_space, extra_space) %>% 
    create_capacity_columns('rrp_', whpk_measures, waste_space, extra_space) %>% 
    mutate(
      pallet_length_item_qty = round(ancho_cm / item_length_qty),
      pallet_height_item_qty = round(alto_cm / item_height_qty),
      pallet_width_item_qty  = round(profundo_cm  / item_width_qty),
      pallet_length_whpk_qty = round(ancho_cm / whpk_length_qty),
      pallet_height_whpk_qty = round(alto_cm / whpk_height_qty),
      pallet_width_whpk_qty  = round(profundo_cm  / whpk_width_qty),
      chimney_length_item_qty = round(chimney_length / item_length_qty),
      chimney_height_item_qty = round(chimney_height / item_height_qty),
      chimney_width_item_qty  = round(chimney_width  / item_width_qty),
      chimney_length_whpk_qty = round(chimney_length / whpk_length_qty),
      chimney_height_whpk_qty = round(chimney_height / whpk_height_qty),
      chimney_width_whpk_qty  = round(chimney_width  / whpk_width_qty),
      rrp_full_vol = round(rrp_max_qty * whpk_length_qty * whpk_width_qty * whpk_height_qty, digits = 2),
      left_avail_space = rrp_avail_space - rrp_full_vol,
      bkp_extra_pcs = floor(left_avail_space / (item_length_qty * item_width_qty * item_height_qty)),
      max_item_capacity = case_when(
        grepl('BASE', used_shelf) & rrp_ind == 'N' ~ 
          round(pallet_length_item_qty * pallet_height_item_qty * pallet_width_item_qty),
        grepl('BASE', used_shelf) & rrp_ind == 'Y' ~ 
          round(pallet_length_whpk_qty * pallet_height_whpk_qty * pallet_width_whpk_qty * whpk_qty),
        grepl('CABECERA', used_shelf) & rrp_ind == 'N' ~ no_rrp_max_qty,
        grepl('CABECERA', used_shelf) & rrp_ind == 'Y' ~ rrp_max_qty * whpk_qty + bkp_extra_pcs,
        grepl('CHIMENEA', used_shelf) & rrp_ind == 'N' ~ 
          round(
            pallet_length_item_qty * pallet_height_item_qty * pallet_width_item_qty + 
              chimney_length_item_qty * chimney_height_item_qty * chimney_width_item_qty
          ),
        grepl('CHIMENEA', used_shelf) & rrp_ind == 'Y' ~ 
          round(
            pallet_length_whpk_qty * pallet_height_whpk_qty * pallet_width_whpk_qty * whpk_qty + 
              chimney_length_whpk_qty * chimney_height_whpk_qty * chimney_width_whpk_qty * whpk_qty  
          ),
        TRUE ~ 0
      )
    )
}

## Resume la base de datos de muebles a cierto nivel
mean_shelves <- function(data, groups, suffix) {
  data %>%
    group_by(!!!syms(groups)) %>%
    select(!!!syms(groups), contains('cm')) %>%
    summarise_all(mean) %>% 
    rename_at(vars(contains('cm')), paste0, suffix) %>% 
    ungroup()
}

## Función para elegir mueble y calcular las piezas
calculate_max_capacity <- function(data){
  initial_columns <- names(data)
  # Bases de datos externas
  rrp_sync_data <- readRDS(gl$rrp_sync_database) %>% 
    set_names(tolower(names(.)))
  stores_shelves_df <- read_csv(gl$shelves_database) %>% 
    select(negocio, store_nbr, shelf, dept_nbr, alto_cm, ancho_cm, profundo_cm, shelves_qty) %>% 
    group_by(store_nbr, shelf, dept_nbr) %>% 
    arrange(desc(shelves_qty)) %>% 
    filter(row_number() == 1) %>% 
    ungroup() %>% 
    mutate(found = ifelse(is.na(alto_cm) | is.na(ancho_cm) | is.na(profundo_cm) | is.na(shelves_qty), FALSE, TRUE))
  # Bases de datos internas
  business_average_shelves <- mean_shelves(stores_shelves_df, c('negocio', 'shelf'), '_business_average_shelves')
  average_shelves <- mean_shelves(stores_shelves_df, c('shelf'), '_average_shelves')
  stores_shelves <- stores_shelves_df %>% 
    select(-c(negocio, shelves_qty)) %>% 
    rename_at(vars(contains('cm'), found), paste0, '_shelves')
  stores_default_shelves <- stores_shelves_df %>% 
    select(-c(negocio, shelves_qty)) %>% 
    rename_at(vars(contains('cm'), found), paste0, '_default_shelves')
  
  sorted_data <- data %>% 
    left_join(rrp_sync_data, by = 'old_nbr') %>% 
    mutate_at(vars('rrp_ind', 'gs1_sync_status'), list(~replace_na(., 'N'))) %>% 
    # Convertir de decímetros a centímetros
    mutate_at(vars(contains('length'), contains('width'), contains('height')), ~.x * 10) %>% 
    left_join(stores_shelves, by = c('store_nbr', 'shelf', 'dept_nbr')) %>% 
    left_join(
      stores_default_shelves,
      by = c('store_nbr' = 'store_nbr', 'default_shelf' = 'shelf', 'dept_nbr' = 'dept_nbr')
    ) %>% 
    mutate(
      classification = case_when(
        is.na(shelf) & !is.na(as.numeric(default_shelf)) ~ 'FORCED_DEFAULT',
        found_shelves ~ 'SHELF_FOUND',
        !is.na(as.numeric(default_shelf)) ~ 'UNFORCED_DEFAULT_PCS',
        found_default_shelves ~ 'UNFORCED_DEFAULT_CHAR',
        TRUE ~ 'NOT_FOUND'
      ),
      used_shelf = case_when(
        classification == 'FORCED_DEFAULT' ~ 'PIECES NUMBER',
        classification == 'SHELF_FOUND' ~ shelf,
        classification == 'UNFORCED_DEFAULT_PCS' ~ 'PIECES NUMBER',
        classification == 'UNFORCED_DEFAULT_CHAR' ~ default_shelf,
        classification == 'NOT_FOUND' ~ default_shelf
      )
    )
  
  ready_classifications <- c('FORCED_DEFAULT', 'UNFORCED_DEFAULT_PCS')
  ready_data <- sorted_data %>% 
    filter(classification %in% ready_classifications) %>% 
    mutate(max_item_capacity = as.numeric(default_shelf))
  
  ## Llamar función que calcula las piezas de acuerdo a tamaños, etc.
  sorted_data %>% 
    filter(!(classification %in% ready_classifications)) %>%
    left_join(business_average_shelves, by = c('default_shelf' = 'shelf', 'negocio' = 'negocio')) %>% 
    left_join(average_shelves, by = c('default_shelf' = 'shelf')) %>% 
    mutate(
      alto_cm = coalesce(alto_cm_shelves, alto_cm_default_shelves, alto_cm_business_average_shelves, alto_cm_average_shelves),
      ancho_cm = coalesce(ancho_cm_shelves, ancho_cm_default_shelves, ancho_cm_business_average_shelves, ancho_cm_average_shelves),
      profundo_cm = coalesce(profundo_cm_shelves, profundo_cm_default_shelves, profundo_cm_business_average_shelves, profundo_cm_average_shelves)
    ) %>%
    perform_spatial_computations() %>% 
    bind_rows(ready_data) %>% 
    select(-ends_with('cm_shelves'), -ends_with('cm_default_shelves'), -ends_with('cm_business_average_shelves'), -ends_with('cm_average_shelves')) %>% 
    mutate_at(setdiff(names(.), initial_columns), list(~replace_na(., 0)))
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
      ## Esta fórmula para el max_feature_qty se obtiene de una reducción algebráica.
      # V = Volumen total,              Vz = Volumen de cada artículo
      # Q = Cantidad total de piezas,   Qz = Cantidad de piezas de cada artículo
      # Uz = Volumen unitario ajustado para cada pieza (ajustado por el algoritmo de cálculo de espacios)
      # Pz = Participación de venta de cada artículo con respecto a la exhibición
      # Mz = Máxima cantidad de piezas que caben en la exhibición por artículo (max_item_capacity)
      
      # Hipótesis inicial: El volumen total es la suma de los volúmenes de los artículos:
      # V = sum(Vz)   =>    V = sum(Qz * Uz)    =>    V = Q * sum(Pz * Uz)
      # Por lo tanto...   Q = V / sum(Pz * Uz)    =>    Q = V / sum(Pz * (V/Mz))    =>    Q = 1 / sum(Pz / Mz)
      max_feature_qty = 1 / sum(feature_perc_pos_or_fcst / max_item_capacity),
      min_feature_qty = max_feature_qty * min_feature_perc,
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
    mutate_at(vars(contains("ss"), -classification), list(~round(replace_na(., 0), digits = 0))) %>%
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
      impact_vnpk = impact_qty / vnpk_qty,
      stock_qty = oh_qty + impact_qty,
      stock_cost = stock_qty * cost,
      stock_ddv = stock_qty / avg_dly_pos_or_fcst,
      stock_vnpk = stock_qty / vnpk_qty
    )
  return(data)
}

## Tabla de resumen
summarise_data <- function(data, group = c('feature_name', 'cid')) {
  ## Checks
  stopifnot(is.null(group) || all(group %in% c('feature_name', 'store_nbr', 'cid', 'dc_nbr', 'old_nbr')))
  ## Cambios a combinaciones específicas
  extra_groups <- list(
    'dc_nbr' = c('dc_name'),
    'store_nbr' = c('store_name')
  )
  group <- unique(c(group, unlist(extra_groups[intersect(names(extra_groups), group)])))
  if (is.null(group)) {
    group <- 'feature_name'
    data <- data %>% 
      mutate(feature_name = 'Total')
  }
  ## Grupos de tabla de salida
  group_order <- c('feature_name', 'store_nbr', 'store_name', 'cid', 'old_nbr', 'dc_nbr', 'dc_name')
  grp <- group_order[group_order %in% group]
  ## Variables numéricas de tabla de salida
  vv <- c('avg_dly_sales', 'avg_dly_forecast', 'min_feature_qty', 'max_feature_qty', 'max_item_capacity', 'max_ddv', 'total_cost', 'total_impact_cost', 'total_stock_cost', 'total_qty', 'total_impact_qty', 'total_stock_qty', 'total_ddv', 'total_impact_ddv', 'total_stock_ddv', 'total_vnpk', 'total_impact_vnpk', 'total_stock_vnpk')
  if ('store_nbr' %in% grp) {
    val_vars <- vv
  } else {
    val_vars <- c('n_stores', vv)
  }
  if ('cid' %in% grp || 'old_nbr' %in% grp) {
    val_vars <- c('primary_desc', val_vars)
  } else {
    val_vars <- val_vars
  }
  ## Sumarizar
  data_summary <- data  %>%
    group_by(!!!syms(grp)) %>%
    summarise(
      n_stores = n_distinct(store_nbr),
      primary_desc = first(primary_desc),
      ## Las ventas ya son promedio, así que sumándolas dan las ventas promedio de una entidad más grande
      avg_dly_sales = ifelse(any(fcst_or_sales == 'S'), sum(avg_dly_pos_or_fcst[fcst_or_sales=='S'], na.rm = TRUE), NA_real_),
      avg_dly_forecast = ifelse(any(fcst_or_sales == 'F'), sum(avg_dly_pos_or_fcst[fcst_or_sales=='F'], na.rm = TRUE), NA_real_),
      min_feature_qty = mean(min_feature_qty, na.rm = TRUE),
      max_feature_qty = mean(max_feature_qty, na.rm = TRUE),
      max_item_capacity = ifelse(length(unique(max_item_capacity)) > 1, sprintf('%s - %s', min(max_item_capacity), max(max_item_capacity)), as.character(unique(max_item_capacity))),
      max_ddv = sum(max_ddv * avg_dly_pos_or_fcst, na.rm = TRUE) / sum(avg_dly_pos_or_fcst, na.rm = TRUE),
      total_cost = sum(store_cost, na.rm = TRUE),
      total_impact_cost = sum(impact_cost, na.rm = TRUE),
      total_stock_cost = sum(stock_cost, na.rm = TRUE),
      total_qty = sum(feature_qty_fin, na.rm = TRUE),
      total_impact_qty = sum(impact_qty, na.rm = TRUE),
      total_stock_qty = sum(stock_qty, na.rm = TRUE),
      total_ddv = sum(feature_qty_fin, na.rm = TRUE) / sum(avg_dly_pos_or_fcst, na.rm = TRUE),
      total_impact_ddv = sum(impact_qty, na.rm = TRUE) / sum(avg_dly_pos_or_fcst, na.rm = TRUE),
      total_stock_ddv = sum(stock_qty, na.rm = TRUE) / sum(avg_dly_pos_or_fcst, na.rm = TRUE),
      total_vnpk = sum(vnpk_fin, na.rm = TRUE),
      total_impact_vnpk = sum(impact_vnpk, na.rm = TRUE),
      total_stock_vnpk = sum(stock_vnpk, na.rm = TRUE)
    ) %>% 
    group_by(!!!syms(grp)) %>% # Agrupar para guardar la info de los grupos afuera y fijar las columnas correctamente
    arrange(!!!syms(grp)) %>% 
    select(!!!syms(grp), !!!syms(val_vars))
  
  return(data_summary)
}

## Tabla de histograma
generate_quantity_histogram_data <- function(output_filtered_data, bins = 10) {
  res <- output_filtered_data %>% 
    summarise_data(group = c('feature_name', 'store_nbr')) %>% 
    ungroup() %>% 
    mutate(
      perc_max_feature_qty = round(total_qty / max_feature_qty, 5)
    )
  
  cut_values <- seq(0, 1, length.out = bins + 1)
  cut_labels <- paste(
    scales::percent(head(cut_values, -1), accuracy = 1),
    scales::percent(cut_values[-1], accuracy = 1),
    sep = ' - '
  )
  no_shelf_label <- 'Sin mueble'
  res %>% 
    mutate(
      perc_max_feature_qty = pmin(1, perc_max_feature_qty),
      perc_max_feature_qty_bin = 
        cut(perc_max_feature_qty, breaks = cut_values,labels = cut_labels, include.lowest = TRUE) %>% 
        fct_explicit_na(no_shelf_label) %>% 
        fct_expand(no_shelf_label) %>% 
        fct_relevel(no_shelf_label),
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
    right_join(tibble(perc_max_feature_qty_bin = fct_relevel(factor(c(no_shelf_label, cut_labels)), no_shelf_label)), by = 'perc_max_feature_qty_bin') %>% 
    replace(., is.na(.), 0) %>% 
    select(perc_max_feature_qty_bin, n_stores, p_stores, everything())
}

## Tabla de histograma
generate_dispersion_histogram_data <- function(output_filtered_data, bins_type = 'fixed', bins = 12, stock = 'total') {
  res <- output_filtered_data %>% 
    summarise_data(group = c('feature_name', 'store_nbr'))
  if (stock == 'total') {
    temp_vars <- syms(list(ddv = 'total_stock_ddv', qty = 'total_stock_qty', cost = 'total_stock_cost'))
  } else {
    temp_vars <- syms(list(ddv = 'total_ddv', qty = 'total_qty', cost = 'total_cost'))
  }
  ddv <- pull(res, !!temp_vars$ddv)
  
  if (bins_type == 'fixed') {
    cut_values <- c(0, 3, 7, 14, 21, 28, 35, 50, 75, 100, 150, 250, 350, 450, Inf)
  } else {
    cut_values <- floor(mean(ddv) + max(sd(ddv), 0.5) * seq(-2, 2, length.out = bins))
    cut_values <- unique(c(0, pmax(0, cut_values) %/% 5 * 5, Inf))
  }
  
  cut_labels <- paste(
    head(cut_values, -1),
    cut_values[-1],
    sep = ' - '
  ) %>% 
    replace(
      list = c(1, length(.)),
      values = c(
        sprintf('< %s', round(cut_values[2])),
        sprintf('> %s', round(cut_values[length(cut_values)-1]))
      )
    )
  
  res %>% 
    mutate(
      ddv = round(!!temp_vars$ddv, 1),
      ddv_bin = cut(ddv,  breaks = cut_values, labels = cut_labels, include.lowest = TRUE),
      temp_qty = !!temp_vars$qty,
      temp_cost = !!temp_vars$cost
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
  feature_times_vector <- c(4, 4, 4, 6, 5, 4)
  query_times_vector <- c(18, 9)
  info <- tibble(
    feature_name = rep(c('LICORES', 'ADEREZOS', 'CAFE', 'HIGIENE_FEM', 'BEBES', 'DETERGENTE'), feature_times_vector),
    user = 'm1234xy',
    dept_nbr = rep(c(96, 95, 92, 40, 69, 13), feature_times_vector),
    negocio = rep(c('SUPERCENTER', 'SUPERAMA', 'MIBODEGA', 'SUPERCENTER', 'BODEGA', 'BODEGA'), feature_times_vector),
    old_nbr = c(9660826, 100327654, 9609839, 9614873, 9554113, 100206810, 100236976, 100336602, 9289651, 9272072, 100101190, 100183858, 100369418, 4088042, 100276420, 100276070, 4078775, 100085127, 100273950, 6909653, 6909632, 6901253, 100290506, 100191564, 100379555, 100122572, 1319359),
    primary_desc_temp = c('GRAN CENTENARIO BCO', 'DON JULIO 70 700ML', 'CAZADORES REP 1.75LT', 'CORRALEJO 1LT+TARRO', 'HEINZ CATSUP ORGANIC', 'HEINZ SALSA CATSUP', 'ZAASCHILA SALS ARBOL', 'KNORR ROJA MARTAJADA', 'NESCAFE CLASICO 60G', 'AU CAFE SOLUBLE 100G', 'AU CAFE SOL LATA400G', 'AU CAFE LIGERO 400G', 'BENZAL WASH DURAZNO', 'LACTACYD FRESH 220M', 'SHAMPOO ESOS DIAS', 'TOALLAS HUMEDAS', 'BENZAL SPRAY BABY P', 'SPRAY ISLAND SPLASH', 'BEBYTO T4 72S', 'BEBYTO T5 40', 'BEBYTO T4 40', 'BEBYTO T3 14', 'TH PARENTS CHOICE 80', 'BOLD AMORES 5KG', 'ACE REGULAR 4 KG', 'GV DETERG POLVO 3KG', 'FOCA DET 1K'),
    min_feature_perc = rep(c(0.3, 0.2, 0.35, 0.4, 0.1, 0.15), feature_times_vector),
    shelf = rep(c('CABECERA_ALTA', 'CABECERA BAJA', 'BASE', NA, 'MEDIA_BASE', 'CHIMENEA'), feature_times_vector),
    default_shelf = rep(c('CABECERA_BAJA', 250, NA, 500, 80, 'MEDIA BASE'), feature_times_vector),
    max_ddv = rep(c(20, 15, 18, 30, 10, 15), feature_times_vector),
    fcst_or_sales = rep(c('S', 'F'), query_times_vector),
    semana_ini = rep(c(sales_wks[1], fcst_wks[1]), query_times_vector),
    semana_fin = rep(c(sales_wks[2], fcst_wks[2]), query_times_vector),
    StartDate = rep(c(Sys.Date() + 7, Sys.Date() + 14), query_times_vector),
    EndDate = rep(c(Sys.Date() + 35, Sys.Date() + 49), query_times_vector),
    Priority = 12,
    white_list = rep(c('tiendas_grandes', NA), c(4, 23)),
    black_list = rep(c(NA, 'tiendas_chicas_y_muy_chicas', NA), c(4, 4, 19))
  )
  names(info) <- remap_names(names(info), column_info, to_col = 'pretty_name')
  stores_lists <- tryCatch({
    read_csv('data/stores-lists.csv')
  }, error = function(e){
    list(
      tiendas_muy_chicas = c(3733, 3983, 1424, 2052, 4036),
      tiendas_chicas = c(3733, 3983, 1424, 2052, 4036),
      tiendas_chicas_y_muy_chicas = c(3733, 3983, 1424, 2052, 4036),
      tiendas_medianas = c(3733, 3983, 1424, 2052, 4036),
      tiendas_grandes = c(3733, 3983, 1424, 2052, 4036)
    )
  })
  catalog <- list(negocios_validos = gl$negocios, muebles_validos = gl$shelves)
  
  dictionary <- gl$cols %>% 
    filter(is_input) %>% 
    select(pretty_name, description) %>% 
    set_names(c('Nombre de columna', 'Descripción'))
  
  return(list(promo = info, tiendas_especiales = stores_lists, catalogo = catalog, glosario = dictionary))
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
    stores_lists = NULL,
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
    r$stores_lists <- NULL
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
    if (!is.list(val) || !is.data.frame(val$items)) {
      shinyalert(
        type = "error", 
        title = lang$error,
        text = val,
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE
      )
      r$items <- NULL
      r$stores_lists <- NULL
      flog.error(toJSON(list(
        session_info = msg_cred(credentials()),
        message = 'PARSING INPUT FILE FAILED',
        details = list(
          file = r$items_file,
          reason = val
        )
      )))
    } else {
      r$items <- val$items
      r$stores_lists <- val$stores_lists
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
      select_if(~!all(is.na(.))) %>% 
      generate_basic_datatable(gl$cols, scrollX = TRUE, scrollY = ifelse(input$graph_toggle, gl$table_height$short, gl$table_height$tall))
  })
  
  output$stores_lists_table <- renderDT({
    shiny::validate(
      shiny::need(!is.null(r$items_file), lang$need_items_file) %then%
        shiny::need(!is.null(r$stores_lists), lang$no_stores_lists)
    )
    r$stores_lists %>% 
      fill_vectors %>% 
      generate_basic_datatable(gl$cols, scrollX = TRUE)
  })
  
  ## Apagar bandera r$is_running
  observeEvent(graph_table(), {
    r$is_running <- FALSE
  })
  
  output$sales_summary_groups_input <- renderUI({
    req(r$items)
    req(is.data.frame(graph_table()))
    req(isTRUE(input$graph_toggle))
    ns <- session$ns
    selectInput(
      ns('sales_summary_groups'),
      label = lang$sales_summary_groups,
      choices = c('feature_name', 'old_nbr', 'negocio', 'dept_nbr', 'cid') %>% 
        set_names(c(lang$feature_name, lang$old_nbr, lang$business, lang$departamento, lang$cid)),
      multiple = TRUE,
      selected = c('feature_name'),
      width = '100%'
    )
  })
  
  sales_graph_group_cols <- reactive({
    group_cols <- input$sales_summary_groups %>% 
      replace(input$sales_summary_groups == 'old_nbr', 'old_nbr_desc')
    if (!('old_nbr' %in% input$sales_summary_groups)) {
      group_cols <- group_cols %>% 
        replace(group_cols == 'cid', 'cid_desc')
    } else {
      group_cols
    }
  })
  
  graph_choices <- reactiveVal()
  output$sales_graph_selector_input <- renderUI({
    req(r$items)
    req(is.data.frame(graph_table()))
    req(isTRUE(input$graph_toggle))
    ns <- session$ns
    if (length(input$sales_summary_groups) == 0) {
      graph_choices('Todos')
    } else {
      table <- graph_table()
      if ('cid' %in% input$sales_summary_groups) {
        table <- table %>% 
          group_by(cid) %>% 
          mutate(
            primary_desc = first(primary_desc)
          ) %>% 
          ungroup()
      }
      table %>% 
        right_join(r$items, by = c('old_nbr', 'negocio')) %>% 
        mutate(
          old_nbr_desc = paste0(primary_desc, ' (', old_nbr, ')'),
          cid_desc = paste0(primary_desc, ' (', cid, ')')
        ) %>% 
        select(sales_graph_group_cols()) %>% 
        apply(1, paste, collapse = '-') %>% 
        unique() %>% 
        sort() %>% 
        graph_choices()
    }
    selectInput(
      ns('sales_graph_selector'),
      lang$grafica_ventas,
      choices = graph_choices(),
      width = '100%'
    )
  })
  
  output$sales_graph_agg_input <- renderUI({
    req(r$items)
    req(is.data.frame(graph_table()))
    req(isTRUE(input$graph_toggle))
    ns <- session$ns
    selectInput(
      ns('sales_graph_agg'),
      lang$sales_graph_agg,
      choices = c('avg', 'sum') %>% 
        set_names(lang$sales_graph_agg_names),
      width = '100%'
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
        shiny::need(input$sales_graph_selector %in% graph_choices(), '')
    )
    flog.info(toJSON(list(
      session_info = msg_cred(credentials()),
      message = 'GENERATING SALES GRAPH',
      details = list()
    )))
    df <- graph_table() %>% 
      right_join(
        y = r$items[c('old_nbr', 'negocio', 'dept_nbr', intersect('feature_name', input$sales_summary_groups))],
        by = c('old_nbr', 'negocio')
      ) %>% 
      unique() %>% 
      group_by(cid) %>% 
      mutate(
        primary_desc = first(primary_desc)
      ) %>%
      ungroup() %>% 
      mutate(
        old_nbr_desc = paste0(primary_desc, ' (', old_nbr, ')'),
        cid_desc = paste0(primary_desc, ' (', cid, ')')
      )
    if (length(sales_graph_group_cols()) == 0) {
      df$filtro <- 'Todos'
    } else {
      df$filtro <- df[sales_graph_group_cols()] %>% 
        apply(1, paste, collapse = '-')
    }
    df <- df %>% 
      filter(filtro == input$sales_graph_selector) %>% 
      mutate(avg_store_wkly_qty = wkly_qty / n_stores) %>% 
      na.omit() %>% 
      group_by(wm_yr_wk, date) %>% 
      summarise(
        type = unique(type),
        avg_store_wkly_qty = sum(avg_store_wkly_qty, na.rm = TRUE),
        sell_price = sum(sell_price * wkly_qty, na.rm = TRUE) / sum(wkly_qty, na.rm = TRUE),
        wkly_qty = sum(wkly_qty, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      select(
        wm_yr_wk,
        date,
        type,
        avg_store_wkly_qty,
        sell_price,
        wkly_qty
      )
    
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
            input$sales_graph_agg == 'avg' ~ avg_store_wkly_qty,
            input$sales_graph_agg == 'sum' ~ wkly_qty,
            TRUE ~ avg_store_wkly_qty
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
            text = sprintf("Ventas semanales en piezas (%s)", lang$sales_graph_agg_names[input$sales_graph_agg])
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
      tags$div(
        class = 'evenly-spaced-inputs',
        uiOutput(ns('sales_summary_groups_input'), style = 'width: 29%;'),
        uiOutput(ns('sales_graph_selector_input'), style = 'width: 50%;'),
        uiOutput(ns('sales_graph_agg_input'), style = 'width: 14%;')
      ),
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
  query_warning <- reactiveVal()
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
    stores_lists <- r$stores_lists
    usr <- input$user
    pwd <- input$password
    query_result_promise <- future({
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
        data = run_query(future_ch, items, stores_lists, 'production-connector'),
        data_ss = search_ss(future_ch, items, 'WM3')
      )
    })
    query_result_promise %...>% 
      query_result()
    
    query_timeout_promise <- future({
      Sys.sleep(gl$timeout_warning_duration)
      list(
        timestamp = time1,
        data = TRUE,
        data_ss = TRUE
      )
    })
    promise_race(query_result_promise, query_timeout_promise) %...>% 
      query_warning()
    
  }, ignoreInit = TRUE)
  
  ## Hacer cálculos
  ### Mostrar alertas y checar info
  observeEvent(query_warning(), {
    req(isTRUE(query_warning()$data))
    flog.info(toJSON(list(
      session_info = msg_cred(isolate(credentials())),
      message = 'QUERY TIMED OUT',
      details = list()
    )))
    shinyalert::shinyalert(
      type = 'warning',
      title = lang$warning,
      text = sprintf('Esto podría demorar unos minutos porque Teradata está tardando más de lo normal. Si quieres, puedes esperar a que termine el proceso, pero te sugerimos intentarlo más tarde. Si quieres reiniciar la aplicación, carga la página de nuevo.<br>Hora de inicio: %s', format(query_warning()$timestamp, "%X", tz = 'America/Mexico_City')),
      showConfirmButton = FALSE,
      html = TRUE
    )
  })
  good_features_rv <- reactiveVal()
  failed_combinations <- reactiveVal(NULL)
  risky_combinations <- reactiveVal(NULL)
  observeEvent(query_result(), {
    r$is_running <- FALSE
    if (!is.data.frame(query_result()$data)) {
      alert_info <- list(
        title = lang$error,
        text = 'No se encontró información para ninguna de las promociones ingresadas, favor de revisar que sean correctos los datos.',
        type = 'error',
        message = 'DOWNLOAD FAILED'
      )
      good_features_rv(NULL)
    } else {
      combs_info <- get_empty_combs(query_result()$data, isolate(r$items))
      alert_info <- alert_param(combs_info, query_result()$timestamp)
      good_features_rv(alert_info$good_features)
    }
    # Si falla parcialmente, asigna la tabla, de otra forma asigna un NULL
    failed_combinations(alert_info$table)
    
    shinyalert::shinyalert(
      type = alert_info$type,
      title = alert_info$title,
      text = alert_info$text,
      closeOnClickOutside = TRUE,
      html = TRUE
    )
    flog.info(toJSON(list(
      session_info = msg_cred(isolate(credentials())),
      message = alert_info$message,
      details = list()
    )))
    r$final_result_trigger <- r$final_result_trigger + 1
  })
  output$failed_combinations_dt <- renderDT({
    req(!is.null(failed_combinations()))
    generate_basic_datatable(failed_combinations(), gl$cols)
  })
  
  output$risky_combinations_dt <- renderDT({
    req(!is.null(risky_combinations()))
    generate_basic_datatable(risky_combinations(), gl$cols)
  })
    
  output$alert_info_ui <- renderUI({
    req(final_result())
    req(!is.null(failed_combinations()) | !is.null(risky_combinations()))
    ns <- session$ns
    risky_combinations_text <- NULL
    failed_combinations_text <- NULL
    if(!is.null(risky_combinations())) {
      risky_combinations_text <- tags$div(
        tags$h3(tags$span(style = "color: #f47521", 'Combinaciones en riesgo')),
        tags$h5(
          'Hemos detectado que los muebles de algunas exhibiciones tienen espacio para almacenar una gran cantidad de DDV de algunos de los artículos que incluíste en ellos. Por favor, revisa que los muebles que especificaste sean los adecuados y que las medidas de los artículos sean correctas para las combinaciones de exhibición-artículoque se muestran abajo.'
        )
      )
    }
    if(!is.null(failed_combinations())) {
      failed_combinations_text <- tags$div(
        tags$h3(tags$span(style = "color: red", 'Combinaciones en conflicto')),
        tags$h5('Hubo problemas al realizar la descarga de información de las combinaciones de promoción-artículo que se muestran en la tabla de abajo. No se encontró la información necesaria de las mismas para ser procesadas por la aplicación, por lo que las promociones a las que pertencen fueron completamente excluidas de los resultados.')
      )
    }
    tags$div(
      tags$h2(tags$b('Alertas')),
      risky_combinations_text,
      DTOutput(ns('risky_combinations_dt')),
      failed_combinations_text,
      DTOutput(ns('failed_combinations_dt'))
    )
  })

  ### Ahora sí cálculos
  final_result <- eventReactive({
    input$min_feature_qty_toggle
    input$sspres_benchmark_toggle
    input$impact_toggle
    r$final_result_trigger
    r$items
  }, {
    req(r$final_result_trigger > 0)
    req(query_result()$data)
    if (length(good_features_rv()) > 0) {
      good_data <- query_result()$data %>% 
        filter(feature_name %in% good_features_rv())
      if(file.exists(gl$shelves_database)) {
        flog.info(toJSON(list(
          session_info = msg_cred(isolate(credentials())),
          message = 'CALCULATING SHELVES MAX CAPACITY',
          details = list()
        )))
        shelf_data <- purrr::safely(calculate_max_capacity)(
          data = good_data
        )$result
        flog.info(toJSON(list(
          session_info = msg_cred(isolate(credentials())),
          message = 'PERFORMING COMPUTATIONS',
          details = list()
        )))
        purrr::safely(perform_computations)(
          data = shelf_data,
          data_ss = query_result()$data_ss,
          min_feature_qty_toggle = input$min_feature_qty_toggle,
          sspres_benchmark_toggle = input$sspres_benchmark_toggle,
          impact_toggle = input$impact_toggle
        )$result
      } else {
        flog.info(toJSON(list(
          session_info = msg_cred(credentials()),
          message = 'SHELVES DATABASE NOT FOUND',
          details = list(
            user = input$user
          )
        )))
      }
    } else {
      NULL
    }
  })
  
  observeEvent(final_result(), {
    req(!is.null(final_result()))
    items_list <- final_result() %>% 
      group_by(feature_name, old_nbr, used_shelf) %>% 
      summarise_at(vars(feature_qty_req, feature_ddv_req, max_ddv), mean) %>% 
      filter(feature_ddv_req >= max_ddv * 10)
    if (length(items_list) <= 0) {
      items_list <- NULL
    }
    risky_combinations(items_list)
  })
  
  ## Validaciones
  need_input_ready <- reactive({
    shiny::need(r$is_open || gl$app_deployment_environment == 'prod', lang$need_auth) %then%
      shiny::need(!is.null(r$items_file), lang$need_items_file) %then%
      shiny::need(!is.null(r$items), lang$need_valid_input)
  })
  need_result_ready <- reactive({
    shiny::need(r$query_was_tried, lang$need_run) %then%
      shiny::need(!r$is_running, lang$need_finish_running) %then%
      shiny::need(is.data.frame(query_result()$data), lang$need_query_result) %then%
      shiny::need(is.data.frame(final_result()), lang$need_final_result)
  })
  need_histogram_ready <- reactive({
    shiny::need(!is.null(quantity_histogram_data()) || !is.null(dispersion_histogram_data()), lang$need_final_result) %then%
      shiny::need(nchar(input$output_feature_select) > 0, lang$need_select_feature)
  })
  
  ## Tabla que muestra las medidas de los artículos y su estatus de sincronización
  output$item_details_table <- renderDT({
    shiny::validate(
      need_input_ready() %then%
        need_result_ready()
    )
    tryCatch({
      final_result() %>%
        group_by(old_nbr) %>% 
        summarise_at(
          vars('primary_desc', 'item_length_qty', 'item_width_qty', 'item_height_qty', 'whpk_length_qty', 'whpk_width_qty', 'whpk_height_qty', 'rrp_ind', 'gs1_sync_status'),
          first
        ) %>% 
        generate_basic_datatable(column_info = gl$cols, scrollX = TRUE, scrollY = gl$table_height$tall)
    }, error = function(e){
      NULL
    })
  })
  
  ## Tabla de salida
  output$detail_table <- renderDT({
    shiny::validate(
      need_input_ready() %then%
        need_result_ready()
    )
    tryCatch({
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
            scrollY = gl$table_height$medium,
            pageLength = 100
          ),
          colnames = remap_names(names(.), gl$cols, to_col = 'pretty_name'),
          callback = build_callback(names(.), gl$cols)
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
        need_result_ready()
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
            scrollY = gl$table_height$medium,
            pageLength = 100
          ),
          colnames = remap_names(names(.), gl$cols, to_col = 'pretty_name'),
          callback = build_callback(names(.), gl$cols)
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
      choices = choices,
      width = '100%'
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
    generate_quantity_histogram_data(final_results_filt(), bins = input$quantity_histogram_bin_number)
  })
  dispersion_histogram_data <- reactive({
    req(final_results_filt())
    generate_dispersion_histogram_data(final_results_filt(), bins_type = input$dispersion_bin_selection, bins = input$dispersion_histogram_bin_number, stock = input$dispersion_histogram_stock_toggle)
  })
  
  ## Histograma de alcance
  quantity_histogram <- reactive({
    shiny::validate(
      need_input_ready() %then%
        need_result_ready() %then%
        need_histogram_ready()
    )
    tryCatch({
      if(all(is.na(final_results_filt()$shelf))) {
        shelf <- unique(final_results_filt()$default_shelf)
      } else {
        shelf <- unique(final_results_filt()$shelf)
      }
      quantity_histogram_data() %>% 
        mutate(
          label_y = n_stores + 0.03 * max(n_stores),
          label = scales::percent(p_stores),
          text = sprintf('Tiendas: %s (%s)<br>Costo total: %s<br>Costo promedio: %s<br>Cant. total: %s<br>Cant. promedio: %s<br>%s promedio: %s', scales::comma(n_stores, accuracy = 1), scales::percent(p_stores), scales::comma(total_cost, accuracy = 1), scales::comma(avg_store_cost, accuracy = 1), scales::comma(total_qty, accuracy = 1), scales::comma(avg_store_qty, accuracy = 1), ifelse(first(fcst_or_sales) == 'F', 'Forecast', 'Venta'), scales::comma(avg_store_dly_pos_or_fcst, accuracy = 1))
        ) %>% 
        plot_ly(
          x = ~perc_max_feature_qty_bin,
          y = ~n_stores,
          text = ~text,
          hoverinfo = 'text',
          type = 'bar',
          name = NULL,
          marker = list(color = c('red', rep('#1f76b4', input$quantity_histogram_bin_number)))
        ) %>% 
        add_text(x = ~perc_max_feature_qty_bin, y = ~label_y, text = ~label, name = NULL, color = I('black'), inherit = FALSE) %>% 
        plotly::layout(
          title = 'Alcance porcentual a piezas máximas por tienda',
          xaxis = list(title = case_when(
            is.na(shelf) ~ 'Mueble no encontrado',
            is.numeric(shelf) ~ sprintf('Alcance a un máximo de %s piezas', try(scales::comma(shelf), silent = TRUE)),
            TRUE ~ 'Alcance a capacidad máxima del mueble encontrado para cada tienda',
          )),
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
        need_result_ready() %then%
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
      need_result_ready() %then%
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
            scrollY = gl$table_height$short,
            pageLength = 20
          ),
          colnames = remap_names(names(.), gl$cols, to_col = 'pretty_name'),
          callback = build_callback(names(.), gl$cols)
        ) %>%
        format_columns(gl$cols)
    }, error = function(e){
      NULL
    })
  })
  
  dispersion_histogram_table <- reactive({
    needs <- need_input_ready() %then%
      need_result_ready() %then%
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
            scrollY = gl$table_height$short,
            pageLength = 20
          ),
          colnames = remap_names(names(.), gl$cols, to_col = 'pretty_name'),
          callback = build_callback(names(.), gl$cols)
        ) %>%
        format_columns(gl$cols)
    }, error = function(e){
      NULL
    })
  })
  
  output$dispersion_histogram_bin_number_ui <- renderUI({
    req(input$dispersion_bin_selection == 'calculated')
    ns <- session$ns
    sliderInput(
      ns('dispersion_histogram_bin_number'),
      lang$bin_number,
      min = 4, max = 15, value = 10, step = 1,
      width = '100%'
    )
  })
  
  output$dispersion_histogram_stock_toggle_ui <- renderUI({
    req(input$histogram_selection == 'dispersion')
    ns <- session$ns
    tags$div(
      title = lang$dispersion_histogram_stock_toggle_title,
      shinyWidgets::radioGroupButtons(
        inputId = ns('dispersion_histogram_stock_toggle'),
        label = lang$dispersion_histogram_stock_toggle, 
        choices = c('Promoción' = 'promo', 'Promoción + OH actual' = 'total'),
        selected = 'total',
        justified = TRUE,
        direction = 'vertical'
      )
    )
  })
  
  observeEvent(input$histogram_selection, {
    ns <- session$ns
    if (input$histogram_selection == 'quantity') {
      output$histogram_input <- renderUI(sliderInput(ns('quantity_histogram_bin_number'), lang$bin_number, min = 2, max = 20, value = 10, step = 1))
      output$feature_histogram <- renderPlotly(quantity_histogram())
      output$feature_histogram_table <- renderDT(server = FALSE, quantity_histogram_table())
    } else if (input$histogram_selection == 'dispersion') {
      output$histogram_input <- renderUI(
        tags$div(
          class = 'evenly-spaced-inputs',
          selectInput(
            ns('dispersion_bin_selection'),
            label = lang$dispersion_bin_selection,
            choices = c('fixed', 'calculated') %>% 
              set_names(c(lang$dispersion_fixed_bins, lang$dispersion_calculated_bins)),
            width = '23%'
          ),
          uiOutput(ns('dispersion_histogram_stock_toggle_ui'), style = 'width: 28%;'),
          uiOutput(ns('dispersion_histogram_bin_number_ui'), style = 'width: 40%;')
        )
      )
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
    r$stores_lists <- NULL
    graph_table(NULL)
    query_result(NULL)
    r$query_was_tried <- NULL
    failed_combinations(NULL)
    risky_combinations(NULL)
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
      write_excel_csv(
        final_result() %>% 
          magrittr::set_names(remap_names(names(.), column_info = gl$cols)),
        path = file,
        na = ''
      )
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
      write_excel_csv(
        summary_table() %>% 
          magrittr::set_names(remap_names(names(.), column_info = gl$cols)),
        path = file,
        na = ''
      )
    },
    contentType = 'text/csv'
  )
  
  ## Descargar template
  output$download_template <- downloadHandler(
    filename = 'promo-fulfillment-template.xlsx',
    content = function(file) {
      x <- generate_sample_input(calendar_day, gl$cols)
      x$tiendas_especiales <- fill_vectors(x$tiendas_especiales)
      x$catalogo <- fill_vectors(x$catalogo)
      openxlsx::write.xlsx(x, file = file, append = FALSE, row.names = FALSE, tabColour = c('#0071ce', '#eb148d', '#ffc220', '#76c043'))
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
            title = lang$compute_promotions_basic_intructions,
            includeHTML('html/instructions-basic-usage.html')
          ),
          tabPanel(
            title = lang$compute_promotions_computation_parameters,
            includeHTML('html/instructions-computation.html')
          ),
          tabPanel(
            title = lang$compute_promotions_shelves,
            includeHTML('html/instructions-shelves.html') %>%
            str_replace_all('__shelves__', paste(gl$shelves, collapse = ', ')) %>%
            HTML()
          ),
          tabPanel(
            title = lang$compute_promotions_impact_parameters,
            includeHTML('html/instructions-impact.html')
          ),
          tabPanel(
            title = lang$compute_promotions_stores_lists,
            includeHTML('html/instructions-stores-lists.html')
          ),
          tabPanel(
            title = lang$compute_promotions_columns_glossary,
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
      if (nrow(detail()) >= gl$max_output_rows) {
        shinyalert::shinyalert(
          type = 'warning',
          title = lang$warning,
          text = sprintf('El archivo "detail" excede el límite permitido por GRS de %s renglones por carga.', scales::comma(gl$max_output_rows, accuracy = 1)),
          closeOnClickOutside = TRUE,
          timer = 4000
        )
      }
      data_files <- list(
        header = header(),
        detail = detail(),
        calculations = final_result(),
        items = r$items,
        stores_lists = r$stores_lists,
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
  
  output$impact_toggle_ui <- renderUI({
    req(input$sspres_benchmark_toggle != 'none')
    ns <- session$ns
    selectInput(
      ns('impact_toggle'),
      label = lang$impact_toggle,
      choices = c('add', 'swap') %>% 
        set_names(lang$impact_toggle_names)
    )
  })
  
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
        class = 'form-group evenly-spaced-inputs',
        actionButton(ns('show_instructions'), label = '', title = lang$show_instructions, icon = icon('question-circle'), class = 'input-icon'),
        downloadButton(ns('download_template'), label = '', title = lang$download_template, icon = icon('download'), class = 'input-icon'),
        actionButton(ns('run'), label = '', title = lang$run, icon = icon('play'), class = 'input-icon'),
        actionButton(ns('reset'), label = '', title = lang$reset, icon = icon('redo-alt'), class = 'input-icon')
      ),
      tags$div(
        class = 'form-group',
        title = lang$sales_graph_selector_title,
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
        uiOutput(ns('impact_toggle_ui'))
      )
    ),
    tabBox(
      id = ns('io'),
      selected = NULL,
      width = 10,
      tabPanel(
        value = 'inputs_tabPanel',
        title = lang$tab_input,
        tabBox(
          width = '100%',
          tabPanel(
            value = 'input_table',
            title = lang$tab_input_graph,
            uiOutput(ns('grafica_ventas_completa')),
            DTOutput(ns('input_table'))
          ),
          tabPanel(
            value = 'stores_lists',
            title = lang$tab_stores_lists,
            DTOutput(ns('stores_lists_table'))
          )
        )
      ),
      tabPanel(
        value = 'output_summary',
        title = lang$tab_output_summary,
        tags$div(
          class = 'inline-inputs',
          selectInput(
            ns('summary_groups'),
            label = lang$summary_groups,
            choices = c('feature_name', 'cid', 'old_nbr', 'store_nbr', 'dc_nbr') %>% 
              set_names(c(lang$feature_name, lang$cid, lang$old_nbr, lang$store_nbr, lang$dc)),
            selected = c('feature_name', 'old_nbr'),
            multiple = TRUE
          ),
          tags$div(
            class = 'inline-dropdown-margin',
            selectInput(
              ns('date_format'),
              lang$date_format,
              c('yyyy-mm-dd' = '%Y-%m-%d', 'dd/mm/yyyy' = '%d/%m/%Y', 'mm/dd/yyyy' = '%m/%d/%Y')
            ) 
          ),
          tags$div(
            class = 'form-group inline-inputs',
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
          )
        ),
        DTOutput(ns('summary_table')) %>% withSpinner(type = 8),
        uiOutput(ns('alert_info_ui'))
      ),
      tabPanel(
        value = 'output_histogram',
        title = lang$tab_output_histogram,
        tags$div(
          class = 'evenly-spaced-inputs',
          selectInput(
            ns('histogram_selection'),
            label = lang$histogram_selection,
            choices = c('quantity', 'dispersion') %>% 
              set_names(c(lang$quantity_histogram, lang$dispersion_histogram)),
            width = '16%'
          ),
          uiOutput(ns('output_feature_select_ui'), style = 'width: 30%;'),
          uiOutput(ns('histogram_input'), style = 'width: 50%;')
        ),
        plotlyOutput(ns('feature_histogram'), height = gl$plotly_height) %>% withSpinner(type = 8),
        DTOutput(ns('feature_histogram_table'))
      ),
      tabPanel(
        value = 'output_item_details',
        title = lang$tab_output_item_details,
        DTOutput(ns('item_details_table')) %>% withSpinner(type = 8)
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
