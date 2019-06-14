

# Funciones ---------------------------------------------------------------

## Generar espeficicación de columnas para readr
generate_cols_spec <- function(columns, types, date_format = '%Y-%m-%d') {
  cs <- cols(.default = col_character())
  for (i in seq_along(columns)) {
    cs$cols[[columns[i]]] <- switch(
      types[i],
      'integer' = col_integer(),
      'numeric' = col_double(),
      'character' = col_character(),
      'date' = col_date(format = date_format)
    )
  }
  cs
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
parse_input <- function(input_file, gl, calendar_day, ch = NULL, date_format = '%Y-%m-%d') {
  tryCatch({
    nms <- names(read_csv(input_file, n_max = 0))
    if (!all(gl$cols$name %in% nms)) {
      return(sprintf('Las siguientes columnas faltan en el archivo de entrada: %s', paste(setdiff(gl$cols$name, nms), collapse = ', ')))
    }
    x <- read_csv(
      file = input_file,
      col_names = TRUE,
      col_types = generate_cols_spec(gl$cols$name, gl$cols$type, date_format = date_format)
    ) %>% 
      .[gl$cols$name] %>% 
      mutate(
        fcst_or_sales = toupper(fcst_or_sales),
        display_key = paste(dept_nbr, old_nbr, negocio, sep = '.'),
        split_var = paste(semana_ini, semana_fin, fcst_or_sales, sep = '-')
      )
    val <- validate_input(x, gl = gl, calendar_day = calendar_day, ch = ch)
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
validate_input <- function(data, gl, calendar_day, ch) {
  if (
    ## Condiciones básicas
    !is.data.frame(data) ||
    nrow(data) == 0 ||
    length(setdiff(gl$cols$name, names(data))) > 0
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
        'No puede haber valores faltantes (blanks). Esto se debe comúnmente a que la fecha está en un formato incorrecto',
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


## Correr query
prepare_query <- function(query, keys, old_nbrs, wk_inicio, wk_final) {
  query %>% 
    str_replace_all('\\?KEY', paste0("'", paste(keys, collapse = "','"), "'")) %>%
    str_replace_all('\\?OLD_NBRS', paste(old_nbrs, collapse = ",")) %>%
    str_replace_all('\\?WK_INICIO', as.character(wk_inicio)) %>% 
    str_replace_all('\\?WK_FINAL', as.character(wk_final)) %>% 
    str_replace_all('[^[:ascii:]]', '') %>% # quitar no ASCII porque truena en producción
    paste(collapse = '\n')
}
run_query_once <- function(ch, input_data) {
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
    if (is.null(ch)) {
      res <- mlutils::dataset.load(name = 'production-connector', query = query)
    } else {
      res <- sqlQuery(ch, query)
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
run_query <- function(ch, input_data) {
  res <- input_data %>% 
    split(., .$split_var) %>% 
    map(safely(function(x){
      run_query_once(ch, x)
    })) %>% 
    map('result') %>% 
    discard(is.null)
  if (length(res) > 0) {
    res <- bind_rows(res)
  } else {
    res <- NULL
  }
  res
}

## Query para buscar los SS actuales
search_ss <- function(ch, input_data_ss){
  query_ss <- readLines('sql/ss-item-str.sql') %>%
    str_replace_all('\\?OLD_NBRS', paste(unique(input_data_ss$old_nbr), collapse = ",")) %>%
    str_replace_all('\\?NEGOCIOS', paste(unique(input_data_ss$negocio), collapse = "','")) %>%
    paste(collapse = '\n')
  
  tryCatch({
    if (is.null(ch)) {
      query_ss_res <- mlutils::dataset.load(name = 'production-connector', query = query_ss)
    } else {
      query_ss_res <- sqlQuery(ch, query_ss, stringsAsFactors = FALSE)
    }
    query_ss_res <- query_ss_res %>% 
      as_tibble() %>% 
      set_names(tolower(names(.))) %>% 
      mutate_if(is.factor, as.character)
  }, error = function(e){
    NULL
  })
}

## Determinar nuevo SS ganador en cantidad
compare_ss_qty <- function(sspress_tot, sscov_tot, min_ss, max_ss){
  pmin(pmax(sspress_tot, sscov_tot, min_ss), max_ss)
}

## Determinar nuevo SS ganador en nombre
compare_ss_name <- function(sspress_tot, sscov_tot, min_ss, max_ss, sspress, base_press, sscov, sstemp, win_qty){
  win_ss = case_when(
        win_qty == max_ss ~ "MAX_SS",
        win_qty == min_ss ~ "MIN_SS",
       (win_qty == sspress_tot & sspress == 0) ~ "BASE_PRESS",
       (win_qty == sspress_tot & base_press == 0) ~ "SS_PRESS",
        win_qty == sspress_tot ~ "SS_PRESS_Tot",
       (win_qty == sscov_tot & sscov == 0) ~ "SSTEMP",
       (win_qty == sscov_tot & sstemp == 0) ~ "SSCOV",
        win_qty == sscov_tot ~ "SS_COV_Tot"
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
perform_computations <- function(data, data_ss = NULL, min_feature_qty_toggle = 'none') {
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
  
  if(is.null(data_ss)){
    data <- data %>%
      mutate(
        impact_qty = NA,
        impact_cost = NA
      )
  } else {
    ##Transformaciones para el impacto del SS
    data <- data %>% 
      left_join(data_ss, by = c("old_nbr", "store_nbr")) %>%
      replace_na(list(ganador = "Unknown", max_ss = 999999999)) %>%
      mutate_at(vars(contains("ss")), list(~round(replace_na(., 0),digits = 0))) %>%
      mutate(
        new_sspress_tot = feature_qty_fin + base_press,
        ss_winner_qty = compare_ss_qty(new_sspress_tot, sscov_tot, min_ss, max_ss),
        ss_winner_name = compare_ss_name(new_sspress_tot, sscov_tot, min_ss, max_ss, feature_qty_fin, base_press, sscov, sstemp, ss_winner_qty),
        impact_qty = ss_winner_qty - ss_ganador,
        impact_cost = impact_qty * cost,
        impact_ddv = impact_qty / avg_dly_pos_or_fcst,
        impact_vnpk = impact_qty / vnpk_qty
      )
  }
  return(data)
}

## Tabla de resumen
summarise_data <- function(data, group = c('feature_name', 'cid')) {
  ## Checks
  stopifnot(is.null(group) || all(group %in% c('feature_name', 'store_nbr', 'cid')))
  ## Cambios a combinaciones específicas
  if ('cid' %in% group) {
    group <- c(group, 'old_nbr')
  }
  if (is.null(group)) {
    group <- 'feature_name'
    data <- data %>% 
      mutate(feature_name = 'Total')
  }
  ## Grupos de tabla de salida
  group_order <- c('feature_name', 'store_nbr', 'cid', 'old_nbr')
  grp <- group_order[group_order %in% group]
  ## Variables numéricas de tabla de salida
  vv <- c('avg_dly_sales', 'avg_dly_forecast', 'min_feature_qty', 'max_feature_qty', 'total_cost', 'total_impact_cost', 'total_qty', 'total_impact_qty', 'total_ddv', 'total_impact_ddv', 'total_vnpk', 'total_impact_vnpk')
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
      avg_dly_sales = ifelse(any(fcst_or_sales == 'S'), sum(avg_dly_pos_or_fcst[fcst_or_sales=='S']), NA_real_),
      avg_dly_forecast = ifelse(any(fcst_or_sales == 'F'), sum(avg_dly_pos_or_fcst[fcst_or_sales=='F']), NA_real_),
      min_feature_qty = mean(min_feature_qty),
      max_feature_qty = mean(max_feature_qty),
      total_cost = sum(store_cost),
      total_impact_cost = sum(impact_cost),
      total_qty = sum(feature_qty_fin),
      total_impact_qty = sum(impact_qty),
      total_ddv = sum(feature_qty_fin) / sum(avg_dly_pos_or_fcst),
      total_impact_ddv = sum(impact_qty) / sum(avg_dly_pos_or_fcst),
      total_vnpk = sum(vnpk_fin),
      total_impact_vnpk = sum(impact_vnpk)
    ) %>% 
    ungroup() %>% 
    arrange(!!!syms(grp)) %>% 
    select(!!!syms(grp), !!!syms(val_vars))
  
  return(data_summary)
}

## Tabla de histograma
generate_histogram_data <- function(output_filtered_data, bin_size = 0.2) {
  res <- output_filtered_data %>% 
    summarise_data(group = c('feature_name', 'store_nbr')) %>% 
    ungroup() %>% 
    mutate(
      perc_max_feature_qty = round(total_qty / max_feature_qty, 5)
    )
  
  max_bin <- bin_size * ceiling(max(res$perc_max_feature_qty) / bin_size)
  cut_values <- seq(0, max_bin, by = bin_size)
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
      temp_qty = total_qty
    ) %>% 
    group_by(perc_max_feature_qty_bin) %>% 
    summarise(
      n_stores = n(),
      total_cost = sum(temp_cost),
      avg_store_cost = mean(temp_cost),
      total_qty = sum(temp_qty),
      avg_store_qty = mean(temp_qty),
      fcst_or_sales = ifelse(any(is.na(avg_dly_sales)), "F", "S"),
      avg_store_dly_pos_or_fcst = mean(coalesce(avg_dly_sales, avg_dly_forecast)),
      min_feature_qty = mean(min_feature_qty),
      max_feature_qty = mean(max_feature_qty),
      total_ddv = sum(temp_qty) / sum(coalesce(avg_dly_sales, avg_dly_forecast))
    ) %>% 
    ungroup() %>% 
    mutate(
      p_stores = n_stores / sum(n_stores)
    ) %>% 
    right_join(tibble(perc_max_feature_qty_bin = factor(cut_labels, levels = cut_labels)), by = 'perc_max_feature_qty_bin') %>% 
    replace(., is.na(.), 0) %>% 
    select(perc_max_feature_qty_bin, n_stores, p_stores, everything())
}

## Generar el nombre de la promo para GRS
generate_promo_name <- function(dept_nbr, user, feature_name) {
  sprintf('MX_D%d_CM_%s_%s', dept_nbr, toupper(user), feature_name)
}

## Generar el id de tienda en formato para GRS
generate_loc_id <- function(store_nbr) {
  sprintf('MX_WMT_ST_%s', str_pad(store_nbr, 5, 'left', '0'))
}

## Generar el HEADER.csv para cargar al sistema
generate_header <- function(input_data) {
  input_data %>% 
    transmute(
      `*Promotion` = generate_promo_name(dept_nbr, user, feature_name),
      Description = '',
      StartDate,
      EndDate,
      ApprovedSw = 'TRUE',
      AdditiveSw = 'TRUE',
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

# Server ------------------------------------------------------------------

computePromotionsServer <- function(input, output, session, credentials) {
  
  ## Calendario para validar inputs
  calendar_day <- read_tsv('data/calendar-day.tsv')
  
  ## Valores reactivos para usar en observadores
  r <- reactiveValues(
    ch = NULL,
    is_open = FALSE,
    auth_trigger = 0,
    items_file = NULL,
    items = NULL,
    query_was_tried = FALSE
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
          message = 'USER LOGIN FAILED',
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
        message = 'USER LOGOUT SUCCESSFUL',
        details = list(
          user = input$user
        )
      )))
    }
  }, ignoreInit = TRUE)
  
  ## Leer input
  observeEvent(input$items, {
    r$items_file <- input$items$datapath
  })
  observe({
    req(r$items_file, input$date_format)
    req(r$is_open || gl$app_deployment_environment == 'prod')
    flog.info(toJSON(list(
      session_info = msg_cred(credentials()),
      message = 'PARSING INPUT FILE',
      details = list(
        file = r$items_file
      )
    )))
    val <- parse_input(r$items_file, gl = gl, calendar_day = calendar_day, ch = r$ch, date_format = input$date_format)
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
  output$input_table <- renderDT({
    shiny::validate(
      shiny::need(r$is_open || gl$app_deployment_environment == 'prod', lang$need_auth) %then%
        shiny::need(!is.null(r$items_file), lang$need_items_file) %then%
        shiny::need(!is.null(r$items), lang$need_valid_input)
    )
    r$items
  }, options = list(
    filter = 'top',
    scrollX = TRUE,
    scrollY = '400px'
  ))
  
  ## Seleccionar pestaña de output para que se vea el loader
  rr <- reactiveVal(0)
  observeEvent(input$run, {
    ns <- session$ns
    if (is.null(r$items)) {
      r$query_was_tried <- FALSE
    } else {
      updateTabItems(session, 'io', selected = 'output_summary')
      output$output_table <- renderDT(NULL)
      r$query_was_tried <- TRUE
      rr(rr() + 1)
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
      text = sprintf('Hora de inicio: %s', format(time1, "%X")),
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
      if (is_dev) {
        ## Las conexiones no se pueden exportar a otros procesos de R, así que se tiene que generar una nueva conexión
        ## Ver: https://cran.r-project.org/web/packages/future/vignettes/future-4-issues.html
        future_ch <- RODBC::odbcDriverConnect(sprintf("Driver={Teradata};DBCName=WM3;AUTHENTICATION=ldap;AUTHENTICATIONPARAMETER=%s", paste0(usr, '@@', pwd)))
      } else {
        future_ch <- NULL
      }
      list(
        timestamp = time1,
        data = run_query(future_ch, items),
        data_ss = search_ss(future_ch, items)
      )
    }) %...>% 
      query_result()
  }, ignoreInit = TRUE)
  
  ## Hacer cálculos
  final_result <- reactive({
    req(query_result()$data)
    flog.info(toJSON(list(
      session_info = msg_cred(isolate(credentials())),
      message = 'PERFORMING COMPUTATIONS',
      details = list()
    )))
    feature_info <- get_empty_features(query_result()$data, isolate(r$items))
    good_features <- with(feature_info, feature_name[!is_empty])
    empty_features <- with(feature_info, feature_name[is_empty])
    alert_info <- alert_param(good_features, empty_features, query_result()$timestamp)
    shinyalert::shinyalert(
      type = alert_info$type,
      title = alert_info$title,
      text = alert_info$text,
      closeOnClickOutside = TRUE,
      timer = 10000,
      animation = "slide-from-top"
    )
    flog.info(toJSON(list(
      session_info = msg_cred(isolate(credentials())),
      message = alert_info$message,
      details = list()
    )))
    if (length(good_features) > 0) {
      good_data <- query_result()$data %>% 
        filter(feature_name %in% good_features)
      purrr::safely(perform_computations)(good_data, query_result()$data_ss, input$min_feature_qty_toggle)$result
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
      shiny::need(!is.null(query_result()$data), lang$need_query_result) %then%
      shiny::need(!is.null(final_result()), lang$need_final_result)
  })
  need_histogram_ready <- reactive({
    shiny::need(!is.null(histogram_data()), lang$need_final_result) %then%
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
      decimal_columns <- c('avg_dly_pos_or_fcst', 'feature_qty_req_min',	'feature_qty_req', 'feature_ddv_req', 'feature_qty_pre', 'feature_ddv_pre', 'feature_qty_pre_tot', 'feature_ddv_fin', 'feature_qty_fin', 'display_key', 'store_cost', 'vnpk_fin', 'cost')
      final_result() %>%
        mutate_at(vars(percent_columns), list(~100 * .)) %>%
        datatable(
          filter = 'top',
          options = list(
            scrollX = TRUE,
            scrollY = '400px'
          )
        ) %>%
        formatCurrency(columns = decimal_columns, digits = 1, currency = '') %>%
        formatCurrency(columns = percent_columns, digits = 1, currency = '%', before = FALSE)
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
      datatable(
        summary_table(),
        filter = 'top',
        options = list(
          scrollX = TRUE,
          scrollY = '400px'
        )
      ) %>%
        formatCurrency(columns = str_subset(names(summary_table()), '^(total|avg)_'), digits = 1, currency = '')
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
  histogram_data <- reactive({
    req(final_results_filt())
    generate_histogram_data(final_results_filt(), bin_size = input$feature_histogram_bin_size)
  })
  
  ## Histograma de alcance
  output$feature_histogram <- renderPlotly({
    shiny::validate(
      need_input_ready() %then%
        need_query_ready() %then%
        need_histogram_ready()
    )
    tryCatch({
      mfq <- unique(final_results_filt()$max_feature_qty)
      histogram_data() %>% 
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
  
  ## Tabla de alcance (output)
  output$feature_histogram_table <- renderDT({
    needs <- need_input_ready() %then%
      need_query_ready() %then%
      need_histogram_ready()
    shiny::validate(
       shiny::need(is.null(needs), '')
    )
    tryCatch({
      percent_columns <- c('p_stores')
      decimal_columns <- str_subset(names(histogram_data()), '^(n|total|avg)_')
      histogram_data() %>%
        mutate_at(vars(percent_columns), list(~100 * .)) %>%
        datatable(
          filter = 'none',
          options = list(
            scrollX = TRUE,
            scrollY = '200px'
          )
        ) %>%
        formatCurrency(columns = decimal_columns, digits = 1, currency = '') %>%
        formatCurrency(columns = percent_columns, digits = 1, currency = '%', before = FALSE)
    }, error = function(e){
      NULL
    })
  })
  
  ## Reset
  observeEvent(input$reset, {
    ## Esto es necesario porque al resetear la UI de input$items, no cambia el datapath
    r$items_file <- NULL
    r$items <- NULL
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
      write_excel_csv(summary_table(), path = file, na = '')
    },
    contentType = 'text/csv'
  )
  
  ## Descargar template
  output$download_template <- downloadHandler(
    filename = 'promo-fulfillment-template.csv',
    content = function(file) {
      x <- generate_sample_input(calendar_day)
      write_excel_csv(x, file)
    },
    contentType = 'text/csv'
  )
  
  ## Mostrar instrucciones
  eval(parse(file = 'html/instructions-table.R', encoding = 'UTF-8'))
  output$instructions_table <- renderTable({
    instructions_table
  })
  observeEvent(input$show_instructions, {
    showModal(modalDialog(
      size = 'l',
      easyClose = TRUE,
      title = 'Instrucciones',
      includeHTML('html/instructions.html'),
      uiOutput(session$ns('instructions_table')),
      footer = modalButton(lang$ok)
    ))
  }, ignoreInit = TRUE)
  
  ## Descargar HEADER
  output$download_header_ui <- renderUI({
    req(r$items)
    ns <- session$ns
    downloadButton(ns('download_header'), label = lang$download_header, icon = icon('download'))
  })
  output$download_header <- downloadHandler(
    filename = sprintf('HEADER_%s.csv', Sys.Date()),
    content = function(file) {
      r$items %>% 
        generate_header() %>% 
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
      final_result() %>% 
        generate_detail() %>% 
        write_excel_csv(path = file, na = '')
    },
    contentType = 'text/csv'
  )
  
}


# UI ----------------------------------------------------------------------

computePromotionsUI <- function(id) {
  ns <- shiny::NS(id)
  
  if (gl$app_deployment_environment == 'dev') {
    login <- tagList(
      textInput(ns('user'), lang$user),
      passwordInput(ns('password'), lang$password),
      uiOutput(ns('auth_ui')),
      tags$hr()
    )
  } else {
    login <- NULL
  }
  
  fluidRow(
    box(
      width = 2,
      login,
      selectInput(ns('date_format'), lang$date_format, c('yyyy-mm-dd' = '%Y-%m-%d',
                                                         'dd/mm/yyyy' = '%d/%m/%Y',
                                                         'mm/dd/yyyy' = '%m/%d/%Y')),
      uiOutput(ns('items_ui')),
      tags$div(
        class = 'input-margin',
        actionButton(ns('show_instructions'), lang$show_instructions, icon = icon('question-circle')),
        downloadButton(ns('download_template'), lang$download_template, icon = icon('download'))
      ),
      tags$div(
        class = 'input-margin',
        actionButton(ns('run'), lang$run, icon = icon('play')),
        actionButton(ns('reset'), lang$reset, icon = icon('redo-alt'))
      ),
      radioButtons(
        ns('min_feature_qty_toggle'),
        label = lang$min_feature_qty_toggle,
        choices = c('none', 'round_down', 'round_up') %>%
          set_names(c(lang$toggle_none, lang$toggle_round_down, lang$toggle_round_up))
      )
    ),
    tabBox(
      id = ns('io'),
      selected = NULL,
      width = 10,
      tabPanel(
        value = 'input_table',
        title = lang$tab_input,
        DTOutput(ns('input_table'))
      ),
      tabPanel(
        value = 'output_summary',
        title = lang$tab_output_summary,
        tags$div(
          class = 'inline-inputs',
          checkboxGroupInput(
            ns('summary_groups'),
            label = lang$summary_groups,
            choices = c('feature_name', 'cid', 'store_nbr') %>% 
              set_names(c(lang$feature_name, lang$cid, lang$store_nbr)),
            selected = c('feature_name', 'cid'),
            inline = TRUE
          ),
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
        DTOutput(ns('summary_table')) %>% withSpinner(type = 8)
      ),
      tabPanel(
        value = 'output_histogram',
        title = lang$tab_output_histogram,
        tags$div(
          class = 'inline-inputs',
          tags$div(
            class = 'input-margin',
            uiOutput(ns('output_feature_select_ui'))
          ),
          tags$div(
            class = 'input-margin',
            sliderInput(ns('feature_histogram_bin_size'), lang$bin_size,
                        min = 0.05, max = 1, value = 0.2, step = 0.05)
          )
        ),
        plotlyOutput(ns('feature_histogram')) %>% withSpinner(type = 8),
        tags$br(),
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
