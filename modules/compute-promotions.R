

# Funciones ---------------------------------------------------------------

## Generar espeficicación de columnas para readr
generate_cols_spec <- function(columns, date_format = '%Y-%m-%d') {
  cs <- cols(.default = col_character())
  for (x in names(columns)) {
    cs$cols[[x]] <- switch(
      columns[x],
      'integer' = col_integer(),
      'numeric' = col_double(),
      'character' = col_character(),
      'date' = col_date(format = date_format)
    )
  }
  cs
}

## Leer entrada
parse_input <- function(input_file, gl, ch = NULL, date_format = '%Y-%m-%d') {
  tryCatch({
    flog.info(toJSON(list(
      message = 'PARSING ITEMS FILE',
      details = list(
        file = input_file
      )
    )))
    x <- read_csv(
      file = input_file,
      col_names = TRUE,
      col_types = generate_cols_spec(gl$cols, date_format = date_format)
    ) %>% 
      .[names(gl$cols)] %>% 
      mutate(
        fcst_or_sales = toupper(fcst_or_sales),
        display_key = paste(dept_nbr, old_nbr, negocio, sep = '.'),
        split_var = paste(semana_ini, semana_fin, fcst_or_sales, sep = '-')
      )
    val <- validate_input(x, gl, ch)
    if (isTRUE(val)) {
      flog.info(toJSON(list(
        message = 'INPUT PARSING DONE',
        details = list(
          file = input_file
        )
      )))
      return(x)
    } else {
      flog.info(toJSON(list(
        message = 'INPUT PARSING ABORTED',
        details = list(
          file = input_file,
          reason = val
        )
      )))
      return(val)
    }
  }, error = function(e){
    flog.info(toJSON(list(
      message = 'INPUT PARSING ABORTED',
      details = list(
        file = input_file,
        reason = 'Error parsing file'
      )
    )))
    return('Error leyendo el archivo')
  })
}


## Validar inputs
validate_input <- function(data, gl, ch) {
  if (
    ## Condiciones básicas
    !is.data.frame(data) ||
    nrow(data) == 0 ||
    length(setdiff(names(gl$cols), names(data))) > 0
  ) {
    return(FALSE)
  } else {
    tryCatch({
      current_wk_query <- 'select wm_yr_wk from mx_cf_vm.calendar_day where gregorian_date = current_date'
      if (is.null(ch)) {
        current_wk <- mlutils::dataset.load(name = 'WMG',
                                            query = current_wk_query)[[1]]
      } else {
        current_wk <- sqlQuery(ch, current_wk_query)[[1]]
      }
      cond <- tribble(
        ~message, ~passed,
        ## Checar que no haya valores faltantes
        'No puede haber valores faltantes (blanks)',
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
          summarise_at(gl$feature_const_cols, funs(length(unique(.)))) %>% 
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
        with(data, all(Sys.Date() <= StartDate & StartDate <= EndDate))
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
  # browser()
  query %>% 
    str_replace_all('\\?KEY', paste0("'", paste(keys, collapse = "','"), "'")) %>%
    str_replace_all('\\?OLD_NBRS', paste(old_nbrs, collapse = ",")) %>%
    str_replace_all('\\?WK_INICIO', as.character(wk_inicio)) %>% 
    str_replace_all('\\?WK_FINAL', as.character(wk_final)) %>% 
    str_replace_all('[^[:ascii:]]', '') %>% # quitar no ASCII porque truena en producción
    paste(collapse = '\n')
}
run_query_once <- function(ch, input_data) {
  # browser()
  wk_inicio <- unique(input_data$semana_ini)
  wk_final <- unique(input_data$semana_fin)
  type <- toupper(unique(input_data$fcst_or_sales))
  if (type == 'F') {
    query <- read_lines('sql/exhibiciones-fcst.sql')
    value <- sym('avg_dly_fcst')
  } else if (type == 'S') {
    query <- read_lines('sql/exhibiciones-pos.sql')
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
      res <- mlutils::dataset.load(name = 'WMG', query = query)
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
    res <- bind_rows(res) %>% 
      replace_na(list(
        avg_dly_pos_or_fcst = 0
      ))
  } else {
    res <- NULL
  }
  res
}

## Lógica en R
perform_computations <- function(data) {
  initial_columns <- names(data)
  data <- data %>% 
    group_by(feature_name, store_nbr) %>% 
    mutate(
      feature_perc_pos_or_fcst = avg_dly_pos_or_fcst / sum(avg_dly_pos_or_fcst)
    ) %>% 
    ungroup() %>% 
    mutate(
      feature_qty_req = feature_perc_pos_or_fcst * max_feature_qty,
      feature_ddv_req = feature_qty_req / avg_dly_pos_or_fcst,
      feature_ddv_fin = pmin(feature_ddv_req, max_ddv),
      feature_ddv_bound_active = ifelse(feature_ddv_req > max_ddv, 1, 0),
      feature_qty_fin = feature_ddv_fin * avg_dly_pos_or_fcst,
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
  data %>% 
    mutate_at(new_columns, funs(replace_na(., 0)))
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
  vv <- c('avg_dly_sales', 'avg_dly_forecast', 'total_cost', 'total_qty', 'max_feature_qty', 'total_ddv', 'total_vnpk')
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
      total_cost = sum(store_cost),
      total_qty = sum(feature_qty_fin),
      max_feature_qty = mean(max_feature_qty),
      total_ddv = sum(feature_qty_fin) / sum(avg_dly_pos_or_fcst),
      total_vnpk = sum(vnpk_fin)
    ) %>% 
    ungroup() %>% 
    arrange(!!!syms(grp)) %>% 
    select(!!!syms(grp), !!!syms(val_vars))
  
  return(data_summary)
}

## Tabla de histograma
generate_histogram_data <- function(output_filtered_data, cut_values = seq(0, 1, 0.2)) {
  cut_labels <- paste(
    scales::percent(head(cut_values, -1)),
    scales::percent(cut_values[-1]),
    sep = ' - '
  )
  output_filtered_data %>% 
    summarise_data(group = c('feature_name', 'store_nbr')) %>% 
    ungroup() %>% 
    mutate(
      perc_max_feature_qty = round(total_qty / max_feature_qty, 5),
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
      max_feature_qty = mean(max_feature_qty),
      total_ddv = sum(temp_qty) / sum(coalesce(avg_dly_sales, avg_dly_forecast))
    ) %>% 
    ungroup() %>% 
    mutate(
      p_stores = n_stores / sum(n_stores)
    ) %>% 
    right_join(tibble(perc_max_feature_qty_bin = factor(cut_labels)), by = 'perc_max_feature_qty_bin') %>% 
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
generate_header <- function(input_data, priority = 15) {
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
      Priority = priority,
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

computePromotionsServer <- function(input, output, session) {
  ## Logging
  if (!dir.exists(paste0(gl$app_deployment_environment, '/log/'))) {
    dir.create(paste0(gl$app_deployment_environment, '/log/'))
  }
  flog.logger(
    name = 'ROOT',
    threshold = INFO,
    appender = appender.tee(paste0(gl$app_deployment_environment, '/log/', as.character(Sys.Date()), '.log'))
  )
  
  ## Valores reactivos para usar en observadores
  r <- reactiveValues(
    ch = NULL,
    is_open = FALSE,
    auth_trigger = 0,
    query_result = NULL,
    final_result = NULL,
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
      tryCatch({
        flog.info(toJSON(list(
          message = 'ATTEMPTING USER LOGIN',
          details = list(
            user = input$user
          )
        )))
        r$ch <- odbcDriverConnect(sprintf("Driver={Teradata};DBCName=WMG;UID=f0g00bq;AUTHENTICATION=ldap;AUTHENTICATIONPARAMETER=%s", paste0(input$user, '@@', input$password)))
        odbcGetInfo(r$ch) ## Truena si no se abrió la conexión
        r$is_open <- TRUE
        output$auth_ui <- renderUI({
          actionButton('auth', label = lang$logout, icon = icon('sign-out-alt'),
                       style="color: #fff; background-color: #f42e2e;")
        })
        flog.info(toJSON(list(
          message = 'USER LOGIN SUCCESSFUL',
          details = list(
            user = input$user
          )
        )))
      }, error = function(e){
        # showModal(modalDialog(
        #   title = "Error al iniciar sesión",
        #   "El usuario o la contraseña no son válidos",
        #   footer = modalButton("Aceptar")
        # ))
        shinyalert("Error", "El usuario o la contraseña no son válidos", type = "error")
        flog.info(toJSON(list(
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
    val <- parse_input(r$items_file, gl, r$ch, input$date_format)
    if (!is.data.frame(val)) {
      shinyalert("Error", val, type = "error", closeOnEsc = TRUE, closeOnClickOutside = TRUE)
      r$items <- NULL
    } else {
      r$items <- val
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
    updateTabItems(session, 'io', selected = 'output_summary')
    output$output_table <- renderDT(NULL)
    rr(rr() + 1)
  })
  
  ## Correr query y análisis
  observeEvent(rr(), {
    r$query_was_tried <- FALSE
    if (!is.null(r$items)) {
      withProgress(min = 0, max = 1, value = 0, message = lang$running_query, expr = {
        incProgress(0.33, message = lang$running_query)
        r$query_was_tried <- TRUE
        flog.info(toJSON(list(
          message = 'RUNNING QUERY',
          details = list()
        )))
        r$query_result <- purrr::safely(run_query)(r$ch, r$items)$result
        incProgress(0.33, message = lang$running_computations)
        flog.info(toJSON(list(
          message = 'PERFORMING COMPUTATIONS',
          details = list()
        )))
        r$final_result <- purrr::safely(perform_computations)(r$query_result)$result
      })
    }
    
    output$output_table <- renderDT({
      shiny::validate(
        shiny::need(r$is_open || gl$app_deployment_environment == 'prod', lang$need_auth) %then%
          shiny::need(!is.null(r$items_file), lang$need_items_file) %then%
          shiny::need(!is.null(r$items), lang$need_valid_input) %then%
          shiny::need(r$query_was_tried, lang$need_run) %then%
          shiny::need(!is.null(r$query_result), lang$need_query_result) %then%
          shiny::need(!is.null(r$final_result), lang$need_final_result)
      )
      percent_columns <- c('feature_perc_pos_or_fcst')
      decimal_columns <- c('avg_dly_pos_or_fcst',	'feature_qty_req', 'feature_ddv_req','feature_ddv_fin',
                           'feature_qty_fin', 'display_key', 'store_cost', 'vnpk_fin', 'cost')
      r$final_result %>%
        mutate_at(vars(percent_columns), funs(100 * .)) %>%
        datatable(
          filter = 'top',
          options = list(
            scrollX = TRUE,
            scrollY = '400px'
          )
        ) %>%
        formatCurrency(columns = decimal_columns, digits = 1, currency = '') %>%
        formatCurrency(columns = percent_columns, digits = 1, currency = '%', before = FALSE)
    })
    #percent_columns <- c('')
  }, ignoreNULL = TRUE)
  
  observe({
    req(r$final_result)
    r$summary_table <- purrr::safely(summarise_data)(r$final_result, input$summary_groups)$result
  })
  output$summary_table <- renderDT({
    shiny::validate(
      shiny::need(r$is_open || gl$app_deployment_environment == 'prod', lang$need_auth) %then%
        shiny::need(!is.null(r$items_file), lang$need_items_file) %then%
        shiny::need(!is.null(r$items), lang$need_valid_input) %then%
        shiny::need(r$query_was_tried, lang$need_run) %then%
        shiny::need(!is.null(r$query_result), lang$need_query_result) %then%
        shiny::need(!is.null(r$final_result), lang$need_final_result)
    )
    datatable(
      r$summary_table,
      filter = 'top',
      options = list(
        scrollX = TRUE,
        scrollY = '400px'
      )
    ) %>%
      formatCurrency(columns = str_subset(names(r$summary_table), '^(total|avg)_'), digits = 1, currency = '')
  })
  
  output$output_feature_select_ui <- renderUI({
    req(r$items)
    ns <- session$ns
    choices <- r$items %>% 
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
  hd <- reactiveValues(
    final_results_filt = NULL,
    histogram_data = NULL
  )
  observe({
    req(r$final_result, input$output_feature_select)
    hd$final_results_filt <- r$final_result %>% 
      filter(feature_name == input$output_feature_select)
    hd$histogram_data <- generate_histogram_data(hd$final_results_filt)
  })
  
  ## Histograma de alcance
  output$feature_histogram <- renderPlotly({
    shiny::validate(
      shiny::need(r$is_open || gl$app_deployment_environment == 'prod', lang$need_auth) %then%
        shiny::need(!is.null(r$items_file), lang$need_items_file) %then%
        shiny::need(!is.null(r$items), lang$need_valid_input) %then%
        shiny::need(r$query_was_tried, lang$need_run) %then%
        shiny::need(!is.null(r$query_result), lang$need_query_result) %then%
        shiny::need(!is.null(r$final_result), lang$need_final_result) %then%
        shiny::need(!is.null(hd$histogram_data), lang$need_final_result) %then%
        shiny::need(nchar(input$output_feature_select) > 0, lang$need_select_feature)
    )
    mfq <- unique(hd$final_results_filt$max_feature_qty)
    hd$histogram_data %>% 
      mutate(
        label_y = n_stores + 0.03 * max(n_stores),
        label = scales::percent(p_stores),
        text = sprintf('Tiendas: %s (%s)<br>Costo total: %s<br>Costo promedio: %s<br>Cant. total: %s<br>Cant. promedio: %s', scales::comma(n_stores, digits = 0), scales::percent(p_stores), scales::comma(total_cost, digits = 0), scales::comma(avg_store_cost, digits = 0), scales::comma(total_qty, digits = 0), scales::comma(avg_store_qty, digits = 0))
      ) %>% 
      plot_ly(x = ~perc_max_feature_qty_bin, y = ~n_stores, text = ~text, type = 'bar', name = NULL) %>% 
      add_text(y = ~label_y, text = ~label, name = NULL) %>% 
      plotly::layout(
        title = 'Alcance a piezas máximas por tienda',
        xaxis = list(title = sprintf('Alcance (%% de Max. Feature Qty. = %s)', scales::comma(mfq, digits = 0))),
        yaxis = list(title = 'Número de tiendas', separators = '.,'),
        showlegend = FALSE
      )
  })
  
  ## Tabla de alcance (output)
  output$feature_histogram_table <- renderDT({
    req(
      r$is_open || gl$app_deployment_environment == 'prod',
      !is.null(r$items_file),
      !is.null(r$items),
      r$query_was_tried,
      !is.null(r$query_result),
      !is.null(r$final_result),
      !is.null(hd$histogram_data)
    )
    percent_columns <- c('p_stores')
    decimal_columns <- str_subset(names(hd$histogram_data), '^(n|total|avg)_')
    hd$histogram_data %>%
      mutate_at(vars(percent_columns), funs(100 * .)) %>%
      datatable(
        filter = 'none',
        options = list(
          scrollX = TRUE,
          scrollY = '200px'
        )
      ) %>%
      formatCurrency(columns = decimal_columns, digits = 1, currency = '') %>%
      formatCurrency(columns = percent_columns, digits = 1, currency = '%', before = FALSE)
  })
  
  ## Reset
  observeEvent(input$reset, {
    ## Esto es necesario porque al resetear la UI de input$items, no cambia el datapath
    r$items_file <- NULL
    r$items <- NULL
    r$query_result <- NULL
    r$final_result <- NULL
    r$summary_table <- NULL
    r$query_was_tried <- NULL
  })
  
  ## Descargar cálculos
  output$download_ui <- renderUI({
    req(r$final_result)
    ns <- session$ns
    downloadButton(ns('download'), label = lang$download, icon = icon('download'))
  })
  output$download <- downloadHandler(
    filename = function() {
      sprintf('estrategias_%s.csv', Sys.Date())
    },
    content = function(file) {
      write_excel_csv(r$final_result, path = file, na = '')
    },
    contentType = 'text/csv'
  )
  
  ## Descargar resumen
  output$download_summary_ui <- renderUI({
    req(r$summary_table)
    ns <- session$ns
    downloadButton(ns('download_summary'), label = lang$download_summary, icon = icon('download'))
  })
  output$download_summary <- downloadHandler(
    filename = function() {
      sprintf('resumen_%s.csv', Sys.Date())
    },
    content = function(file) {
      write_excel_csv(r$summary_table, path = file, na = '')
    },
    contentType = 'text/csv'
  )
  
  ## Descargar template
  output$download_template <- downloadHandler(
    filename = 'promo-fulfillment-template.csv',
    content = function(file) {
      file.copy('data/sample-input.csv', file)
    },
    contentType = 'text/csv'
  )
  
  ## Descargar instrucciones
  output$download_instructions <- downloadHandler(
    filename = 'promo-fulfillment-instructions.xlsx',
    content = function(file) {
      file.copy('data/instructions.xlsx', file)
    },
    contentType = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
  )
  
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
        generate_header(priority = 15) %>% 
        write_excel_csv(path = file, na = '')
    },
    contentType = 'text/csv'
  )
  
  ## Descargar DETAIL
  output$download_detail_ui <- renderUI({
    req(r$final_result)
    ns <- session$ns
    downloadButton(ns('download_detail'), label = lang$download_detail, icon = icon('download'))
  })
  output$download_detail <- downloadHandler(
    filename = sprintf('DETAIL_%s.csv', Sys.Date()),
    content = function(file) {
      r$final_result %>% 
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
      width = 3,
      login,
      selectInput(ns('date_format'), lang$date_format, c('yyyy-mm-dd' = '%Y-%m-%d',
                                                         'dd/mm/yyyy' = '%d/%m/%Y',
                                                         'mm/dd/yyyy' = '%m/%d/%Y')),
      uiOutput(ns('items_ui')),
      tags$div(
        style = 'margin-bottom: 20px;',
        downloadButton(ns('download_instructions'), lang$download_instructions, icon = icon('download')),
        downloadButton(ns('download_template'), lang$download_template, icon = icon('download'))
      ),
      actionButton(ns('run'), lang$run, icon = icon('play')),
      actionButton(ns('reset'), lang$reset, icon = icon('redo-alt'))
    ),
    tabBox(
      id = 'io',
      selected = NULL,
      width = 9,
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
        DTOutput(ns('summary_table'))
      ),
      tabPanel(
        value = 'output_histogram',
        title = lang$tab_output_histogram,
        uiOutput(ns('output_feature_select_ui')),
        plotlyOutput(ns('feature_histogram')) %>% withSpinner(type = 8),
        tags$br(),
        DTOutput(ns('feature_histogram_table'))
      ),
      tabPanel(
        value = 'output_table',
        title = lang$tab_output_table,
        uiOutput(ns('download_ui')),
        DTOutput(ns('output_table')) #%>% withSpinner(type = 8)
      )
    )
  )
}
