
require(shiny)
library(RODBC)


shinyServer(function(input, output, session){
  
  ## Logging
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
    fileInput('items', label = lang$items, buttonLabel = lang$browse, placeholder = lang$browse_empty)
  })
  
  output$auth_ui <- renderUI({
    actionButton('auth', label = lang$login, icon = icon('sign-out-alt'),
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
        actionButton('auth', label = lang$login, icon = icon('sign-out-alt'),
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
    val <- parse_input(r$items_file, gl, input$date_format)
    if (!is.data.frame(val)) {
      shinyalert("Error", val, type = "error", closeOnEsc = TRUE, closeOnClickOutside = TRUE)
      r$items <- NULL
    } else {
      r$items <- val
    }
  })
  output$input_table <- renderDT({
    shiny::validate(
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
    # browser()
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
    choices <- r$items %>% 
      pull(feature_name) %>%
      unique() %>% 
      sort()
    selectInput(
      inputId = 'output_feature_select',
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
    shiny::validate(
      shiny::need(r$is_open || gl$app_deployment_environment == 'prod', lang$need_auth) %then%
        shiny::need(!is.null(r$items_file), lang$need_items_file) %then%
        shiny::need(!is.null(r$items), lang$need_valid_input) %then%
        shiny::need(r$query_was_tried, lang$need_run) %then%
        shiny::need(!is.null(r$query_result), lang$need_query_result) %then%
        shiny::need(!is.null(r$final_result), lang$need_final_result) %then%
        shiny::need(!is.null(hd$histogram_data), lang$need_final_result)
    )
    percent_columns <- c('p_stores')
    decimal_columns <- str_subset(names(hd$histogram_data), '^(n|total|avg)_')
    hd$histogram_data %>%
      mutate_at(vars(percent_columns), funs(100 * .)) %>%
      datatable(
        filter = 'none',
        options = list(
          scrollX = TRUE,
          scrollY = '400px'
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
    downloadButton('download', label = lang$download, icon = icon('download'))
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
    downloadButton('download_summary', label = lang$download_summary, icon = icon('download'))
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
    downloadButton('download_header', label = lang$download_header, icon = icon('download'))
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
    downloadButton('download_detail', label = lang$download_detail, icon = icon('download'))
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
  
})

