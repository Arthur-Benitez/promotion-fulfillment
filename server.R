
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
        updateActionButton(session, 'auth', label = lang$logout, icon = icon('sign-out-alt'))
        flog.info(toJSON(list(
          message = 'USER LOGIN SUCCESSFUL',
          details = list(
            user = input$user
          )
        )))
      }, error = function(e){
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
  observeEvent(r$items_file, {
    # req(input$items)
    flog.info(toJSON(list(
      message = 'PARSING ITEMS FILE',
      details = list(
        file = r$items_file
      )
    )))
    r$items <- parse_input(r$items_file, gl)
  }, ignoreNULL = TRUE)
  items_is_valid <- eventReactive(r$items, {
    # req(r$items)
    flog.info(toJSON(list(
      message = 'VALIDATING ITEMS FILE',
      details = list(
        file = r$items_file
      )
    )))
    validate_input(r$items, gl)
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
        r$summary_table <- purrr::safely(summarise_data)(r$final_result)$result
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
      datatable(
        r$final_result,
        filter = 'top',
        options = list(
          scrollX = TRUE,
          scrollY = '400px'
        )
      )
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
      )
    })
    }, ignoreNULL = TRUE)
  
  
  
  output$output_feature_select_ui <- renderUI({
    req(r$items)
    choices <- r$items %>% 
      select(feature_nbr, feature_name) %>%
      distinct() %>% 
      with(feature_nbr %>% set_names(paste(feature_nbr, feature_name, sep = ' - ')))
    selectInput(
      inputId = 'output_feature_select',
      label = lang$feature,
      choices = choices
    )
  })
  
  output$feature_histogram <- renderPlot({
    # req(r$final_result)
    # req(input$output_feature_select)
    # req(nchar(input$output_feature_select) > 0)
    shiny::validate(
      shiny::need(r$is_open || gl$app_deployment_environment == 'prod', lang$need_auth) %then%
        shiny::need(!is.null(r$items_file), lang$need_items_file) %then%
        shiny::need(!is.null(r$items), lang$need_valid_input) %then%
        shiny::need(r$query_was_tried, lang$need_run) %then%
        shiny::need(!is.null(r$query_result), lang$need_query_result) %then%
        shiny::need(!is.null(r$final_result), lang$need_final_result) %then%
        shiny::need(nchar(input$output_feature_select) > 0, lang$need_select_feature)
    )
    cut_values <- c(0, 0.25, 0.50, 0.75, 1.00)
    cut_labels <- paste(
      scales::percent(head(cut_values, -1)),
      scales::percent(cut_values[-1]),
      sep = ' - '
    )
    x <- r$final_result %>% 
      filter(feature_nbr == input$output_feature_select) %>% 
      group_by(feature_nbr, feature_name, store_nbr) %>% 
      summarise(
        feature_perc_qty = round(sum(feature_qty_fin), 5) / mean(max_feature_qty)
      ) %>% 
      ungroup() %>% 
      mutate(
        feature_perc_qty_bin = cut(feature_perc_qty, breaks = cut_values, labels = cut_labels, include.lowest = TRUE)
      )
    x %>% 
      group_by(feature_perc_qty_bin) %>% 
      summarise(
        n = n()
      ) %>% 
      ungroup() %>% 
      mutate(
        p = n / sum(n),
        label_y = n + 0.05 * max(n)
      ) %>% 
      ggplot(aes(feature_perc_qty_bin, n)) +
      geom_col() +
      geom_text(aes(y = label_y, label = sprintf('%s (%s)', scales::comma(n), scales::percent(p)))) +
      scale_y_continuous(labels = scales::comma) +
      theme_bw()+
      labs(
        title = 'Alcance a piezas máximas por tienda',
        x = 'Alcance (% Max. Feature Qty.)',
        y = 'Número de tiendas'
      )
  })
  ## Reset
  observeEvent(input$reset, {
    ## Esto es necesario porque al resetear la UI de input$items, no cambia el datapath
    r$items_file <- NULL
    r$items <- NULL
    r$query_result <- NULL
    r$final_result <- NULL
    r$query_was_tried <- NULL
  })
  
  ## Download results
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
  
  ## Download results
  output$download_summary_ui <- renderUI({
    req(r$summary_table)
    downloadButton('download_summary', label = lang$download, icon = icon('download'))
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
  
  ## Download template
  output$download_template <- downloadHandler(
    filename = 'promo-fulfillment-sample.xlsx',
    content = function(file) {
      file.copy('data/sample-input.xlsx', file)
    },
    contentType = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
  )
})

