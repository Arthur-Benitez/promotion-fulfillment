
require(shiny)
library(RODBC)
ch <- 

shinyServer(function(input, output, session){
  
  r <- reactiveValues(
    ch = NULL,
    is_open = FALSE,
    auth_trigger = 0,
    query_result = NULL,
    final_result = NULL,
    items_file = NULL
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
        r$ch <- odbcDriverConnect(sprintf("Driver={Teradata};DBCName=WMG;UID=f0g00bq;AUTHENTICATION=ldap;AUTHENTICATIONPARAMETER=%s", paste0(input$user, '@@', input$password)))
        odbcGetInfo(r$ch) ## Truena si no se abrió la conexión
        r$is_open <- TRUE
        updateActionButton(session, 'auth', label = lang$logout, icon = icon('sign-out-alt'))
        flog.info('LOGGED IN')
      }, error = function(e){
        flog.warn('ERROR LOGGING IN')
      })
    } else {
      odbcClose(r$ch)
      r$is_open <- FALSE
      updateActionButton(session, 'auth', label = lang$login, icon = icon('sign-in-alt'))
      updateTextInput(session, 'user', value = '')
      updateTextInput(session, 'password', value = '')
      flog.info('LOGGED OUT')
    }
  }, ignoreInit = TRUE)
  
  ## Leer input
  observeEvent(input$items, {
    r$items_file <- input$items$datapath
  })
  items <- reactive({
    # req(input$items)
    parse_input(r$items_file, gl)
  })
  items_is_valid <- reactive({
    # req(items())
    validate_input(items(), gl)
  })
  output$input_table <- renderDT({
    validate(
      need(!is.null(r$items_file), lang$need_items_file) %then%
        need(!is.null(items()), lang$need_valid_input)
    )
    items()
  })
  
  ## Seleccionar pestaña de output para que se vea el loader
  rr <- reactiveVal(0)
  observeEvent(input$run, {
    updateTabItems(session, 'io', selected = 'output_table')
    output$output_table <- renderDT(NULL)
    rr(rr() + 1)
  })
  
  ## Correr query y análisis
  observeEvent(rr(), {
    # browser()
    query_was_tried <- FALSE
    if (!is.null(items())) {
      withProgress(min = 0, max = 1, value = 0, message = lang$running_query, expr = {
        incProgress(0.33, message = lang$running_query)
        query_was_tried <- TRUE
        r$query_result <- purrr::safely(run_query)(r$ch, items())$result
        incProgress(0.33, message = lang$running_computations)
        r$final_result <- purrr::safely(perform_computations)(r$query_result)$result
      })
    }
    output$output_table <- renderDT({
      validate(
        need(r$is_open, lang$need_auth) %then%
          need(!is.null(r$items_file), lang$need_items_file) %then%
          need(!is.null(items()), lang$need_valid_input) %then%
          need(query_was_tried, lang$need_run) %then%
          need(!is.null(r$query_result), lang$need_query_result) %then%
          need(!is.null(r$final_result), lang$need_final_result)
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
  }, ignoreNULL = TRUE)
  
  ## Reset
  observeEvent(input$reset, {
    ## Esto es necesario porque al resetear la UI de input$items, no cambia el datapath
    r$items_file <- NULL
    r$query_result <- NULL
    r$final_result <- NULL
  })
  
  ## Download
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
})

