
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
      flog.info('LOGGED OUT')
    }
  }, ignoreInit = TRUE)
  
  ## Leer input
  observeEvent(input$items, {
    r$items_file <- input$items$datapath
  })
  items <- reactive({
    # req(input$items)
    parse_input(r$items_file)
  })
  items_is_valid <- reactive({
    # req(items())
    validate_input(items(), gl$cols)
  })
  output$input_table <- renderDT({
    validate(
      need(!is.null(r$items_file), lang$need_items_file) %then%
        need(is.data.frame(items()), lang$need_data_frame) %then%
        need(items_is_valid(), lang$need_valid_input)
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
    if (items_is_valid()) {
      r$query_result <- tryCatch({
        query_was_tried <- TRUE
        sqlQuery(r$ch, 'select top 10 * from mx_cf_vm.calendar_day') %>% 
          withProgress(min = 0, max = 1, value = 1)
      }, error = function(e){
        NULL
      })
      r$final_result <- tryCatch({
        perform_computations(r$query_result) %>% 
          withProgress(min = 0, max = 1, value = 1)
      }, error = function(e){
        NULL
      })
    }
    output$output_table <- renderDT({
      validate(
        need(r$is_open, lang$need_auth) %then%
          need(!is.null(r$items_file), lang$need_items_file) %then%
          need(items_is_valid(), lang$need_valid_input) %then%
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
})

