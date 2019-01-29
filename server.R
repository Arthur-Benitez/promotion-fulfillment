
require(shiny)
library(RODBC)
ch <- 

shinyServer(function(input, output, session){
  
  r <- reactiveValues(
    ch = NULL,
    query_result = NULL,
    final_result = NULL
  )
  
  ## UI
  output$items_ui <- renderUI({
    input$reset
    fileInput('items', 'Items')
  })
  
  ## Login a Teradata
  observeEvent(input$auth, {
    is_open <- tryCatch({
      odbcGetInfo(r$ch)
      TRUE
    }, error = function(e){FALSE})
    if (is.null(r$ch) || !is_open) {
      tryCatch({
        r$ch <- odbcDriverConnect(sprintf("Driver={Teradata};DBCName=WMG;UID=f0g00bq;AUTHENTICATION=ldap;AUTHENTICATIONPARAMETER=%s", paste0(input$user, '@@', input$password)))
        updateActionButton(session, 'auth', label = 'Logout', icon = icon('sign-out-alt'))
        flog.info('LOGGED IN')
      }, error = function(e){
        flog.warn('ERROR LOGGING IN')
      })
      
    } else {
      odbcClose(r$ch)
      updateActionButton(session, 'auth', label = 'Login', icon = icon('sign-in-alt'))
      flog.info('LOGGED OUT')
    }
  })
  
  ## Leer input
  items <- reactive({
    # req(input$items)
    # input$reset
    parse_input(input$items$datapath)
  })
  items_is_valid <- reactive({
    # req(items())
    validate_input(items(), gl$cols)
  })
  output$input_table <- renderDT({
    validate(
      need(is.data.frame(items()), 'No pudimos leer el archivo de entrada :('),
      need(nrow(items()) > 0, 'No hay items para procesar')
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
    if (items_is_valid()) {
      r$query_result <- tryCatch({
        sqlQuery(r$ch, 'select top 10 * from mx_cf_vm.calendar_day') %>% 
          withProgress(min = 0, max = 1, value = 1)
      }, error = function(e){
        NULL
      })
      r$final_result <- tryCatch({
        perform_computations(r$query_result)
      }, error = function(e){
        NULL
      })
    }
    output$output_table <- renderDT({
      validate(
        need(!is.null(r$query_result), 'El query falló :('),
        need(items_is_valid(), 'El input no está en el formato correcto :('),
        need(!is.null(r$final_result), 'Los cálculos fallaron :(')
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
  }, ignoreInit = TRUE)
})

