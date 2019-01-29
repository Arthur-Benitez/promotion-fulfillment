
require(shiny)
library(RODBC)
ch <- 

shinyServer(function(input, output, session){
  
  r <- reactiveValues(
    ch = NULL
  )
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
  
  ## Validar input
  items <- reactive({
    parse_input(input$items$datapath)
  })
  
  ## Correr análisis
  rr <- reactiveVal(0)
  observeEvent(input$run, {
    updateTabItems(session, 'io', selected = 'output_table')
    output$output_table <- renderDT(NULL)
    rr(rr() + 1)
  })
  observeEvent(rr(), {
    tb <- tryCatch({
      sqlQuery(r$ch, 'select top 10 * from mx_cf_vm.calendar_day')
    }, error = function(e){
      NULL
    })
    is_valid <- validate_input(items(), gl$cols)
    tb_fin <- tryCatch({
      if (is_valid) {
        perform_computations(tb)
      } else {
        NULL
      }
    }, error = function(e){
      NULL
    })
    output$output_table <- renderDT({
      validate(
        need(!is.null(tb), 'El query falló :('),
        need(is_valid, 'El input no está en el formato correcto :('),
        need(!is.null(tb_fin), 'Los cálculos fallaron :(')
      )
      datatable(
        tb,
        filter = 'top',
        options = list(
          scrollX = TRUE,
          scrollY = '400px'
        )
      )
    })
  })
  
})

