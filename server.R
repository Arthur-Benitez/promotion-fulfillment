
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
  
  ## 
  rr <- reactiveVal(0)
  observeEvent(input$run, {
    updateTabItems(session, 'io', selected = 'output_table')
    output$output_table <- renderDT(NULL)
    rr(rr() + 1)
  })
  observeEvent(rr(), {
    browser()
    tb <- tryCatch({
      sqlQuery(r$ch, 'select top 10 * from mx_cf_vm.calendar_day')
    }, error = function(e){
      NULL
    })
    if (is.null(tb)) {
      show(id = 'error-query', anim = TRUE, animType = 'fade')
      delay(4000, hide(id = 'error-query', anim = TRUE, animType = 'fade'))
    } else {
      tb_fin <- tryCatch({
        perform_computations(tb)
      }, error = function(e){
        show(id = 'error-comp', anim = TRUE, animType = 'fade')
        delay(4000, hide(id = 'error-comp', anim = TRUE, animType = 'fade'))
        NULL
      })
      if (!is.null(tb_fin)) {
        output$output_table <- renderDT({
          datatable(
            tb,
            filter = 'top',
            options = list(
              scrollX = TRUE,
              scrollY = '400px'
            )
          )
        })
      }
    }
  })
  
})

