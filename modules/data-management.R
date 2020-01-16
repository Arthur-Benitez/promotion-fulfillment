
# Funciones
update_rrp_info <- function(ch = NULL, credentials, connector = 'production-connector') {
  query <- readLines('sql/item_rrp_sync.sql') %>% 
    paste(collapse = '\n')
  tryCatch({
    flog.info(toJSON(list(
      session_info = msg_cred(credentials),
      message = 'START UPDATING RRP INFO',
      details = list()
    )))
    shinyalert(
      title = 'Actualizando base de datos de RRP y Sync status...',
      text = '',
      type = 'info',
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE
    )
    res <- sql_query(
      ch = ch,
      connector =  connector,
      query = query,
      stringsAsFactors = FALSE
    )
    backup_file(
      res,
      folder = dirname(gl$rrp_sync_database),
      complete_file_name = basename(gl$rrp_sync_database),
      preserved_backups = gl$rrp_sync_backups_n
    )
    saveRDS(res, gl$rrp_sync_database)
    flog.info(toJSON(list(
      session_info = msg_cred(credentials),
      message = 'DONE UPDATING RRP INFO',
      details = list()
    )))
    shinyalert(
      title = 'Base de datos de RRP y Sync status actualizada',
      text = '',
      type = 'info',
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE
    )
  }, error = function(e){
    flog.warn(toJSON(list(
      session_info = msg_cred(credentials),
      message = 'FAILED TO UPDATE RRP INFO',
      details = list()
    )))
    shinyalert(
      title = 'La base de datos de RRP y Sync status NO pudo actualizarse',
      text = '',
      type = 'error',
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE
    )
  })
}

## UI
dataManagementUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns('data_management'))
}

## Server
dataManagementServer <- function(input, output, session, credentials) {
  output$data_management <- shiny::renderUI({
    ns <- session$ns
    if (!('owner' %in% credentials()$role)) {
      data_panel <- NULL
    } else {
      if (gl$is_dev) {
        login <- shiny::tagList(
          shiny::textInput(ns('db2_user'), lang$user),
          shiny::passwordInput(ns('db2_password'), lang$db2_password)
        )
      } else {
        login <- NULL
      }
      data_panel <- shiny::tagList(
        h3(lang$rrp_sync_info),
        login,
        shiny::actionButton(ns('update_rrp'), lang$update_rrp, icon = icon('redo-alt'))
      )
    }
    data_panel
  })
  
  shiny::observeEvent(eventExpr = input$update_rrp, handlerExpr = {
    req(!gl$is_dev)
    if (file.exists(gl$rrp_sync_database)) {
      last_updated <- file.info(gl$rrp_sync_database)$mtime
    } else {
      last_updated <- as.POSIXct('1900-01-01 00:00:00')
    }
    current_time <- Sys.time()
    elapsed_days <- as.numeric(difftime(current_time, last_updated, units = 'days'))
    flog.info(toJSON(list(
      session_info = msg_cred(credentials()),
      message = 'CHECKING AGE OF RRP INFO FILE',
      details = list(
        last_updated = last_updated,
        timestamp = current_time,
        elapsed_days = elapsed_days
      )
    )))
    if (elapsed_days > gl$rrp_sync_update_period) {
      flog.info(toJSON(list(
        session_info = msg_cred(credentials()),
        message = 'TRIGGERING RRP INFO UPDATE AUTOMATICALLY',
        details = list(
          last_updated = last_updated,
          timestamp = current_time,
          elapsed_days = elapsed_days
        )
      )))
      update_rrp_info(ch = NULL, credentials(), 'db2-production-connector')
    }
  }, ignoreNULL = FALSE, once = TRUE)
  
  shiny::observeEvent(input$update_rrp, {
    ns <- session$ns
    if (gl$is_dev) {
      shinyalert(
        type = 'info',
        title = 'Iniciando sesión en DB2...',
        closeOnEsc = FALSE,
        closeOnClickOutside = FALSE,
        showCancelButton = FALSE,
        showConfirmButton = FALSE
      )
      flog.info(toJSON(list(
        session_info = msg_cred(credentials()),
        message = 'ATTEMPTING TO LOG IN INTO DB2',
        details = list(
          user = input$db2_user
        )
      )))
      tryCatch({
        ch <- odbcConnect("DSN5", uid = input$db2_user, pwd = input$db2_password)
        odbcGetInfo(ch) ## Truena si no se abrió la conexión
        flog.info(toJSON(list(
          session_info = msg_cred(credentials()),
          message = 'DB2 LOGIN SUCCESSFUL',
          details = list(
            user = input$db2_user
          )
        )))
        # Run query & update file
        flog.info(toJSON(list(
          session_info = msg_cred(credentials()),
          message = 'TRIGGERING RRP INFO DEV UPDATE MANUALLY',
          details = list()
        )))
        update_rrp_info(ch, credentials(), connector = NULL)
        odbcClose(ch)
        flog.info(toJSON(list(
          session_info = msg_cred(credentials()),
          message = 'DB2 LOGOUT SUCCESSFUL',
          details = list(
            user = input$db2_user
          )
        )))
        updateTextInput(session, 'db2_user', value = '')
        updateTextInput(session, 'db2_password', value = '')
      }, error = function(e){
        shinyalert(
          type = "error",
          title = lang$error,
          text = "El usuario o la contraseña no son válidos",
          closeOnClickOutside = TRUE
        )
        flog.warn(toJSON(list(
          session_info = msg_cred(credentials()),
          message = 'DB2 LOGIN FAILED',
          details = list(
            user = input$db2_user
          )
        )))
        updateTextInput(session, 'db2_password', value = '')
      })
    } else {
      flog.info(toJSON(list(
        session_info = msg_cred(credentials()),
        message = 'TRIGGERING RRP INFO PROD UPDATE MANUALLY',
        details = list()
      )))
      update_rrp_info(ch = NULL, credentials(), 'db2-production-connector')      
    }
  })
}
