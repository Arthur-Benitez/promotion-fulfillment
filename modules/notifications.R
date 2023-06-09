# Functions ---------------------------------------------------------------
save_title <- function(message_data, user_file, user_notifications_path) {
  if (!dir.exists(user_notifications_path)) {
    dir.create(user_notifications_path, recursive = TRUE)
  }
  write_csv(message_data, path = user_file, na = '', append = ifelse(file.exists(user_file), TRUE, FALSE))
}

make_modal <- function(message_path, ns){
  modalDialog(
    size = 'l',
    title = 'Anuncios',
    includeHTML(message_path),
    footer = tags$table(
      align = 'right',
      tags$tr(
        tags$td(
          id = 'notification-buttons',
          checkboxInput(ns('save'), label = 'No volver a mostrar', value = FALSE)
        ),
        tags$td(actionButton(ns('continue'), label = 'Siguiente'))
      )
    )
  )
}

# Server ------------------------------------------------------------------
notificationsServer <- function(input, output, session, credentials) {
  deploy_file <- file.path(gl$app_deployment_environment, 'notifications', 'messages', 'deploy.csv')
  user_notifications_path <- file.path(gl$app_deployment_environment, 'notifications', 'users')
  user_file <- reactive(
    sprintf('%s/%s.csv', user_notifications_path, credentials()$user)
  )
  
  r <- reactiveValues(
    unread_messages = NULL,
    message_data = NULL,
    trigger = 0,
    save = FALSE
  )
  observeEvent(credentials(), {
    req(isTRUE(credentials()$user_auth))
    if (file.exists(deploy_file)) {
      all_messages <- read_csv(deploy_file, col_types = 'cD')
      if (is.data.frame(all_messages) && nrow(all_messages) > 0) {
        if (file.exists(user_file())) {
          user_messages <- read_csv(user_file(), col_types = 'cT')
          r$unread_messages <- all_messages %>% 
            filter(!(message %in% user_messages$message))
        } else {
          r$unread_messages <- all_messages
        }
        r$message_data <- r$unread_messages %>%
          select(message) %>% 
          mutate(
            view_time = Sys.time()
          )
        r$trigger <- r$trigger + 1
      }
    } else {
      dir.create(dirname(deploy_file), recursive = TRUE, showWarnings = FALSE)
      tribble(~message, ~start_date) %>% 
      write_csv(path = deploy_file)
    }
  })
  
  observeEvent(input$continue, {
    r$trigger <- r$trigger + 1
    if (input$save) {
      save_title(r$message_data[1, ], user_file(), user_notifications_path)
    }
    r$message_data <- r$message_data[-1, ]
  }, ignoreInit = TRUE)
  
  observeEvent(r$trigger, {
    req(r$trigger > 0)
    if (nrow(r$message_data) > 0) {
      message_path <- sprintf('%s/notifications/messages/%s', gl$app_deployment_environment, r$message_data$message[1])
      if (file.exists(message_path)) {
        showModal(make_modal(message_path, session$ns))  
      } else {
        # Quitar el mensaje manualmente pero no guardar como visto
        r$message_data <- r$message_data[-1, ]
        flog.warn(toJSON(list(
          session_info = msg_cred(credentials()),
          message = 'NOTIFICATION NOT FOUND',
          details = list(
            notification = message_path
          )
        )))
        r$trigger <- r$trigger + 1
      }
    } else {
      removeModal()
    }
  })
}

# UI ----------------------------------------------------------------------
notificationsUI <- function(id) {
  ns <- shiny::NS(id)
}
