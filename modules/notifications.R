# Functions ---------------------------------------------------------------
save_title <- function(message_data, user_file, user_notifications_path) {
  if (!dir.exists(user_notifications_path)) {
    dir.create(user_notifications_path, recursive = TRUE)
  }
  write_csv(message_data, path = user_file, na = '', append = ifelse(file.exists(user_file), TRUE, FALSE))
}

make_modal <- function(message, ns){
  modalDialog(
    size = 'l',
    title = 'Anuncios',
    includeHTML(sprintf('dev/notifications/messages/%s', message)),
    footer = list(actionButton(ns('continue'), label = 'Siguiente'), modalButton(lang$ok))
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
    trigger = 0 # Corregir - la inicialización actúa como gatillo
  )
  observeEvent(credentials(), {
    all_messages <- read_csv(deploy_file)
    if (file.exists(user_file())) {
      user_messages <- read_csv(user_file())
      r$unread_messages <- all_messages %>% 
        filter(!(message %in% user_messages$message))
    } else {
      r$unread_messages <- all_messages
    }
    r$trigger <- r$trigger + 1
    message_data <- r$unread_messages %>%
      select(message) %>% 
      mutate(
        view_time = format(Sys.time(), "%x %H:%M:%S", tz = 'America/Mexico_City')
      )
    save_title(message_data, user_file(), user_notifications_path)
  })
  
  observeEvent(input$continue, {
    r$trigger <- r$trigger + 1
  })
  
  observeEvent(r$trigger, {
    if (nrow(r$unread_messages) > 0) {
      showModal(make_modal(r$unread_messages$message[1], session$ns))
      r$unread_messages <- r$unread_messages[-1, ]
    } else {
      removeModal()
    }
  })
}

# UI ----------------------------------------------------------------------
notificationsUI <- function(id) {
  ns <- shiny::NS(id)
}
