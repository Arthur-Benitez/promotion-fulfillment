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
    footer = tags$div(
      checkboxInput(ns('save'), label = 'No volver a mostrar', value = FALSE),
      actionButton(ns('continue'), label = 'Siguiente')        
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
    all_messages <- read_csv(deploy_file)
    if (file.exists(user_file())) {
      user_messages <- read_csv(user_file())
      r$unread_messages <- all_messages %>% 
        filter(!(message %in% user_messages$message))
    } else {
      r$unread_messages <- all_messages
    }
    r$message_data <- r$unread_messages %>%
      select(message) %>% 
      mutate(
        view_time = format(Sys.time(), "%x %H:%M:%S", tz = 'America/Mexico_City')
      )
    r$trigger <- r$trigger + 1
  })
  
  observeEvent(input$continue, {
    print(input$continue)
    r$trigger <- r$trigger + 1
    # browser()
    if (input$save) {
      # browser()
      save_title(r$message_data[1, ], user_file(), user_notifications_path)
      
      # r$save <- FALSE
    }
    r$message_data <- r$message_data[-1, ]
  }, ignoreInit = TRUE)
  
  # observeEvent(input$save, {
  #   print("Flag")
  #   if (input$save == TRUE) {
  #     r$save <- TRUE 
  #   }
  # })
  
  observeEvent(r$trigger, {
    req(r$trigger > 0)
    print(r$message_data)
    print(input$save)
    print(r$trigger)
    if (nrow(r$message_data) > 0) {
      showModal(make_modal(r$message_data$message[1], session$ns))
    } else {
      removeModal()
    }
  })
}

# UI ----------------------------------------------------------------------
notificationsUI <- function(id) {
  ns <- shiny::NS(id)
}
