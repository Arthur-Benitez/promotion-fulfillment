# Functions ---------------------------------------------------------------
save_title <- function(message_data, user_file, user_notifications_path) {
  if (!dir.exists(user_notifications_path)) {
    dir.create(user_notifications_path, recursive = TRUE)
  }
  write_csv(message_data, path = user_file, na = '', append = ifelse(file.exists(user_file), TRUE, FALSE))
}

# Server ------------------------------------------------------------------
notificationsServer <- function(input, output, session, credentials) {
  deploy_file <- file.path(gl$app_deployment_environment, 'notifications', 'messages', 'deploy.csv')
  user_notifications_path <- file.path(gl$app_deployment_environment, 'notifications', 'users')
  user_file <- sprintf('%s/%s.csv', user_notifications_path, 'sam')
  # credentials()$user
  all_messages <- read_csv(deploy_file) %>% 
    select(message)
  if (file.exists(user_file)) {
    user_messages <- read_csv(user_file)
    message <- all_messages %>% 
      filter(!(message %in% user_messages$message))
  } else {
    message <- all_messages
  }

  
  if (nrow(message) > 0 && is.data.frame(message)) {
    showModal(modalDialog(
      size = 'l',
      easyClose = TRUE,
      title = 'Anuncios',
      includeHTML(sprintf('dev/notifications/messages/%s', message[1,1])),
      footer = modalButton(lang$ok)
    ))
    
    message_data <- message %>% 
      mutate(
        view_time = format(Sys.time(), "%x %X", tz = 'America/Mexico_City'),
        date = format(Sys.time(), "%x")
      )
    save_title(message_data, user_file, user_notifications_path) 
  }
}

# UI ----------------------------------------------------------------------
notificationsUI <- function(id) {
  ns <- shiny::NS(id)
}
