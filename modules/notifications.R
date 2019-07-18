#Funciones
save_title <- function(message_data, gl, credentials) {
  user_notifications_path <- file.path(gl$app_deployment_environment, 'notifications', 'users')
  user_file <- sprintf('%s/%s.csv', user_notifications_path, credentials$user)
  
  if (!dir.exists(user_notifications_path)) {
    dir.create(user_notifications_path, recursive = TRUE)
  }
  
  write_csv(message_data, path = user_file, na = '', append = ifelse(file.exists(user_file), TRUE, FALSE))
}

# Server ------------------------------------------------------------------
notificationsServer <- function(input, output, session, credentials) {
  message = 'instructions.html'
  showModal(modalDialog(
    size = 'l',
    easyClose = TRUE,
    title = 'Instrucciones',
    includeHTML(sprintf('dev/notifications/messages/%s', message)),
    footer = modalButton(lang$ok)
  ))
  
  message_data <- tibble(message) %>% 
    mutate(
      view_time = format(Sys.time(), "%x %X", tz = 'America/Mexico_City'),
      date = format(Sys.time(), "%x")
    )
  save_title(message_data = message_data, gl = gl, credentials = credentials)
}

# UI ----------------------------------------------------------------------
notificationsUI <- function(id) {
  ns <- shiny::NS(id)
}
