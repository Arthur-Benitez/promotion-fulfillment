# Server ------------------------------------------------------------------
notificationsServer <- function(input, output, session, credentials) {
  showModal(modalDialog(
    size = 'l',
    easyClose = TRUE,
    title = 'Instrucciones',
    includeHTML('dev/notifications/messages/instructions.html'),
    footer = modalButton(lang$ok)
  ))
}

# UI ----------------------------------------------------------------------
notificationsUI <- function(id) {
  ns <- shiny::NS(id)
}
