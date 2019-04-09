
require(shiny)
require(shinydashboard)

header <- uiOutput('header')

sidebar <- uiOutput('sidebar')

body <- uiOutput('body')

dashboardPage(
  title = lang$app_name,
  header = header,
  sidebar = sidebar,
  body = body
)