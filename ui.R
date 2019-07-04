
require(shiny)
require(shinydashboard)

header <- dashboardHeader(
  title = tags$div(
    tags$div(
      id = 'app-logo-container',
      tags$img(src = 'spark-logo.png', id = 'app-logo')
    ),
    tags$div(
      id = 'app-name-container',
      tags$p(id = 'app-name', lang$app_name)
      # tags$p(id = 'app-description', 'XXXXXXX')
    )
  ),
  tags$li(class = 'dropdown', actionButton('help', '', icon = icon('question-circle'))),
  tags$li(class = 'dropdown', uiOutput('logout_button'))
)

sidebar <- dashboardSidebar(
  collapsed = TRUE,
  uiOutput('sidebar'),
  column(
    icon('empire'),
    gl$app_version_text,
    align = 'right',
    width = 12,
    style = 'position: absolute; bottom: 0;'
  )
)

body <- dashboardBody(
  useShinyjs(),
  useShinyalert(),
  tags$head(
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'theme.css'),
    tags$link(rel = 'stylesheet', href = 'https://fonts.googleapis.com/css?family=Bree+Serif|Coiny')
  ),
  uiOutput('body')
)

dashboardPage(
  title = lang$app_name,
  header = header,
  sidebar = sidebar,
  body = body
)