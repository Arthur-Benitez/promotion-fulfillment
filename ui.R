
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
  tags$li(
    class = 'dropdown header-icons',
    tags$div(
      title = lang$help_title,
      actionButton('help', '', icon = icon('question-circle'), class = 'header-icon')
    ),
    tags$div(
      title = lang$logout_timeout_info,
      class = 'header-text',
      logoutUI('logout')
    ),
    uiOutput('user_level_icon')
  )
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
    tags$link(rel = 'stylesheet', type = 'text/css', href = sprintf('theme.css?%s', Sys.time())),
    tags$link(rel = 'stylesheet', type = 'text/css', href = sprintf('season/season.css?%s', Sys.time())),
    HTML("<script src = 'season/season.js'></script>"),
    # includeCSS('www/theme.css'), ## Alternativa que mete el CSS al HTML, por si se necesita
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
