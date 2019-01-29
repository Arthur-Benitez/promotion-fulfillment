
require(shiny)
require(shinydashboard)

header <- dashboardHeader(
  title = lang$app_name
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      tabName = 'promo',
      text = lang$promo,
      icon = icon('calculator')
    ),
    column(
      icon('empire'),
      gl$app_version_text,
      align = 'right',
      width = 12,
      style = 'position: absolute; bottom: 0;'
    )
  )
)

body <- dashboardBody(
  useShinyjs(),
  tags$head(
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'theme.css')
  ),
  tabItems(
    tabItem(
      tabName = 'promo',
      fluidRow(
        box(
          width = 3,
          textInput('user', lang$user),
          passwordInput('password', lang$password),
          actionButton('auth', lang$login, icon = icon('sign-in-alt')),
          tags$hr(),
          uiOutput('items_ui'),
          actionButton('run', lang$run, icon = icon('play')),
          actionButton('reset', lang$reset, icon = icon('redo-alt')),
          uiOutput('download_ui')
        ),
        tabBox(
          id = 'io',
          selected = NULL,
          width = 9,
          tabPanel(
            value = 'input_table',
            title = lang$tab_input,
            DTOutput('input_table')
          ),
          tabPanel(
            value = 'output_table',
            title = lang$tab_output,
            tags$div(
              DTOutput('output_table') #%>% withSpinner(type = 8)
              # h2('Hello'),
            )
          )
        )
      )
    )
  )
)

dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body
)