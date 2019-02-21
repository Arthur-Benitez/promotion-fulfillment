
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

if (gl$app_deployment_environment == 'dev') {
  login <- tagList(
    textInput('user', lang$user),
    passwordInput('password', lang$password),
    actionButton('auth', lang$login, icon = icon('sign-in-alt')),
    tags$hr()
  )
} else {
  login <- NULL
}
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
          login,
          uiOutput('items_ui'),
          actionButton('run', lang$run, icon = icon('play')),
          actionButton('reset', lang$reset, icon = icon('redo-alt')),
          downloadButton('download_template', lang$download_template, icon = icon('download'))
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
            value = 'output_summary',
            title = lang$tab_output_summary,
            box(
              width = NULL,
              uiOutput('download_summary_ui'),
              DTOutput('summary_table')
            ),
            box(
              width = NULL,
              uiOutput('output_feature_select_ui'),
              plotOutput('feature_histogram')
            )
          ),
          tabPanel(
            value = 'output_table',
            title = lang$tab_output_table,
            uiOutput('download_ui'),
            DTOutput('output_table') #%>% withSpinner(type = 8)
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