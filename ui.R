
require(shiny)
require(shinydashboard)

header <- dashboardHeader(
  title = 'Promo Fulfillment'
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      tabName = 'promo',
      text = 'Promo',
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
          textInput('user', 'Usuario'),
          passwordInput('password', 'Password'),
          actionButton('auth', 'Login', icon = icon('sign-in-alt')),
          tags$hr(),
          fileInput('items', 'Items'),
          actionButton('run', 'Correr', icon = icon('play')),
          actionButton('reset', 'Reset', icon = icon('redo-alt'))
        ),
        box(
          width = 9,
          tabBox(
            id = 'io',
            selected = NULL,
            tabPanel(
              value = 'input_table',
              title = 'Entradas',
              DTOutput('input_table')
            ),
            tabPanel(
              value = 'output_table',
              title = 'Resultados',
              DTOutput('output_table') %>% withSpinner(type = 8),
              hidden(
                tags$div(
                  id = 'error-query',
                  h1('La consulta falló :(', class = 'error-msg')
                )
              ),
              hidden(
                tags$div(
                  id = 'error-comp',
                  h1('Los cálculos fallaron :(', class = 'error-msg')
                )
              )
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