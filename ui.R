
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
    uiOutput('auth_ui'),
    tags$hr()
  )
} else {
  login <- NULL
}
body <- dashboardBody(
  useShinyjs(),
  useShinyalert(),
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
          selectInput('date_format', lang$date_format, c('yyyy-mm-dd' = '%Y-%m-%d',
                                                         'dd/mm/yyyy' = '%d/%m/%Y',
                                                         'mm/dd/yyyy' = '%m/%d/%Y')),
          uiOutput('items_ui'),
          tags$div(
            style = 'margin-bottom: 20px;',
            downloadButton('download_instructions', lang$download_instructions, icon = icon('download')),
            downloadButton('download_template', lang$download_template, icon = icon('download'))
          ),
          actionButton('run', lang$run, icon = icon('play')),
          actionButton('reset', lang$reset, icon = icon('redo-alt'))
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
            tags$div(
              class = 'inline-inputs',
              checkboxGroupInput(
                'summary_groups',
                label = lang$summary_groups,
                choices = c('feature_name', 'cid', 'store_nbr') %>% 
                  set_names(c(lang$feature_name, lang$cid, lang$store_nbr)),
                selected = c('feature_name', 'cid'),
                inline = TRUE
              ),
              tags$div(
                class = 'inline-button-wrapper',
                uiOutput('download_summary_ui')
              ),
              tags$div(
                class = 'inline-button-wrapper',
                uiOutput('download_header_ui')
              ),
              tags$div(
                class = 'inline-button-wrapper',
                uiOutput('download_detail_ui')
              )
            ),
            DTOutput('summary_table')
          ),
          tabPanel(
            value = 'output_histogram',
            title = lang$tab_output_histogram,
            uiOutput('output_feature_select_ui'),
            plotlyOutput('feature_histogram')
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