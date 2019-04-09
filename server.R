
require(shiny)
require(shinydashboard)


shinyServer(function(input, output, session){
  
  output$header <- renderUI({
    dashboardHeader(
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
      )
    )
  })
  
  output$sidebar <- renderUI({
    dashboardSidebar(
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
  })
  
  output$body <- renderUI({
    dashboardBody(
      useShinyjs(),
      useShinyalert(),
      tags$head(
        tags$link(rel = 'stylesheet', type = 'text/css', href = 'theme.css'),
        tags$link(rel = 'stylesheet', href = 'https://fonts.googleapis.com/css?family=Bree+Serif|Coiny')
      ),
      tabItems(
        tabItem(
          tabName = 'promo',
          computePromotionsUI('compute-promotions')
        )
      )
    )
  })
  
  promotions <- callModule(
    computePromotionsServer,
    id = 'compute-promotions'
  )
  
})

