
require(shiny)
require(shinydashboard)


shinyServer(function(input, output, session){
  
  ## Logging
  if (!dir.exists(paste0(gl$app_deployment_environment, '/log/'))) {
    dir.create(paste0(gl$app_deployment_environment, '/log/'))
  }
  flog.logger(
    name = 'ROOT',
    threshold = INFO,
    appender = appender.tee(paste0(gl$app_deployment_environment, '/log/', as.character(Sys.Date()), '.log'))
  )
  
  output$logout_button <- renderUI({
    logoutUI('logout')
  })
  
  output$sidebar <- renderUI({
    if (credentials()$user_auth) {
      items <- tagList(
        menuItem(tabName = 'promo', text = lang$promo, icon = icon('calculator'), selected = TRUE),
        menuItem(tabName = 'password_update', text = lang$password_update, icon = icon('lock'))
      )
      if (user_clearance(credentials(), gl$clearance_levels) <= 1) {
        items <- tagAppendChildren(
          items,
          tagList(
            menuItem(tabName = 'user_management', text = lang$user_management, icon = icon('users'))
          )
        )
      }
    } else {
      items <- menuItem(tabName = 'login', text = lang$login, icon = icon('sign-in'), selected = TRUE)
    }
    sidebarMenu(
      id = 'menu',
      items
    )
  })
  
  output$body <- renderUI({
    tabItems(
      tabItem(
        tabName = 'login',
        loginUI('login')
      ),
      tabItem(
        tabName = 'promo',
        computePromotionsUI('compute_promotions')
      ),
      tabItem(
        tabName = 'password_update',
        passwordUpdateUI('password_update')
      ),
      tabItem(
        tabName = 'user_management',
        userManagementUI('user_management')
      )
    )
  })
  
  ## Seleccionar una opción del menú al hacer login/logout
  observe({
    if (credentials()$user_auth) {
      updateTabItems(session, 'menu', 'promo')
    } else {
      updateTabItems(session, 'menu', 'login')
    }
  })
  
  ## Login
  credentials <- callModule(
    loginServer,
    id = 'login',
    logout = reactive(logout())
  )
  
  ## Logout
  logout <- callModule(
    logoutServer,
    id = 'logout',
    user_auth = reactive(credentials()$user_auth),
    active = reactive(promotions()$activity_detected)
  )
  
  ## Administración de usuarios
  callModule(
    userManagementServer,
    id = 'user_management',
    credentials = reactive(credentials())
  )
  
  ## Actualización de contraseña
  callModule(
    passwordUpdateServer,
    id = 'password_update',
    credentials = reactive(credentials())
  )
  
  promotions <- callModule(
    computePromotionsServer,
    id = 'compute_promotions',
    credentials = reactive(credentials())
  )
  
})

