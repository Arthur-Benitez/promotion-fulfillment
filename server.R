
require(shiny)
require(shinydashboard)


shinyServer(function(input, output, session){
  
  rv <- reactiveValues(
    sidebar_changed = 0
  )
  
  output$logout_button <- renderUI({
    if (credentials()$user_auth) {
      logout_button <- logoutUI('logout')
    } else {
      logout_button <- NULL
    }
    logout_button
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
    active = reactive(credentials()$user_auth)
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
    id = 'compute_promotions'
  )
  
})

