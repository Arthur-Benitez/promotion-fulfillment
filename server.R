
require(shiny)
require(shinydashboard)


shinyServer(function(input, output, session){
  
  ## Logging
  log_dir <- paste0(gl$app_deployment_environment, '/log/')
  init_log(log_dir)
  
  output$logout_counter <- renderUI({
    logoutUI('logout')
  })
  
  output$user_level_icon <- renderUI({
    req(credentials()$user_auth)
    icn_name <- switch(
      as.character(role_clearance(credentials()$role, gl$clearance_levels)),
      '0' = 'crown',
      '1' = 'star',
      '2' = 'user'
    )
    tags$div(
      title = sprintf(lang$user_level_icon, credentials()$user),
      actionButton('user_level_icon', '', icon = icon(icn_name), class = 'header-icon')
    )
  })
  
  output$sidebar <- renderUI({
    if (credentials()$user_auth) {
      if (user_clearance(credentials(), gl$clearance_levels) <= 1) {
        items <- tagList(
          menuItem(tabName = 'promo', text = lang$promo, icon = icon('calculator'), selected = TRUE),
          menuItem(tabName = 'usage_stats', text = lang$usage_stats, icon = icon('tachometer-alt')),
          menuItem(tabName = 'password_update', text = lang$password_update, icon = icon('lock')),
          menuItem(tabName = 'user_management', text = lang$user_management, icon = icon('users'))
        )
      } else {
        items <- tagList(
          menuItem(tabName = 'promo', text = lang$promo, icon = icon('calculator'), selected = TRUE),
          menuItem(tabName = 'password_update', text = lang$password_update, icon = icon('lock'))
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
    if (credentials()$user_auth) {
      if (user_clearance(credentials(), gl$clearance_levels) <= 1) {
        items <- tabItems(
          tabItem(
            tabName = 'promo',
            computePromotionsUI('compute_promotions')
          ),
          tabItem(
            tabName = 'usage_stats',
            usageStatsUI('usage_stats')
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
      } else {
        items <- tabItems(
          tabItem(
            tabName = 'promo',
            computePromotionsUI('compute_promotions')
          ),
          tabItem(
            tabName = 'password_update',
            passwordUpdateUI('password_update')
          )
        )
      }
    } else {
      items <- tabItems(
        tabItem(
          tabName = 'login',
          loginUI('login')
        )
      )
    }
  })
  
  ## Seleccionar una opción del menú al hacer login/logout
  observe({
    if (credentials()$user_auth) {
      updateTabItems(session, 'menu', 'promo')
    } else {
      updateTabItems(session, 'menu', 'login')
    }
  })
  
  ## Ayuda
  observeEvent(input$help, {
    shinyalert(
      title = lang$help_title,
      text = includeHTML('html/help.html'),
      type = 'info',
      html = TRUE,
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      showCancelButton = FALSE,
      showConfirmButton = TRUE,
      confirmButtonText = lang$ok
    )
  })
  
  ## Login
  credentials <- callModule(
    loginServer,
    id = 'login'
  )
  
  ## Logout
  ### Detectar actividad en general
  activity <- reactiveValues(
    counter = 0
  )
  onclick('body', {
    activity$counter <- activity$counter + 1
  })
  onclick('sidebarCollapsed', {
    activity$counter <- activity$counter + 1
  })
  logout <- callModule(
    logoutServer,
    id = 'logout',
    user_auth = reactive(credentials()$user_auth), # login
    active = reactive(activity$counter) # click en sidebar o body
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
  
  ## Cálculos de Promo Fulfillment
  promotions <- callModule(
    computePromotionsServer,
    id = 'compute_promotions',
    credentials = reactive(credentials())
  )
  
  ## Utilización
  callModule(
    usageStatsServer,
    id = 'usage_stats',
    credentials = reactive(credentials()),
    dev_connection = reactive(promotions()[c('ch', 'is_open')])
  )
  
  ## Notificaciones
  callModule(
    notificationsServer,
    id = 'notifications',
    credentials = reactive(credentials())
  )
})

