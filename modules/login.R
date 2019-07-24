
# Funciones ---------------------------------------------------------------

## Identificadores únicos basados en la hora y en el sistema (similar a UUID)
uid <- function() {
  digest::digest(list(Sys.time(), Sys.info(), runif(1)), 'sha256')
}

## Roles de credentials para el log
roles <- function(credentials) {
  paste0('[', paste(credentials$role, collapse = ', '), ']')
}

## Convertir credentials a string para el log
msg_id <- function(credentials) {
  if (is.character(credentials)) {
    msg <- credentials
  } else if (is.list(credentials)) {
    msgs <- c(
      credentials$user,
      roles(credentials),
      credentials$session
    )
    idx <- nchar(msgs) > 0
    msgs <- msgs[idx]
    keys <- c('user', 'role', 'session')[idx]
    msg <- paste(paste0(keys, ': ', msgs), collapse = '; ')
  } else {
    msg <- ''
  }
  sprintf('{%s}', msg)
}

## Seleccionar sólo user, role y session
msg_cred <- function(credentials) {
  idx <- intersect(c('user', 'role', 'session'), names(credentials))
  if (length(idx) == 0) {
    NULL
  } else {
    credentials[idx]
  }
}

## Hash de un objeto
hash <- function(x) {
  digest::digest(x, algo = 'sha256', serialize = FALSE)
}

## Nivel de permisos de un rol
role_clearance <- function(role, clearance_levels) {
  if (length(role) == 0 || is.na(role)) {
    return(Inf)
  }
  lvls <- rep(Inf, length(role))
  idx <- role %in% names(clearance_levels)
  lvls[idx] <- clearance_levels[role[idx]]
  return(min(lvls))
}

## Nivel de permisos de un usuario
user_clearance <- function(user, clearance_levels) {
  if (!is.list(user) || length(user$role) == 0) {
    return(Inf)
  }
  role_clearance(user$role, clearance_levels)
}

## El usuario tiene permiso para realizar la acción?
assert_clearance <- function(requester, affected, affected_new) {
  ## Add: affected = NULL, affected_new = user to add
  ## Update: affected = current user to change, affected_new = user after changes
  ## Delete: affected = current user to delete, affected_new = NULL
  stopifnot(!is.null(affected) || !is.null(affected_new))
  requester_level <- user_clearance(requester, gl$clearance_levels)
  affected_level <- user_clearance(affected, gl$clearance_levels)
  affected_new_level <- user_clearance(affected_new, gl$clearance_levels)
  
  if (is.null(requester$user) || requester_level == min(gl$clearance_levels)) {
    ## NULL (root) and the highest level (owner) have the absolute clearance by default to override any settings
    verdict <- TRUE
  } else if (
    requester_level < affected_level &&
    requester_level <= affected_new_level
  ) {
    ## Permission to do anything to users with lower clearance level
    verdict <- TRUE
  } else if (
    !is.null(affected$user) &&
    !is.null(affected_new$user) &&
    requester$user == affected$user &&
    requester$user == affected_new$user &&
    requester_level <= affected_new_level
  ) {
    ## Can only edit self amongst users with same clearance, but can't change user name and can only maintain or lower clearance level
    ## WARNING: You could end up with no owners!
    verdict <- TRUE
  } else {
    verdict <- FALSE
  }
  verdict
}

## Inicializar base de datos de usuarios
initialize_user_database <- function() {
  list()
}

## Cargar usuarios
load_users <- function(user_data_path) {
  readr::read_tsv(user_data_path, col_types = readr::cols(.default = readr::col_character()), na = 'NA') %>% 
    dplyr::mutate(role = stringr::str_split(role, ',')) %>% 
    purrr::transpose()
}

## Verificar si existe un usuario
user_exists <- function(username, user_data_path) {
  users <- load_users(user_data_path)
  user %in% map_chr(users, 'user')
}

## Regresar un usuario usando su nombre
get_user <- function(username, user_data_path) {
  stopifnot(is.character(username) && length(username) == 1)
  usr <- load_users(user_data_path) %>% 
    keep(~ .x$user == username)
  if (length(usr) == 0) {
    usr <- NULL
  } else {
    usr <- usr[[1]]
  }
  return(usr)
}

## Guardar usuarios
save_users <- function(users, user_data_path) {
  users %>% 
    transpose() %>% 
    tibble::as.tibble() %>% 
    dplyr::mutate_at(vars(user, password_hash), unlist) %>% 
    dplyr::mutate(role = purrr::map_chr(role, ~paste(.x, collapse=','))) %>% 
    readr::write_tsv(user_data_path)
  
  futile.logger::flog.info(toJSON(list(
    message = "SAVED USERS",
    details = list(
      target = user_data_path
    )
  )))
}

## Borrar usuarios
delete_user <- function(users, credentials = NULL, user_to_delete) {
  idx <- which(unlist(purrr::map(users, 'user')) == user_to_delete)
  if (length(idx) == 1) {
    if (assert_clearance(credentials, users[[idx]], NULL)) {
      futile.logger::flog.info(toJSON(list(
        session_info = msg_cred(credentials),
        message = "DELETED USER",
        details = list(
          target = msg_cred(users[[idx]])
        )
      )))
      users <- users[-idx]
      status <- 0
    } else {
      futile.logger::flog.error(toJSON(list(
        session_info = msg_cred(credentials),
        message = "ERROR DELETING USER",
        details = list(
          target = msg_cred(users[[idx]]),
          reason = 'Insufficient clearance'
        )
      )))
      status <- 1
    }
  } else {
    futile.logger::flog.error(toJSON(list(
      session_info = msg_cred(credentials),
      message = "ERROR DELETING USER",
      details = list(
        target = msg_cred(list(user = user_to_delete)),
        reason = 'User doesn\'t exist'
      )
    )))
    status <- 1
  }
  list(
    users = users,
    status = status
  )
}

## Agregar usuarios
add_user <- function(users, credentials = NULL, new_user, new_password, new_role) {
  idx <- which(unlist(purrr::map(users, 'user')) == new_user)
  #### ARREGLAR! sólo admins deben agregar admins, etc
  if (length(idx) == 0) {
    new <- list(
      user = new_user,
      role = new_role
    )
    if (assert_clearance(credentials, NULL, new)) {
      futile.logger::flog.info(toJSON(list(
        session_info = msg_cred(credentials),
        message = "CREATED USER",
        details = list(
          target = msg_cred(new)
        )
      )))
      users <- users %>% 
        c(list(list(
          user = new_user,
          password_hash = hash(new_password),
          role = new_role
        )))
      status <- 0
    } else {
      futile.logger::flog.error(toJSON(list(
        session_info = msg_cred(credentials),
        message = "ERROR CREATING USER",
        details = list(
          target = msg_cred(new),
          reason = 'Clearance insufficient'
        )
      )))
      status <- 1
    }
  } else {
    futile.logger::flog.error(toJSON(list(
      session_info = msg_cred(credentials),
      message = "ERROR CREATING USER",
      details = list(
        target = msg_cred(list(user = new_user)),
        reason = 'User already exists'
      )
    )))
    status <- 1
  }
  list(
    users = users,
    status = status
  )
}

## Actualizar usuarios
update_user <- function(users, credentials = NULL, user_to_update, new_password = NULL, new_role = NULL) {
  password_empty <- is.null(new_password) || length(new_password) == 0 # || nchar(new_password) == 0
  role_empty <- is.null(new_role) || length(new_role) == 0
  if (password_empty && role_empty) {
    futile.logger::flog.error(toJSON(list(
      session_info = msg_cred(credentials),
      message = "ERROR UPDATING USER",
      details = list(
        target = msg_cred(list(user = user_to_update)),
        reason = 'No changes submitted'
      )
    )))
    status <- 1
  } else if (!password_empty && !role_empty) {
    futile.logger::flog.error(toJSON(list(
      session_info = msg_cred(credentials),
      message = "ERROR UPDATING USER",
      details = list(
        target = msg_cred(list(user = user_to_update)),
        reason = 'Cannot update password and role at the same time'
      )
    )))
    status <- 1
  } else {
    idx <- which(unlist(purrr::map(users, 'user')) == user_to_update)
    if (length(idx) == 1) {
      new <- list(
        user = user_to_update,
        role = new_role
      )
      if (assert_clearance(credentials, users[[idx]], new)) {
        ## A: Esto es excluyente con B
        if (!password_empty) {
          if (length(new_password) == 1) {
            new_password_hash <- hash(new_password)
            futile.logger::flog.info(toJSON(list(
              session_info = msg_cred(credentials),
              message = "UPDATED USER PASSWORD",
              details = list(
                target = msg_cred(users[[idx]])
              )
            )))
            users[[idx]]$password_hash <- new_password_hash
            status <- 0
          } else {
            futile.logger::flog.error(toJSON(list(
              session_info = msg_cred(credentials),
              message = "ERROR UPDATING PASSWORD",
              details = list(
                target = msg_cred(users[[idx]]),
                reason = 'The new password must be a character vector of length = 1'
              )
            )))
            status <- 1
          }
        }
        ## B: Esto es excluyente con A
        if (!role_empty) {
          if (length(new_role) > 0) {
            futile.logger::flog.info(toJSON(list(
              session_info = msg_cred(credentials),
              message = "UPDATED USER ROLE",
              details = list(
                target = msg_cred(users[[idx]]),
                updated_target = msg_cred(list(user = users[[idx]]$user, role = new$role))
              )
            )))
            users[[idx]]$role <- new_role
            status <- 0
          } else {
            futile.logger::flog.error(toJSON(list(
              session_info = msg_cred(credentials),
              message = "ERROR UPDATING ROLE",
              details = list(
                target = msg_cred(users[[idx]]),
                reason = 'New role must be a character vector of length >= 1'
              )
            )))
            status <- 1
          }
        }
      } else {
        futile.logger::flog.error(toJSON(list(
          session_info = msg_cred(credentials),
          message = "ERROR UPDATING USER",
          details = list(
            target = msg_cred(new),
            reason = 'Insufficient clearance'
          )
        )))
        status <- 1
      }
    } else {
      futile.logger::flog.error(toJSON(list(
        session_info = msg_cred(credentials),
        message = "ERROR UPDATING USER",
        details = list(
          target = msg_cred(list(user = user_to_update)),
          reason = 'User doesn\'t exist'
        )
      )))
      status <- 1
    }
  }
  
  list(
    users = users,
    status = status
  )
}

## Autenticar (hacer login)
auth_user <- function(input_user, input_password) {
  users <- load_users(gl$user_data_path)
  idx <- which(unlist(purrr::map(users, 'user')) == input_user)
  if (length(idx) == 1) {
    if (hash(input_password) == users[[idx]]$password_hash) {
      return(list(
        user_auth = TRUE,
        user = input_user,
        role = users[[idx]]$role
      ))
    }
  }
  return(list(
    user_auth = FALSE,
    user = input_user,
    role = NULL
  ))
}

## Generar usuarios iniciales
if (FALSE) {
  initialize_user_database() %>% 
    add_user(NULL, 'f0g00bq', '', c('basic', 'admin', 'owner')) %>% 
    .$users %>% 
    save_users(gl$user_data_path)
}

## Leer credenciales de HTTP_COOKIE (SSO login)
sso_credentials <- function(session) {
  cookies <- session$request$HTTP_COOKIE
  if (is.null(cookies)) {
    res <- NULL
  } else {
    res <- tryCatch({
      strsplit(cookies, " ")[[1]] %>% 
        {keep(., ~ startsWith(.x, 'MLAuth'))[[1]]} %>% 
        {strsplit(trimws(.), '=')[[1]][2]} %>% 
        {strsplit(., '.', fixed = TRUE)[[1]][2]} %>% 
        base64enc::base64decode() %>% 
        rawToChar() %>% 
        jsonlite::fromJSON()
    }, error = function(e){
      NULL
    })
    if (!is.null(res)) {
      res$user <- str_replace(res$loginId, '.+\\\\', '')
    }
  }
  
  return(res)
}


# Login -------------------------------------------------------------------

## UI
loginUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    id = ns('panel'),
    class = 'login-panel',
    shiny::wellPanel(
      includeHTML('html/welcome.html') %>% 
        str_replace_all('__email_name__', lang$email_name) %>% 
        str_replace_all('__app_name__', lang$app_name) %>% 
        str_replace_all('__email_details__', lang$emailto) %>%
        HTML()
    )
  )
}

## Server
loginServer <- function(input, output, session) {
  
  ## Ayuda: Recuperar contraseña
  observeEvent(input$recover_password, {
    showModal(modalDialog(
      title = lang$recover_password,
      HTML('<p>Pídele a cualquier usuario con permisos de administrador que la cambie en la ruta:<br><strong>Administración de usuarios > <em>seleccionar tu usuario</em> > Actualizar contraseña</strong></p>'),
      easyClose = TRUE
    ))
  })
  
  ## Login
  credentials <- shiny::reactiveValues(user_auth = FALSE)
  
  observe({
    if (gl$app_deployment_environment == 'dev') {
      cred <- list(
        user_auth = TRUE,
        user = 'sam',
        role = 'owner'
      )
    } else {
      sso_cred <- sso_credentials(session)
      if (is.null(sso_cred)) {
        cred <- list(
          user_auth = FALSE,
          user = NULL,
          role = NULL
        )
      } else {
        usr <- get_user(sso_cred$user, gl$user_data_path)
        cred <- list(
          user_auth = !is.null(usr),
          user = sso_cred$user,
          role = usr$role
        )
      }
    }
    futile.logger::flog.info(toJSON(list(
      session_info = list(),
      message = "ATTEMPTING USER LOGIN",
      details = list(
        credentials = cred['user']
      )
    )))
    if (cred$user_auth) {
      credentials$user_auth <- cred$user_auth
      credentials$user <- cred$user
      credentials$role <- cred$role
      credentials$session <- uid()
      futile.logger::flog.info(toJSON(list(
        session_info = msg_cred(shiny::reactiveValuesToList(credentials)),
        message = "LOGIN SUCCESSFUL",
        details = list(
          session = credentials$session
        )
      )))
    } else {
      futile.logger::flog.warn(toJSON(list(
        session_info = list(),
        message = "LOGIN FAILED",
        details = list(
          credentials = cred['user']
        )
      )))
      shinyjs::toggle(id = 'error', anim = TRUE, time = 1, animType = 'fade')
      shinyjs::delay(5000, shinyjs::toggle(id = "error", anim = TRUE, time = 1, animType = "fade"))
    }
  })
  
  shiny::reactive({
    shiny::reactiveValuesToList(credentials)
  })
}


# Logout ------------------------------------------------------------------


## UI
logoutUI <- function(id) {
  ns <- shiny::NS(id)
  uiOutput(ns('ui'))
}

## Server
logoutServer <- function(input, output, session, user_auth, active) {
  output$ui <- renderUI({
    ns <- session$ns
    tags$div(
      id = 'header-icons',
      tags$div(
        title = lang$logout_timeout_info,
        class = 'header-text',
        textOutput(ns('counter'))
      )
    )
  })
  ## Contador de tiempo hasta logout automático
  counter_sec <- 60
  counter_max <- 60 * 20
  rv <- reactiveValues(
    counter = counter_max
  )
  ## El contador se resetea si se hace login, logout, run, reset o login Teradata
  observeEvent(user_auth() + active() + input$button, {
    rv$counter <- counter_max
  })
  ## Se restan un contador cada counter_sec
  timer <- reactiveTimer(1000)
  observeEvent(timer(), {
    rv$counter <- rv$counter - 1
    if (rv$counter <= 0) {
      shinyalert(
        title = lang$auto_logout_title,
        type = 'info'
      )
      session$close()
    }
  })
  output$counter <- renderText({
    s <- ifelse(rv$counter < counter_sec, '"', "'")
    sprintf("%d%s", rv$counter %/% ifelse(rv$counter < counter_sec, 1, counter_sec), s)
  })
  shiny::reactive({input$button})
}


# Cambio de password ------------------------------------------------------

## UI
passwordUpdateUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::wellPanel(
    id = ns('password_update'),
    shiny::h2('Cambiar contraseña'),
    shiny::passwordInput(ns('old_password'), lang$old_password),
    shiny::passwordInput(ns('new_password_1'), lang$new_password_1),
    shiny::passwordInput(ns('new_password_2'), lang$new_password_2),
    shiny::actionButton(ns('button'), lang$button),
    shinyjs::hidden(
      shiny::div(
        id = ns("msg"),
        shiny::span(
          textOutput(ns('msg_text')),
          ## NO SIRVE ESTO
          style = "color: red; font-weight: bold; padding-top: 5px;", class = "text-center"
        )
      )
    )
  )
}

## Server
passwordUpdateServer <- function(input, output, session, credentials) {
  observeEvent(input$button, {
    if (input$new_password_1 != input$new_password_2) {
      futile.logger::flog.error(toJSON(list(
        session_info = msg_cred(credentials()),
        message = "ERROR CHANGING PASSWORD",
        details = list(
          reason = 'New password mismatch'
        )
      )))
      output$msg_text <- renderText(lang$passwords_must_match)
      shinyjs::toggle(id = 'msg', anim = TRUE, time = 1, animType = 'fade')
      shinyjs::delay(5000, shinyjs::toggle(id = "msg", anim = TRUE, time = 1, animType = "fade"))
    } else {
      cred <- auth_user(credentials()$user, input$old_password)
      if (cred$user_auth) {
        output$msg_text <- renderText(lang$password_updated_successfully)
        users <- load_users(gl$user_data_path) %>% 
          update_user(
            credentials = cred,
            user_to_update = cred$user,
            new_password = input$new_password_1,
            new_role = NULL
          )
        if (users$status == 0) {
          save_users(users$users, gl$user_data_path)
        }
      } else {
        futile.logger::flog.error(toJSON(list(
          session_info = msg_cred(credentials()),
          message = "ERROR CHANGING PASSWORD",
          details = list(
            reason = 'Current password is incorrect'
          )
        )))
        output$msg_text <- renderText(lang$wrong_current_password)
        shinyjs::toggle(id = 'msg', anim = TRUE, time = 1, animType = 'fade')
        shinyjs::delay(5000, shinyjs::toggle(id = "msg", anim = TRUE, time = 1, animType = "fade"))
      }
    }
  })
}


# Administración de usuarios ----------------------------------------------

## UI
userManagementUI <- function(id) {
  ns <- shiny::NS(id)
  actions <- c('add', 'update_password', 'update_role', 'delete')
  names(actions) <- c(lang$add, lang$update_password, lang$update_role, lang$delete)
  shiny::wellPanel(
    id = ns('manage_users'),
    shiny::h2(lang$manage_users),
    shiny::uiOutput(ns('user_list_ui')),
    shiny::textInput(ns('new_user'), lang$user),
    shiny::textInput(ns('new_password'), lang$password),
    shiny::selectInput(ns('new_role'), lang$role, choices = rev(names(gl$clearance_levels)), selected = 'basic', multiple = TRUE),
    shiny::radioButtons(ns('action'), lang$action, choices = actions),
    shiny::actionButton(ns('button'), lang$button),
    shinyjs::hidden(
      shiny::div(
        id = ns("msg"),
        shiny::span(
          textOutput(ns('msg_text')),
          ## NO SIRVE ESTO
          style = "color: red; font-weight: bold; padding-top: 5px;", class = "text-center"
        )
      )
    )
  )
}

## Server
userManagementServer <- function(input, output, session, credentials) {
  output$user_list_ui <- shiny::renderUI({
    input$button
    ns <- session$ns
    shiny::selectInput(
      inputId = ns('selected_user'),
      label = lang$selected_user,
      choices = sort(load_users(gl$user_data_path) %>% map_chr('user'))
    )
  })
  shiny::observe({
    req(input$selected_user)
    ns <- session$ns
    user <- load_users(gl$user_data_path) %>% keep(~.x$user == input$selected_user) %>% .[[1]]
    shiny::updateTextInput(session, 'new_user', value = user$user)
    shiny::updateTextInput(session, 'new_password', value = '')
    shiny::updateSelectInput(session, 'new_role', selected = user$role)
  })
  
  msg <- reactiveVal()
  output$msg_text <- renderText(msg())
  shiny::observeEvent(input$button, {
    
    users <- load_users(gl$user_data_path)
    
    if (input$action == 'add') {
      users <- users %>% 
        add_user(credentials(), input$new_user, input$new_password, input$new_role)
    } else if (input$action == 'update_password') {
      users <- users %>% 
        update_user(credentials(), input$new_user, input$new_password, NULL)
    } else if (input$action == 'update_role') {
      users <- users %>% 
        update_user(credentials(), input$new_user, NULL, input$new_role)
    } else if (input$action == 'delete') {
      users <- users %>% 
        delete_user(credentials(), input$new_user)
    }
    
    if (users$status == 0) {
      save_users(users$users, gl$user_data_path)
      msg(switch(
        input$action,
        add = lang$add_success,
        update_password = lang$update_password_success,
        update_role = lang$update_role_success,
        delete = lang$delete_success,
        lang$unknown_action
      ))
    } else {
      msg(switch(
        input$action,
        add = lang$add_error,
        update_password = lang$update_password_error,
        update_role = lang$update_role_error,
        delete = lang$delete_error,
        lang$unknown_action
      ))
    }
    
    shinyjs::show(id = 'msg', anim = TRUE, time = 1, animType = 'fade')
    shinyjs::delay(5000, shinyjs::hide(id = "msg", anim = TRUE, time = 1, animType = "fade"))
  })
}



