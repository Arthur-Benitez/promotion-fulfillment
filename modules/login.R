
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
  idx <- intersect(c('user', 'role', 'session', 'platform'), names(credentials))
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
  if (is.character(username) && length(username) == 1) {
    usr <- load_users(user_data_path) %>% 
      keep(~ .x$user == username)
    if (length(usr) == 0) {
      usr <- NULL
    } else {
      usr <- usr[[1]]
    } 
  } else {
    usr <- NULL
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
      value <- str_replace(res$loginId, '.+\\\\', '')
      res$user <- ifelse(is.na(value), NULL, value)
    }
  }
  
  return(res)
}

## Leer credenciales de Compass
compass_credentials <- function(session) {
  cookies <- session$request$HTTP_COOKIE
  res <- list(user = NULL)
  if (is.null(cookies)) {
    res$user <- NULL
  } else {
    res$user <- tryCatch({
      strsplit(cookies, " ")[[1]] %>% 
        {keep(., ~ startsWith(.x, 'CompassUserName'))[[1]]} %>% 
        {strsplit(trimws(.), '=')[[1]][2]}
    }, error = function(e){
      NULL
    })
    if (!is.null(res$user) && is.na(res$user)) {
      res$user <- NULL
    }
  }
  return(res)
}

## Combinar todos los accesos
all_credentials <- function(session, user_data_path, app_deployment_environment) {
  sso_cred <- sso_credentials(session)
  compass_cred <- compass_credentials(session)
  res <- list(user = NULL, role = NULL, user_auth = NULL, platform = NULL)
  
  if (app_deployment_environment == 'dev') {
    res <- list(
      user_auth = TRUE,
      user = 'sam',
      role = 'owner',
      platform = 'dev'
    )
  } else if (!is.null(sso_cred)) {
    sso_user_auth <- get_user(sso_cred$user, user_data_path)
    res$user <- sso_cred$user
    res$role <- sso_user_auth$role
    res$user_auth <- !is.null(sso_user_auth$user)
    res$platform <- 'sso'
  } else if (!is.null(compass_cred$user)) {
    compass_user_auth <- get_user(compass_cred$user, user_data_path)
    res$user <- compass_cred$user
    if (is.null(compass_user_auth)) {
      users <- load_users(user_data_path) %>% 
        add_user(NULL, compass_cred$user, '', c('basic'))
      if (users$status == 0) {
        save_users(users$users, user_data_path)
      }
      res$role <- c('basic')
    } else {
      res$role <- compass_user_auth$role
    }
    res$user_auth <- TRUE
    res$platform <- 'compass'
  } else {
    res$user <- NULL
    res$role <- NULL
    res$user_auth <- FALSE
    res$platform <- 'unknown'
  }
  return(res)
}

update_rrp_info <- function(ch = NULL, credentials, connector = 'production-connector') {
  query <- readLines('sql/item_rrp_sync.sql') %>% 
    paste(collapse = '\n')
  tryCatch({
    flog.info(toJSON(list(
      session_info = msg_cred(credentials),
      message = 'START UPDATING RRP INFO',
      details = list()
    )))
    shinyalert(
      title = 'Actualizando base de datos de RRP y Sync status...',
      text = '',
      type = 'info',
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE
    )
    res <- sql_query(
      ch = ch,
      connector =  connector,
      query = query,
      stringsAsFactors = FALSE
    )
    path_info <- unlist(strsplit(gl$rrp_sync_database, split = '/'))
    backup_file(
      res,
      folder = path_info[1],
      complete_file_name = path_info[length(path_info)],
      preserved_backups = gl$rrp_sync_backups_n
    )
    saveRDS(res, gl$rrp_sync_database)
    flog.info(toJSON(list(
      session_info = msg_cred(credentials),
      message = 'DONE UPDATING RRP INFO',
      details = list()
    )))
    shinyalert(
      title = 'Base de datos de RRP y Sync status actualizada',
      text = '',
      type = 'info',
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE
    )
  }, error = function(e){
    flog.warn(toJSON(list(
      session_info = msg_cred(credentials),
      message = 'FAILED TO UPDATE RRP INFO',
      details = list()
    )))
  })
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
    cred <- all_credentials(session, gl$user_data_path, gl$app_deployment_environment)
    
    futile.logger::flog.info(toJSON(list(
      session_info = list(),
      message = "ATTEMPTING USER LOGIN",
      details = list(
        credentials = cred$user,
        platform = cred$platform
      )
    )))
    if (cred$user_auth) {
      ## No se puede copiar una lista a reactiveValues directo
      for (v in names(cred)) {
        credentials[[v]] <- cred[[v]]
      }
      credentials$session <- uid()
      futile.logger::flog.info(toJSON(list(
        session_info = msg_cred(shiny::reactiveValuesToList(credentials)),
        message = "LOGIN SUCCESSFUL",
        details = list(
          session = credentials$session,
          platform = credentials$platform
        )
      )))
    } else {
      futile.logger::flog.warn(toJSON(list(
        session_info = list(),
        message = "LOGIN FAILED",
        details = list(
          credentials = cred$user,
          platform = cred$platform
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
logoutServer <- function(input, output, session, user_auth, active, is_running) {
  output$ui <- renderUI({
    ns <- session$ns
    textOutput(ns('counter'))
  })
  ## Contador de tiempo hasta logout automático
  counter_sec <- 60
  counter_max <- 60 * 60
  rv <- reactiveValues(
    tic = Sys.time(),
    remaining = 0
  )
  ## El contador se resetea si se hace login, logout, run, reset o login Teradata
  observeEvent(user_auth() + active() + input$button + is_running(), {
    rv$tic <- Sys.time()
  })
  ## Se restan un contador cada counter_sec
  time_check_interval <- ifelse(gl$is_dev, 60000, 1000)
  timer <- reactiveTimer(time_check_interval)
  observeEvent(timer(), {
    rv$remaining <- counter_max - as.numeric(difftime(Sys.time(), rv$tic, units = 'secs'))
    if (rv$remaining <= 0) {
      shinyalert(
        title = lang$auto_logout_title,
        type = 'info'
      )
      session$close()
    }
  })
  output$counter <- renderText({
    s <- ifelse(rv$remaining < counter_sec, '"', "'")
    sprintf(
      "%d%s",
      ifelse(
        rv$remaining < counter_sec,
        rv$remaining %/% 1,
        rv$remaining %/% counter_sec
      ),
      s
    )
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


# Panel de administración ----------------------------------------------

## UI
managementUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns('management'))
}

## Server
managementServer <- function(input, output, session, credentials) {
  output$management <- shiny::renderUI({
    ns <- session$ns
    actions <- c('add', 'update_password', 'update_role', 'delete')
    names(actions) <- c(lang$add, lang$update_password, lang$update_role, lang$delete)
    users_panel <- tabPanel(
      value = 'users',
      title = lang$users,
      shiny::wellPanel(
        id = ns('users'),
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
    )
    if (gl$is_dev) {
      login <- shiny::tagList(
        shiny::textInput(ns('db2_user'), lang$user),
        shiny::passwordInput(ns('db2_password'), lang$db2_password)
      )
    } else {
      login <- NULL
    }
    data_panel <- tabPanel(
      value = 'data',
      title = lang$data,
      shiny::tagList(
        h3(lang$rrp_sync_info),
        login,
        shiny::actionButton(ns('update_rrp'), lang$update_rrp, icon = icon('redo-alt'))
      )
    )
    if (!('owner' %in% credentials()$role)) {
      data_panel <- NULL
    }
    tabBox(
      width = '100%',
      users_panel,
      data_panel
    )
  })
  
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
  
  shiny::observeEvent(eventExpr = input$update_rrp, handlerExpr = {
    req(!gl$is_dev)
    if (file.exists(gl$rrp_sync_database)) {
      last_updated <- file.info(gl$rrp_sync_database)$mtime
    } else {
      last_updated <- as.POSIXct('1900-01-01 00:00:00')
    }
    current_time <- Sys.time()
    elapsed_days <- as.numeric(difftime(current_time, last_updated, units = 'days'))
    flog.info(toJSON(list(
      session_info = msg_cred(credentials()),
      message = 'CHECKING AGE OF RRP INFO FILE',
      details = list(
        last_updated = last_updated,
        timestamp = current_time,
        elapsed_days = elapsed_days
      )
    )))
    if (elapsed_days > gl$rrp_sync_update_period) {
      flog.info(toJSON(list(
        session_info = msg_cred(credentials()),
        message = 'TRIGGERING RRP INFO UPDATE AUTOMATICALLY',
        details = list(
          last_updated = last_updated,
          timestamp = current_time,
          elapsed_days = elapsed_days
        )
      )))
      update_rrp_info(ch = NULL, credentials(), 'db2-production-connector')
    }
  }, ignoreNULL = FALSE, once = TRUE)
  
  shiny::observeEvent(input$update_rrp, {
    ns <- session$ns
    if (gl$is_dev) {
      shinyalert(
        type = 'info',
        title = 'Iniciando sesión en DB2...',
        closeOnEsc = FALSE,
        closeOnClickOutside = FALSE,
        showCancelButton = FALSE,
        showConfirmButton = FALSE
      )
      flog.info(toJSON(list(
        session_info = msg_cred(credentials()),
        message = 'ATTEMPTING TO LOG IN INTO DB2',
        details = list(
          user = input$db2_user
        )
      )))
      tryCatch({
        ch <- odbcConnect("DSN5", uid = input$db2_user, pwd = input$db2_password)
        odbcGetInfo(ch) ## Truena si no se abrió la conexión
        flog.info(toJSON(list(
          session_info = msg_cred(credentials()),
          message = 'DB2 LOGIN SUCCESSFUL',
          details = list(
            user = input$db2_user
          )
        )))
        # Run query & update file
        flog.info(toJSON(list(
          session_info = msg_cred(credentials()),
          message = 'TRIGGERING RRP INFO DEV UPDATE MANUALLY',
          details = list()
        )))
        update_rrp_info(ch, credentials(), connector = NULL)
        odbcClose(ch)
        flog.info(toJSON(list(
          session_info = msg_cred(credentials()),
          message = 'DB2 LOGOUT SUCCESSFUL',
          details = list(
            user = input$db2_user
          )
        )))
        updateTextInput(session, 'db2_user', value = '')
        updateTextInput(session, 'db2_password', value = '')
      }, error = function(e){
        shinyalert(
          type = "error",
          title = lang$error,
          text = "El usuario o la contraseña no son válidos",
          closeOnClickOutside = TRUE
        )
        flog.warn(toJSON(list(
          session_info = msg_cred(credentials()),
          message = 'DB2 LOGIN FAILED',
          details = list(
            user = input$db2_user
          )
        )))
        updateTextInput(session, 'db2_password', value = '')
      })
    } else {
      flog.info(toJSON(list(
        session_info = msg_cred(credentials()),
        message = 'TRIGGERING RRP INFO PROD UPDATE MANUALLY',
        details = list()
      )))
      update_rrp_info(ch = NULL, credentials(), 'db2-production-connector')      
    }
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



