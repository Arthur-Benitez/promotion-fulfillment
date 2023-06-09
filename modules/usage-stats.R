
require(tidyverse)
require(lubridate)
require(jsonlite)
require(plotly)
require(viridis)


generate_empty_plot <- function(title = 'Error', text = ':(') {
  plot_ly() %>% 
    add_text(x = 0, y = 0, text = text, textfont = list(size = 80)) %>% 
    layout(
      title = title,
      titlefont = list(size = 30),
      xaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      ),
      yaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      ),
      margin = list(t = 60)
    )
}

load_log <- function(log_files) {
  logs_ls <- log_files %>% 
    map(read_lines) %>% 
    do.call(c, .) %>%
    str_replace('^([A-Z]+) \\[([0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2})\\] \\{(.*)\\}', '{"level":["\\1"], "timestamp":["\\2"], \\3}') %>% 
    map(function(x){
      tryCatch({
        fromJSON(x)
      }, error = function(e){
        NULL
      })
    }) %>% 
    discard(is.null)
  
  tibble(
    level = map_chr(logs_ls, 'level'),
    timestamp = ymd_hms(format(ymd_hms(map_chr(logs_ls, 'timestamp')), tz = 'America/Mexico_City')),
    date = as_date(timestamp),
    user = map(logs_ls, c('session_info', 'user')) %>% as.character() %>% ifelse(. == 'NULL', NA, .) %>% tolower(),
    role = map(logs_ls, c('session_info', 'role')),
    clearance = map_dbl(role, ~role_clearance(.x, gl$clearance_levels)),
    top_role =  names(gl$clearance_levels)[match(clearance, gl$clearance_levels)],
    session = map(logs_ls, c('session_info', 'session')) %>% as.character() %>% ifelse(. == 'NULL', NA, .),
    platform = map(logs_ls, c('session_info', 'platform')) %>% as.character() %>% ifelse(. == 'NULL', NA, .),
    message = map_chr(logs_ls, 'message'),
    details = map(logs_ls, 'details')
  )
}


usageStatsServer <- function(input, output, session, credentials, dev_connection) {
  
  log_path <- paste0(gl$app_deployment_environment, '/log/')
  
  logs <- eventReactive(input$refresh, {
    flog.info(toJSON(list(
      session_info = msg_cred(credentials()),
      message = 'START REFRESHING LOGS',
      details = list()
    )))
    res <- list.files(log_path, full.names = TRUE) %>% 
      str_subset('\\.log$') %>% 
      keep(~Sys.Date() - as.Date(str_replace(basename(.x), '\\.log', '')) <= input$max_days_to_load) %>% 
      load_log()
    flog.info(toJSON(list(
      session_info = msg_cred(credentials()),
      message = 'DONE REFRESHING LOGS',
      details = list(
        lines_read = nrow(logs)
      )
    )))
    res
  }, ignoreNULL = FALSE)
  
  output$date_range_ui <- renderUI({
    ns <- session$ns
    dateRangeInput(
      inputId = ns('date_range'),
      label = 'Rango de fechas',
      min = min(logs()$date),
      max = max(logs()$date),
      start = max(
        floor_date(Sys.Date(), unit = 'month'),
        min(logs()$date)
      ),
      end = max(logs()$date)
    )
  })
  
  output$update_user_info_ui <- renderUI({
    req('owner' %in% credentials()$role)
    actionButton(session$ns('update_user_info'), lang$update_user_info, icon = icon('redo-alt'))
  })
  # checkpoint
  ## Actualizar base de usuarios
  update_user_info <- reactiveVal(0)
  update_user_df <- reactiveVal(0)
  ### Trigger automático periódico (el ignoreInit = FALSE hace que también corra
  ### en producción)
  observeEvent(dev_connection()$is_open, {
    if (file.exists('data/user-info.rds')) {
      last_updated <- file.info('data/user-info.rds')$mtime
    } else {
      last_updated <- as.POSIXct('1900-01-01 00:00:00')
    }
    current_time <- Sys.time()
    elapsed_days <- as.numeric(difftime(current_time, last_updated, units = 'days'))
    flog.info(toJSON(list(
      session_info = msg_cred(credentials()),
      message = 'CHECKING AGE OF USER INFO FILE',
      details = list(
        last_updated = last_updated,
        timestamp = current_time,
        elapsed_days = elapsed_days
      )
    )))
    if (elapsed_days > 7) {
      flog.info(toJSON(list(
        session_info = msg_cred(credentials()),
        message = 'TRIGGERING USER INFO UPDATE AUTOMATICALLY',
        details = list(
          last_updated = last_updated,
          timestamp = current_time,
          elapsed_days = elapsed_days
        )
      )))
      update_user_info(update_user_info() + 1)
    }
  }, ignoreInit = FALSE, ignoreNULL = TRUE)
  ### Trigger manual
  observeEvent(input$update_user_info, {
    flog.info(toJSON(list(
      session_info = msg_cred(credentials()),
      message = 'TRIGGERING USER INFO UPDATE MANUALLY',
      details = list()
    )))
    update_user_info(update_user_info() + 1)
  }, ignoreInit = FALSE, ignoreNULL = TRUE)
  ### Ahora sí actualizar
  observeEvent(update_user_info(), ignoreInit = FALSE, handlerExpr = {
    req(update_user_info() > 0)
    req(
      !gl$is_dev ||
        (gl$is_dev && dev_connection()$is_open)
    )
    qry <- read_lines('sql/user-info.sql') %>% 
      paste(collapse = '\n')
    tryCatch({
      flog.info(toJSON(list(
        session_info = msg_cred(credentials()),
        message = 'START UPDATING USER INFO',
        details = list()
      )))
      shinyalert(
        title = 'Actualizando base de datos de usuarios...',
        text = '',
        type = 'info',
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE
      )
      res <- sql_query(
        ch = dev_connection()$ch,
        connector = 'WM3', # con un usuario personal porque el aplicativo no puede accesar WM_AD_HOC
        query = qry,
        stringsAsFactors = FALSE
      ) %>% 
        as_tibble() %>%
        set_names(tolower(names(.))) %>% 
        mutate(puesto = str_replace_all(puesto, '_', ' ')) %>% 
        mutate_at(vars(tribu, name, puesto), str_to_title) %>% 
        mutate_at('user', tolower) %>% 
        group_by(user) %>% # por si ya no son únicos al cambiar las mayúsculas
        filter(row_number() == 1) %>% 
        ungroup()
      saveRDS(res, sprintf('data/%s-user-info.rds', format(Sys.time(), '%Y%m%d_%H%M%S')))
      saveRDS(res, 'data/user-info.rds')
      flog.info(toJSON(list(
        session_info = msg_cred(credentials()),
        message = 'DONE UPDATING USER INFO',
        details = list()
      )))
      shinyalert(
        title = 'Base de datos de usuarios actualizada',
        text = '',
        type = 'info',
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE
      )
    }, error = function(e){
      flog.warn(toJSON(list(
        session_info = msg_cred(credentials()),
        message = 'FAILED TO UPDATE USER INFO',
        details = list()
      )))
    })
    update_user_df(update_user_df() + 1)
  })
  
  user_info <- reactive({
    update_user_df()
    tryCatch({
      readRDS('data/user-info.rds')
    }, error = function(e){
      flog.warn(toJSON(list(
        session_info = msg_cred(credentials()),
        message = 'USER INFO NOT FOUND',
        details = list()
      )))
      NULL
    })
  })
  
  user_info_tribu_totals <- reactive({
    if (is.data.frame(user_info())) {
      user_info() %>% 
        group_by(tribu) %>% 
        summarise(
          total_users = n_distinct(user)
        )
    } else {
      tibble(
        tribu = 'Otros',
        total_users = 0
      )
    }
  })
  
  logs_tribu <- reactive({
    if (is.data.frame(user_info())) {
      res <- logs() %>% 
        left_join(user_info(), by = 'user')
    } else {
      res <- logs() %>% 
        mutate(
          tribu = NA,
          name = NA,
          puesto = NA,
        )
    }
    res %>% 
      replace_na(list(tribu = '(N/A)', name = '(N/A)', puesto = '(N/A)', platform = '(N/A)')) %>% 
      mutate(
        user_name = paste0(name, ' (', user, ')')
      )
  })
  
  logs_filt <- reactive({
    req(input$date_range)
    logs_tribu() %>% 
      filter(date >= min(input$date_range[[1]]) & date <= max(input$date_range[[2]])) %>% 
      filter_at(vars(user, message), all_vars(!is.na(.)))
  })
  
  graph_data <- reactiveValues(
    daily = NULL,
    top = NULL,
    time = NULL,
    detail = NULL
  )
  
  output$graph_daily <- renderPlotly({
    if (is.null(logs_filt()) || nrow(logs_filt()) == 0) {
      p <- generate_empty_plot(title = lang$title_error, text = ':(')
      graph_data$daily <- NULL
    } else {
      df <- logs_filt() %>% 
        select(-role) %>% 
        rename(role = top_role)
      if (input$graph_daily_split == 'all') {
        x <- df %>% 
          mutate(
            color = lang$all
          )
      } else {
        x <- df %>% 
          mutate(
            color = !!sym(input$graph_daily_split)
          )
      }
      
      lvls <- switch(
        input$graph_daily_split,
        all = lang$all,
        role = c('all', 'owner', 'admin', 'basic'),
        tribu = sort(unique(x$tribu)),
        puesto = sort(unique(x$puesto)),
        platform = sort(unique(x$platform))
      )
      x <- x %>%
        mutate(
          x = floor_date(timestamp, unit = input$graph_daily_x, week_start = 1),
          color = factor(color, levels = lvls) %>% 
            fct_explicit_na(na_level = '(N/A)')
        ) %>% 
        group_by(x, color) %>% 
        summarise(
          n_users = n_distinct(user),
          n_sessions = n_distinct(session)
        ) %>% 
        ungroup() %>% 
        mutate(
          y = !!sym(input$graph_daily_kpi),
          text = sprintf('%s: %s', get_pretty_names(input$graph_daily_kpi), scales::comma(!!sym(input$graph_daily_kpi)))
        )
      
      pal <- switch(
        input$graph_daily_split,
        all = '#000000',
        role = gl$clearance_pal,
        tribu = extended_colorblind_pal(n_distinct(x$color)) %>%
          set_names(sort(unique(x$color), na.last = TRUE)),
        puesto = extended_colorblind_pal(n_distinct(x$color)) %>%
          set_names(sort(unique(x$color), na.last = TRUE)),
        platform = colorblind_pal()(5) %>% 
          set_names(sort(c('(N/A)', 'sso', 'compass', 'dev', 'unknown')))
      )
      
      ## Datos para descargar
      graph_data$daily <- x %>% 
        transmute(
          date = x,
          !!sym(input$graph_daily_split) := color,
          n_users,
          n_sessions
        ) %>% 
        arrange(desc(date), !!sym(input$graph_daily_split))

      ## Gráfica
      p <- x %>% 
        plot_ly(x = ~x, y = ~y) %>% 
        add_bars(color = ~color, text = ~text, colors = pal) %>%
        layout(
          barmode = 'stack',
          title = get_pretty_names(input$graph_daily_kpi),
          xaxis = list(
            title = ''
          ),
          yaxis = list(
            title = ''
          )
        )
    }
    return(p)
  })
  
  
  output$graph_top <- renderPlotly({
    if (is.null(logs_filt()) || nrow(logs_filt()) == 0) {
      p <- generate_empty_plot(title = lang$title_error, text = ':(')
      graph_data$top <- NULL
    } else {
      df <- logs_filt() %>% 
        filter(top_role %in% input$graph_top_clearance)
      if (input$graph_top_split == 'message') {
        msgs <- df$message %>% as.factor() %>% fct_infreq() %>% levels()
        colorvar <- sym('message')
        pal <- extended_colorblind_pal(length(msgs)) %>% set_names(msgs)
        sort_fun <- fct_infreq
      } else if (input$graph_top_split %in% c('tribu', 'puesto')) {
        colorvar <- sym(input$graph_top_split)
        pal <- extended_colorblind_pal(n_distinct(df[[input$graph_top_split]])) %>% set_names(sort(unique(df[[input$graph_top_split]]), na.last = TRUE))
        sort_fun <- identity
      } else if (input$graph_top_split == 'platform') {
        colorvar <- sym(input$graph_top_split)
        pal <- colorblind_pal()(5) %>% 
          set_names(sort(c('(N/A)', 'sso', 'compass', 'dev', 'unknown')))
        sort_fun <- identity
      } else {
        colorvar <- sym('role')
        pal <- gl$clearance_pal
        sort_fun <- as_mapper(~fct_relevel(.x, names(gl$clearance_levels)))
      }
      
      xvar <- sym(input$graph_top_x)
      kpi <- sym(input$graph_top_kpi)
      x <- df %>%
        select(-role) %>% 
        rename(role = top_role) %>% 
        left_join(user_info_tribu_totals(), by = 'tribu') %>% 
        mutate(
          !!colorvar := factor(!!colorvar) %>% 
            fct_explicit_na('(N/A)') %>% 
            sort_fun()
        ) %>% 
        group_by(!!xvar, !!colorvar) %>% 
        summarise(
          n_actions = n(),
          n_sessions = n_distinct(session),
          n_users = n_distinct(user),
          total_users = mean(total_users)
        ) %>% 
        ungroup() %>% 
        mutate(
          tribu_flag = input$graph_top_split == 'tribu' & input$graph_top_x == 'tribu',
          p_active_users = ifelse(tribu_flag, n_users / total_users, 0),
          x = fct_reorder(!!xvar, !!kpi, .fun = sum, .desc = TRUE),
          y = !!kpi,
          color = !!colorvar,
          text = sprintf(
            '%s\n%s: %s\n%s: %s\n%s: %s\n%s: %s', 
            !!colorvar,
            lang$n_users, scales::comma(n_users),
            lang$p_active_users, scales::percent(p_active_users),
            lang$n_sessions, scales::comma(n_sessions),
            lang$n_actions, scales::comma(n_actions)
          )
        )
     
      ## Datos para descargar
      graph_data$top <- x %>% 
        select(!!xvar, !!colorvar, starts_with('n_'), starts_with('p_')) %>% 
        {
          y <- .
          if (nrow(distinct(y, !!xvar, !!colorvar)) == nrow(distinct(y, !!xvar))) {
            arrange(y, desc(!!kpi))
          } else {
            y %>% 
              group_by(!!xvar) %>% 
              mutate(x_kpi = sum(!!kpi)) %>% 
              arrange(desc(x_kpi), !!xvar, desc(!!kpi)) %>% 
              select(-x_kpi) %>% 
              ungroup()
          }
        }
        
      ## Gráfica
      
      y <- x %>% 
        filter(dense_rank(x) <= input$graph_top_nbar)
      p <- y %>% 
        plot_ly(x = ~x, y = ~y, hoverinfo = 'text') %>% 
        add_bars(color = ~color, text = ~text, colors = pal) %>%
        layout(
          barmode = 'stack',
          title = get_pretty_names(input$graph_top_kpi),
          xaxis = list(
            title = ''
          ),
          yaxis = list(
            title = '',
            tickformat = ifelse(substr(input$graph_top_kpi, 1, 2) == 'p_', '%', ',d')
          ),
          margin = list(
            b = max(nchar(as.character(y$x))) * 7 * sin(30 / 180 * pi) # Necesario porque el auto-height de plotly no considera los ticks con ángulo; 7 = avg character width; sin(...) = corrección por el ángulo de 30 grados
          )
        )
    }
    return(p)
  })
  
  output$graph_time_event <- renderUI({
    ns <- session$ns
    selectInput(
      ns('graph_time_event'),
      label = lang$graph_time_event,
      choices = c('query', 'sales_graph') %>%
        set_names(lang$graph_time_event_names)
    )
  })
  
  output$graph_time <- renderPlotly({
    req(input$graph_time_event)
    msg1 <- switch(
      input$graph_time_event,
      query = 'RUNNING QUERY',
      sales_graph = 'RUNNING SALES GRAPH QUERY'
    )
    msg2 <- switch(
      input$graph_time_event,
      query = 'DOWNLOAD SUCCESSFUL',
      sales_graph = 'GENERATING SALES GRAPH'
    )
    if (
      !is.data.frame(logs_filt()) || 
      nrow(logs_filt()) == 0 || 
      !all(c(msg1, msg2) %in% logs_filt()$message)
    ) {
      p <- generate_empty_plot(title = lang$title_error, text = ':(')
      graph_data$time <- NULL
    } else {
      x <- logs_filt() %>%
        filter(message %in% c(msg1, msg2)) %>% 
        transmute(
          x = date, #floor_date(date, unit = input$graph_daily_x, week_start = 1),
          timestamp,
          session,
          message = factor(message, levels = c(msg1, msg2))
        ) %>% 
        arrange(x, timestamp) %>% 
        group_by(x, session) %>% 
        mutate(
          event_flag = as.numeric(message == msg1),
          event_group = cumsum(event_flag)
        ) %>% 
        group_by(x, session, event_group) %>% 
        filter(row_number() <= 2) %>% # por si se repite el segundo mensaje
        ungroup() %>% 
        select(x, timestamp, session, event_group, message) %>% 
        spread(message, timestamp) %>% 
        mutate(
          duration_secs = as.numeric(difftime(!!sym(msg2), !!sym(msg1), units = 'secs'))
        ) %>% 
        set_names(str_replace_all(names(.), ' ', '_')) %>% 
        na.omit()
      
      graph_data$time <- x %>% 
        rename(date = x)
      
      lns <- list(
        list(
          x0 = min(x$x),
          x1 = max(x$x),
          y0 = 120,
          y1 = 120,
          line = list(color = "black", dash = "dash"),
          type = "line"
        )
      )
      p <- x %>% 
        plot_ly(x = ~x, y = ~duration_secs) %>% #, hoverinfo = 'text') %>% 
        add_boxplot() %>%
        layout(
          title = lang$graph_time_title,
          xaxis = list(
            title = ''
          ),
          yaxis = list(
            title = ''
          ),
          shapes = lns
        )
    }
    return(p)
  })
  
  output$detail_table <- DT::renderDataTable({
    graph_data$detail <- logs_filt() %>% 
      mutate_all(function(x){
        if (is.atomic(x)) {
          x
        } else {
          map_chr(x, toJSON)
        }
      }) %>%
      arrange(desc(timestamp))
    graph_data$detail %>% 
      generate_basic_datatable(gl$cols, scrollX = TRUE, scrollY = gl$table_height$tall)
  })
  
  output$daily_table <- DT::renderDataTable({
    req(graph_data$daily)
    graph_data$daily %>% 
      generate_basic_datatable(gl$cols, scrollY = gl$table_height$short)
  })
  
  output$top_table <- DT::renderDataTable({
    req(graph_data$top)
    graph_data$top %>% 
      generate_basic_datatable(gl$cols, scrollY = gl$table_height$short)
  })
  
  output$time_table <- DT::renderDataTable({
    req(graph_data$time)
    graph_data$time %>% 
      generate_basic_datatable(gl$cols, scrollY = gl$table_height$short)
  })
  
  output$download_detail <- downloadHandler(
    filename = function(){
      sprintf('%s-detalle.csv', format(Sys.time(), '%F_%H-%M-%S'))
    },
    content = function(file){
      graph_data$detail %>% 
        write_excel_csv(file, na = '')
    },
    contentType = 'text/csv'
  )
  
  output$download_daily <- downloadHandler(
    filename = function(){
      sprintf('%s-tendencias.csv', format(Sys.time(), '%F_%H-%M-%S'))
    },
    content = function(file){
      graph_data$daily %>% 
        write_excel_csv(file, na = '')
    },
    contentType = 'text/csv'
  )
  
  output$download_top <- downloadHandler(
    filename = function(){
      sprintf('%s-usuarios-top.csv', format(Sys.time(), '%F_%H-%M-%S'))
    },
    content = function(file){
      graph_data$top %>% 
        write_excel_csv(file, na = '')
    },
    contentType = 'text/csv'
  )
  
  output$download_time <- downloadHandler(
    filename = function(){
      sprintf('%s-tiempo-de-ejecucion.csv', format(Sys.time(), '%F_%H-%M-%S'))
    },
    content = function(file){
      graph_data$time %>% 
        write_excel_csv(file, na = '')
    },
    contentType = 'text/csv'
  )
}

usageStatsUI <- function(id) {
  ns <- shiny::NS(id)
  fluidPage(
    box(
      width = 12,
      tags$div(
        class = 'usage-inline-inputs',
        uiOutput(ns('date_range_ui')),
        numericInput(ns('max_days_to_load'), lang$max_days_to_load, value = 90, min = 1, max = 730, step = 1),
        tags$div(
          class = 'inline-select-button-wrapper',
          actionButton(ns('refresh'), lang$refresh, icon = icon('redo-alt'))
        ),
        tags$div(
          class = 'inline-select-button-wrapper',
          uiOutput(ns('update_user_info_ui'))
        )
      )
    ),
    box(
      width = 12,
      tabBox(
        width = 12,
        tabPanel(
          title = lang$graph_daily_tab,
          fluidRow(
            column(
              width = 3,
              selectInput(ns('graph_daily_x'), lang$graph_daily_x, c('hour', 'day', 'week', 'month', 'year') %>%
                            set_names(get_pretty_names(.)), selected = 'week'),
              selectInput(
                ns('graph_daily_split'),
                label = lang$graph_daily_split,
                choices = c('role', 'tribu', 'puesto', 'platform', 'all') %>% 
                  set_names(map_chr(., ~lang[[.x]]))
              ),
              selectInput(ns('graph_daily_kpi'), lang$kpi, c('n_users', 'n_sessions') %>%
                            set_names(get_pretty_names(.))),
              downloadButton(ns('download_daily'), lang$download)
            ),
            column(
              width = 9,
              plotlyOutput(ns('graph_daily'), height = gl$plotly_height) %>% withSpinner(type = 8),
              DT::DTOutput(ns('daily_table')) %>% withSpinner(type = 8)
            )
          )
        ),
        tabPanel(
          title = lang$graph_top_tab,
          fluidRow(
            column(
              width = 3,
              selectInput(
                ns('graph_top_x'),
                label = lang$graph_top_x,
                choices = c('user_name', 'tribu', 'puesto', 'platform') %>% 
                  set_names(map_chr(., ~lang[[.x]]))
              ),
              selectInput(
                ns('graph_top_split'),
                label = lang$graph_top_split,
                choices = c('role', 'tribu', 'puesto', 'platform', 'message') %>% 
                  set_names(map_chr(., ~lang[[.x]]))
              ),
              selectInput(
                ns('graph_top_kpi'),
                label = lang$kpi,
                choices = c('n_sessions', 'n_actions', 'n_users', 'p_active_users') %>% 
                  set_names(map_chr(., ~lang[[.x]]))
              ),
              selectInput(
                ns('graph_top_clearance'),
                label = lang$graph_top_clearance,
                choices = names(gl$clearance_levels),
                selected = names(gl$clearance_levels),
                multiple = TRUE
              ),
              sliderInput(
                ns('graph_top_nbar'),
                label = lang$graph_top_nbar,
                min = 5,
                max = 30,
                step = 1,
                value = 20
              ),
              downloadButton(ns('download_top'), lang$download)
            ),
            column(
              width = 9,
              plotlyOutput(ns('graph_top'), height = gl$plotly_height) %>% withSpinner(type = 8),
              DT::DTOutput(ns('top_table')) %>% withSpinner(type = 8)
            )
          )
        ),
        tabPanel(
          title = lang$graph_time_tab,
          fluidRow(
            column(
              width = 3,
              uiOutput(ns('graph_time_event')),
              downloadButton(ns('download_time'), lang$download)
            ),
            column(
              width = 9,
              plotlyOutput(ns('graph_time'), height = gl$plotly_height) %>% withSpinner(type = 8),
              DT::DTOutput(ns('time_table')) %>% withSpinner(type = 8)
            )
          )
        ),
        tabPanel(
          title = lang$detail_table_tab,
          tags$div(
            style = 'margin-bottom: 15px;',
            downloadButton(ns('download_detail'), lang$download)
          ),
          DT::DTOutput(ns('detail_table')) %>% withSpinner(type = 8)
        )
      )
    )
  )
}