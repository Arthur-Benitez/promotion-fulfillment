
require(tidyverse)
require(lubridate)
require(jsonlite)
require(plotly)
require(viridis)

remap_text <- c(
  'user' = 'active_users',
  'session' = 'unique_sessions',
  'n_actions' = 'n_actions',
  'n_sessions' = 'unique_sessions'
)

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
      start = min(logs()$date),
      end = max(logs()$date)
    )
  })
  
  update_user_info_trigger <- reactiveVal(0)
  observeEvent(input$update_user_info, {
    req(input$update_user_info > 0)
    req(
      !gl$is_dev ||
        (gl$is_dev && dev_connection()$is_open)
    )
    qry <- read_lines('sql/usuarios-vp.sql') %>% 
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
      sql_query(
        ch = dev_connection()$ch,
        connector = 'WM3', # con un usuario personal porque el aplicativo no puede accesar WM_AD_HOC
        query = qry,
        stringsAsFactors = FALSE
      ) %>% 
        as_tibble() %>%
        mutate_at(vars(vp, name), str_to_title) %>% 
        mutate_at('user', tolower) %>% 
        distinct() %>% # por si ya no son únicos al cambiar las mayúsculas
        saveRDS('data/user-info.rds')
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
    update_user_info_trigger(update_user_info_trigger() + 1)
  })
  
  user_info <- reactive({
    update_user_info_trigger()
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
  
  user_info_vp_totals <- reactive({
    if (is.data.frame(user_info())) {
      user_info() %>% 
        group_by(vp) %>% 
        summarise(
          total_users = n_distinct(user)
        )
    } else {
      tibble(
        vp = 'Otros',
        total_users = 0
      )
    }
  })
  
  logs_vp <- reactive({
    if (is.data.frame(user_info())) {
      res <- logs() %>% 
        left_join(user_info(), by = 'user')
    } else {
      res <- logs() %>% 
        mutate(
          vp = NA,
          name = NA
        )
    }
    res %>% 
      replace_na(list(vp = 'Otros', name = 'N/A')) %>% 
      mutate(
        user_name = paste0(name, ' (', user, ')')
      )
  })
  
  logs_filt <- reactive({
    req(input$date_range)
    logs_vp() %>% 
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
      
      pal <- switch(
        input$graph_daily_split,
        all = '#000000',
        role = gl$clearance_pal,
        vp = colorblind_pal()(n_distinct(x$vp)) %>%
          set_names(sort(unique(x$vp), na.last = TRUE))
      )
      lvls <- switch(
        input$graph_daily_split,
        all = lang$all,
        role = c('all', 'owner', 'admin', 'basic'),
        vp = sort(unique(x$vp))
      )
      x <- x %>%
        mutate(
          x = floor_date(date, unit = input$graph_daily_x, week_start = 1),
          color = factor(color, levels = lvls)
        ) %>% 
        group_by(x, color) %>% 
        summarise(
          n_unique = n_distinct(!!rlang::sym(input$graph_daily_kpi))
        ) %>% 
        ungroup() %>% 
        mutate(
          text = sprintf('%s: %s', get_pretty_names(remap_text[input$graph_daily_kpi]), scales::comma(n_unique))
        )
      
      ## Datos para descargar
      graph_data$daily <- x %>% 
        transmute(
          date = x,
          !!sym(input$graph_daily_split) := color,
          !!sym(paste0('n_unique_', input$graph_daily_kpi, 's')) := n_unique
        ) %>% 
        arrange(desc(date), !!sym(input$graph_daily_split))

      ## Gráfica
      p <- x %>% 
        plot_ly(x = ~x, y = ~n_unique) %>% 
        add_bars(color = ~color, text = ~text, colors = pal) %>%
        layout(
          barmode = 'stack',
          title = get_pretty_names(remap_text[input$graph_daily_kpi]),
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
        pal <- viridis::viridis_pal()(length(msgs)) %>% set_names(msgs)
        sort_fun <- fct_infreq
      } else if (input$graph_top_split == 'vp') {
        colorvar <- sym('vp')
        pal <- colorblind_pal()(n_distinct(df$vp)) %>% set_names(sort(unique(df$vp), na.last = TRUE))
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
        left_join(user_info_vp_totals(), by = 'vp') %>% 
        mutate(
          !!colorvar := factor(!!colorvar) %>% sort_fun()
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
          vp_flag = input$graph_top_split == 'vp' & input$graph_top_x == 'vp',
          p_active_users = ifelse(vp_flag, n_users / total_users, 0),
          x = fct_reorder(!!xvar, !!kpi, .fun = sum, .desc = TRUE),
          y = !!kpi,
          color = !!colorvar,
          text = sprintf(
            '%s\n%s: %s\n%s: %s\n%s: %s\n%s: %s', 
            !!colorvar,
            lang$active_users, scales::comma(n_users),
            lang$p_active_users, scales::percent(p_active_users),
            lang$unique_sessions, scales::comma(n_sessions),
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
      
      p <- x %>% 
        filter(dense_rank(x) <= input$graph_top_nbar) %>% 
        plot_ly(x = ~x, y = ~y, hoverinfo = 'text') %>% 
        add_bars(color = ~color, text = ~text, colors = pal) %>%
        layout(
          barmode = 'stack',
          title = get_pretty_names(remap_text[input$graph_top_kpi]),
          xaxis = list(
            title = ''
          ),
          yaxis = list(
            title = '',
            tickformat = ifelse(substr(input$graph_top_kpi, 1, 2) == 'p_', '%', ',d')
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
    if (is.null(logs_filt()) || nrow(logs_filt()) == 0) {
      p <- generate_empty_plot(title = lang$title_error, text = ':(')
      graph_data$top <- NULL
    } else {
      df <- logs_filt()
      
      # browser()
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
      x <- df %>%
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
      datatable(
        extensions = c('KeyTable'),
        filter = 'top',
        options = list(
          scrollX = TRUE,
          scrollY = '500px',
          pageLength = 100,
          keys = TRUE
        )
      )
  })
  
  output$daily_table <- DT::renderDataTable({
    graph_data$daily %>% 
      datatable(
        extensions = c('KeyTable'),
        filter = 'top',
        options = list(
          scrollX = FALSE,
          scrollY = '200px',
          pageLength = 100,
          keys = TRUE
        )
      )
  })
  
  output$top_table <- DT::renderDataTable({
    req(graph_data$top)
    decimal_columns <- str_subset(names(graph_data$top), '^n_')
    percent_columns <- str_subset(names(graph_data$top), '^p_')
    graph_data$top %>% 
      mutate_at(percent_columns, ~ 100 * .x) %>% 
      datatable(
        extensions = c('KeyTable'),
        filter = 'top',
        options = list(
          scrollX = FALSE,
          scrollY = '200px',
          pageLength = 100,
          keys = TRUE
        )
      ) %>%
      formatCurrency(columns = decimal_columns, digits = 0, currency = '') %>%
      formatCurrency(columns = percent_columns, digits = 1, currency = '%', before = FALSE) %>% 
      return()
  })
  
  output$time_table <- DT::renderDataTable({
    graph_data$time %>% 
      datatable(
        extensions = c('KeyTable'),
        filter = 'top',
        options = list(
          scrollX = FALSE,
          scrollY = '200px',
          pageLength = 100,
          keys = TRUE
        )
      )
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
        style = 'display: flex; margin-left: 15px',
        tags$div(
          class = 'inline-select-button-wrapper',
          actionButton(ns('refresh'), lang$refresh, icon = icon('redo-alt'))
        ),
        tags$div(
          class = 'inline-select-button-wrapper',
          actionButton(ns('update_user_info'), lang$update_user_info, icon = icon('redo-alt'))
        ),
        uiOutput(ns('date_range_ui'))
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
              selectInput(ns('graph_daily_x'), lang$graph_daily_x, c('days', 'weeks', 'months', 'years') %>%
                            set_names(get_pretty_names(.))),
              selectInput(
                ns('graph_daily_split'),
                label = lang$graph_daily_split,
                choices = c('all', 'role', 'vp') %>% 
                  set_names(lang$all, lang$role, lang$vp)
              ),
              selectInput(ns('graph_daily_kpi'), lang$kpi, c('user', 'session') %>%
                            set_names(get_pretty_names(remap_text[.]))),
              downloadButton(ns('download_daily'), lang$download)
            ),
            column(
              width = 9,
              plotlyOutput(ns('graph_daily')) %>% withSpinner(type = 8),
              tags$hr(),
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
                choices = c('user_name', 'vp') %>% 
                  set_names(c(lang$user, lang$vp))
              ),
              selectInput(
                ns('graph_top_split'),
                label = lang$graph_top_split,
                choices = c('role', 'vp', 'message') %>% 
                  set_names(c(lang$role, lang$vp, lang$message))
              ),
              selectInput(
                ns('graph_top_kpi'),
                label = lang$kpi,
                choices = c('n_sessions', 'n_actions', 'n_users', 'p_active_users') %>% 
                  set_names(c(lang$unique_sessions, lang$n_actions, lang$active_users, lang$p_active_users))
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
              plotlyOutput(ns('graph_top')) %>% withSpinner(type = 8),
              tags$hr(),
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
              plotlyOutput(ns('graph_time')) %>% withSpinner(type = 8),
              tags$hr(),
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