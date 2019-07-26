
require(tidyverse)
require(lubridate)
require(jsonlite)
require(plotly)
require(viridis)

remap_text <- c(
  'user' = 'unique_users',
  'session' = 'unique_sessions',
  'n_actions' = 'unique_actions',
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


usageStatsServer <- function(input, output, session, credentials) {
  
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
  
  
  logs_filt <- reactive({
    req(input$date_range)
    logs() %>% 
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
      df <- logs_filt()
      if (input$split_by_clearance) {
        x <- df %>% 
          mutate(
            color = top_role
          )
      } else {
        x <- df %>% 
          mutate(
            color = 'all'
          )
      }
      
      x <- x %>%
        mutate(
          x = floor_date(date, unit = input$unit, week_start = 1),
          color = factor(color, levels = c('all', 'owner', 'admin', 'basic'))
        ) %>% 
        group_by(x, color) %>% 
        summarise(
          n_unique = n_distinct(!!rlang::sym(input$variable))
        ) %>% 
        ungroup() %>% 
        mutate(
          text = sprintf('%s: %s', get_pretty_names(remap_text[input$variable]), scales::comma(n_unique))
        )
      
      ## Datos para descargar
      graph_data$daily <- x %>% 
        transmute(
          date = x,
          role = color,
          !!sym(paste0('n_unique_', input$variable, 's')) := n_unique
        ) %>% 
        arrange(desc(date), role)
      
      ## Gráfica
      p <- x %>% 
        plot_ly(x = ~x, y = ~n_unique) %>% 
        add_bars(color = ~color, text = ~text, colors = gl$clearance_pal) %>%
        layout(
          barmode = 'stack',
          title = get_pretty_names(remap_text[input$variable]),
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
        filter(input$graph_clearance == 'all' | top_role == input$graph_clearance)
      if (input$split_by_message) {
        x <- df %>% 
          mutate(
            color = factor(message) %>% fct_infreq(),
            text = message
          )
        grp <- c('message')
        msgs <- levels(x$color)
        pal <- viridis::viridis_pal()(length(msgs)) %>% set_names(msgs)
      } else {
        x <- df %>% 
          mutate(
            color = top_role,
            text = 'ALL ACTIONS'
          )
        grp <- NULL
        pal <- gl$clearance_pal
      }
      
      x <- x %>%
        group_by(user, role = top_role, !!!syms(grp), color, text) %>% 
        summarise(
          n_actions = n(),
          n_sessions = n_distinct(session)
        ) %>% 
        ungroup() %>% 
        mutate(
          x = fct_reorder(user, !!sym(input$graph_top_kpi), .desc = TRUE),
          y = !!sym(input$graph_top_kpi),
          text = sprintf('%s\nAcciones: %s\nSesiones: %s', text, n_actions, n_sessions)
        )
     
      ## Datos para descargar
      graph_data$top <- x %>% 
        select(user = x, role, !!!syms(grp), n_actions, n_sessions) %>% 
        arrange(user, desc(!!sym(input$graph_top_kpi)))
      
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
            title = ''
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
          x = date, #floor_date(date, unit = input$unit, week_start = 1),
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
    graph_data$top %>% 
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
        style = 'display: flex;',
        tags$div(
          style = 'margin: auto 25px 15px 15px;',
          actionButton(ns('refresh'), lang$refresh, icon = icon('redo-alt'))
        ),
        uiOutput(ns('date_range_ui'))
      )
    ),
    box(
      width = 12,
      tabBox(
        width = 12,
        tabPanel(
          title = 'Tendencia',
          fluidRow(
            column(
              width = 3,
              selectInput(ns('variable'), lang$kpi, c('user', 'session') %>%
                            set_names(get_pretty_names(remap_text[.]))),
              selectInput(ns('unit'), lang$unit, c('days', 'weeks', 'months', 'years') %>%
                            set_names(get_pretty_names(.))),
              checkboxInput(ns('split_by_clearance'), lang$split_by_clearance, TRUE),
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
          title = 'Top',
          fluidRow(
            column(
              width = 3,
              selectInput(ns('graph_clearance'), lang$graph_clearance, c('all', names(gl$clearance_levels))),
              selectInput(
                ns('graph_top_kpi'),
                label = lang$kpi,
                choices = c('n_sessions', 'n_actions') %>% 
                  set_names(c(lang$unique_sessions, lang$unique_actions))
              ),
              sliderInput(
                ns('graph_top_nbar'),
                label = lang$graph_top_nbar,
                min = 5,
                max = 30,
                step = 1,
                value = 20
              ),
              checkboxInput(ns('split_by_message'), lang$split_by_message, FALSE),
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
          title = 'Desempeño',
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
          title = 'Detalle',
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