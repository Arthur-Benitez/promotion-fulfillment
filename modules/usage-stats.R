
require(tidyverse)
require(lubridate)
require(jsonlite)

remap_text <- c(
  'user' = 'unique_users',
  'session' = 'unique_sessions'
)

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
    user = map(logs_ls, c('session_info', 'user')) %>% as.character() %>% ifelse(. == 'NULL', NA, .),
    role = map(logs_ls, c('session_info', 'role')),
    session = map(logs_ls, c('session_info', 'session')) %>% as.character() %>% ifelse(. == 'NULL', NA, .),
    message = map_chr(logs_ls, 'message'),
    details = map(logs_ls, 'details')
  )
}

# 
# ll <- list.files('dev/log/', full.names = TRUE) %>% 
#   str_subset('\\.log$')
# 
# microbenchmark::microbenchmark(
#   current = load_log1(ll),
#   
# )

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
        lines_read = length(logs_ls)
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
      filter(!is.na(!!rlang::sym(input$variable))) %>% 
      filter(date >= min(input$date_range[[1]]) & date <= max(input$date_range[[2]]))
  })
  
  output$graph_daily <- renderPlotly({
    if (is.null(logs_filt()) || nrow(logs_filt()) == 0) {
      p <- plot_ly() %>% 
        add_text(x = 0, y = 0, text = ':(', textfont = list(size = 80)) %>% 
        layout(
          title = lang$title_error,
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
    } else {
      if (input$split_by_clearance) {
        x <- logs_filt() %>% 
          mutate(
            color = names(gl$clearance_levels)[match(map_dbl(role, ~role_clearance(.x, gl$clearance_levels)), gl$clearance_levels)]
          )
      } else {
        x <- logs_filt() %>% 
          mutate(
            color = 'all'
          )
      }
      pal <- c(
        all = rgb(0, 56, 150, maxColorValue = 255),
        owner = rgb(26, 117, 207, maxColorValue = 255),
        admin = rgb(253, 187, 48, maxColorValue = 255),
        basic = rgb(51, 115, 33, maxColorValue = 255)
      )
      p <- x %>%
        mutate(
          x = floor_date(date, unit = input$unit, week_start = 1),
          color = factor(color, levels = c('all', 'owner', 'admin', 'basic'))
        ) %>% 
        group_by(x, color) %>% 
        summarise(
          n_unique = n_distinct(!!rlang::sym(input$variable))
        ) %>% 
        mutate(
          text = sprintf('%s: %s', get_pretty_names(remap_text[input$variable]), scales::comma(n_unique))
        ) %>% 
        plot_ly(x = ~x, y = ~n_unique) %>% 
        add_bars(color = ~color, text = ~text, colors = pal) %>%
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
  
  output$logs_table <- DT::renderDataTable({
    logs_filt() %>% 
      mutate_all(function(x){
        if (is.atomic(x)) {
          x
        } else {
          map_chr(x, toJSON)
        }
      })
  },
  options = list(
    filter = 'top',
    scrollX = TRUE,
    scrollY = '400px'
  ))
}

usageStatsUI <- function(id) {
  ns <- shiny::NS(id)
  fluidPage(
    box(
      width = 12,
      actionButton(ns('refresh'), lang$refresh, icon = icon('redo-alt'))
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
              uiOutput(ns('date_range_ui')),
              selectInput(ns('variable'), lang$kpi, c('user', 'session') %>%
                            set_names(get_pretty_names(remap_text[.]))),
              selectInput(ns('unit'), lang$unit, c('days', 'weeks', 'months', 'years') %>%
                            set_names(get_pretty_names(.))),
              checkboxInput(ns('split_by_clearance'), lang$split_by_clearance, FALSE)
            ),
            column(
              width = 9,
              plotlyOutput(ns('graph_daily')) %>% withSpinner(type = 8)
            )
          )
        ),
        tabPanel(
          title = 'Tabla',
          DTOutput(ns('logs_table')) %>% withSpinner(type = 8)
        )
      )
    )
  )
}