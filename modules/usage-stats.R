
remap_text <- c(
  'user' = 'unique_users',
  'session' = 'unique_sessions'
)

usageStatsServer <- function(input, output, session, credentials) {
  
  log_path <- paste0(gl$app_deployment_environment, '/log/')
  
  logs <- eventReactive(input$refresh, {
    flog.info(toJSON(list(
      session_info = msg_cred(credentials()),
      message = 'START REFRESHING LOGS',
      details = list()
    )))
    logs_ls <- list.files(log_path, full.names = TRUE) %>% 
      str_subset('\\.log$') %>% 
      map(read_lines) %>% 
      do.call(c, .) %>%
      str_replace('^([A-Z]+) \\[([0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2})\\] \\{(.*)\\}', '{"level":["\\1"], "timestamp":["\\2"], \\3}') %>% 
      map(function(x){
        tryCatch({
          fromJSON(x)
        }, error = function(e){
          flog.info(toJSON(list(
            session_info = list(),
            message = "ERROR READING LOG",
            details = list(
              broken_json = x
            )
          )))
          NULL
        })
      }) %>% 
      discard(is.null)
    flog.info(toJSON(list(
      session_info = msg_cred(credentials()),
      message = 'DONE REFRESHING LOGS',
      details = list(
        lines_read = length(logs_ls)
      )
    )))
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
  
  output$graph_daily <- renderPlot({
    if (is.null(logs_filt()) || nrow(logs_filt()) == 0) {
      p <- ggplot() +
        annotate('text', x = 0, y = 0, label = ':(', size = 20, angle = 270) +
        theme_minimal() +
        theme(
          axis.text = element_blank(),
          line = element_blank()
        ) +
        labs(
          title = lang$title_error,
          x = NULL,
          y = NULL,
          fill = NULL
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
      # browser()
      p <- x %>%
        mutate(
          x = floor_date(date, unit = input$unit, week_start = 1)
        ) %>% 
        group_by(x, color) %>% 
        summarise(
          n_unique = n_distinct(!!rlang::sym(input$variable))
        ) %>% 
        ggplot(aes(x, n_unique)) + 
        geom_col(aes(fill = color), alpha = 0.8) +
        scale_y_continuous(labels = scales::comma) +
        scale_x_date(date_labels = '%Y-%m-%d') +
        scale_fill_colorblind(NULL) +
        theme_bw() +
        theme(
          legend.position = 'top',
        ) +
        labs(
          title = get_pretty_names(remap_text[input$variable]),
          x = NULL,
          y = NULL
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
              plotOutput(ns('graph_daily')) %>% withSpinner(type = 8)
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