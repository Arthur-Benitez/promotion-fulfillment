
## Inicializar logger (necesario para reinicializar desde futures)
init_log <- function(log_dir) {
  if (!dir.exists(log_dir)) {
    dir.create(log_dir)
  }
  log_file <- file.path(log_dir, paste0(as.character(Sys.Date()), '.log'))
  futile.logger::flog.logger(
    name = 'ROOT',
    threshold = futile.logger::INFO,
    appender = appender.tee(log_file)
  )
}

## Función para encadenar condiciones dentro de validate()
`%then%` <- shiny:::`%OR%`

## Función para extraer etiquetas con base en una llave, si aplica
maybe_extract <- function(x, mapping){
  stopifnot(is.atomic(x))
  stopifnot(!is.null(names(mapping)))
  was_factor <- is.factor(x)
  x <- as.factor(x)
  idx <- levels(x) %in% names(mapping)
  levels(x)[idx] <- unlist(mapping[match(levels(x)[idx], names(mapping))])
  if (was_factor) {
    x
  } else {
    as.character(x)
  }
} 

## Renombrar variables para mostrar
get_pretty_names <- function(x) {
  maybe_extract(x, lang)
}

## Da formato a una diferencia de tiempos
format_difftime <- function(x) {
  units(x) <- "secs"
  x <- round(as.numeric(x))
  sprintf(
    "%d minutos y %d segundos",
    x %/% 60,
    x %% 60
  )
}

## Run a query in dev or prod
sql_query <- function(ch = NULL, connector = NULL, query, stringsAsFactors = FALSE, ...) {
  if (is.null(ch)) {
    res <- mlutils::dataset.load(name = connector, query = query, ...)
  } else {
    res <- RODBC::sqlQuery(ch, query, stringsAsFactors = stringsAsFactors, ...)
  }
  return(res)
}

## Remap column names
remap_names <- function(columns, column_info, from_col = 'name', to_col = 'pretty_name') {
  maybe_extract(columns, deframe(column_info[c(from_col[1], to_col[1])]))
}

## Get columns with a given format
get_column_formats <- function(column_info, columns, format) {
  intersect(column_info$name[column_info$format == format], columns)
}

## Transform variables before formatting
transform_columns <- function(x, column_info) {
  datetime_columns <- get_column_formats(column_info, names(x), 'datetime')
  x %>% 
    mutate_at(datetime_columns, ~ format(.x, '%F %H:%M:%S'))
}

## Format columns
format_columns <- function(dt, column_info) {
  format_funs <- column_info %>% 
    filter(name %in% names(dt$x$data)) %>% 
    mutate(
      fun = pmap(list(name, format, digits), function(name, format, digits){
        switch(format,
          'character' = ~formatString(.x, columns = name),
          'comma' = ~formatRound(.x, columns = name, digits = digits),
          'currency' = ~formatCurrency(.x, columns = name, digits = digits),
          'percent' = ~formatPercentage(.x, columns = name, digits = digits),
          identity
        )
      })
    ) %>% 
    pull(fun)
  format_fun <- purrr::compose(!!!format_funs)
  format_fun(dt)
}

## Build JS callback to set hover text help
build_callback <- function(titles) {
  JS(sprintf("
    let tips = ['%s'];
    let header = table.columns().header();
    for (let i = 0; i < tips.length; i++) {
      $(header[i]).attr('title', tips[i])
    }
  ", paste(c('', titles), collapse = "', '")))
}

## Generar datatable con parámetros comunes
generate_basic_datatable <- function(x, column_info, scrollX = FALSE, scrollY = '200px') {
  x %>% 
    transform_columns(column_info) %>% 
    datatable(
      extensions = c('KeyTable'),
      filter = 'top',
      options = list(
        scrollX = scrollX,
        scrollY = scrollY,
        pageLength = 100,
        keys = TRUE
      ),
      colnames = remap_names(names(.), column_info, to_col = 'pretty_name'),
      callback = build_callback(remap_names(names(.), column_info, to_col = 'description'))
    ) %>%
    format_columns(column_info)
}
