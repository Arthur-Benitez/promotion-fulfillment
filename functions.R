
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

## Función que inicializa una columna con un valor específico si no existe ya
init_col <- function(data, col, init_value = NA) {
  for (cc in col) {
    if (is.null(data[[cc]])) {
      data[[cc]] <- init_value
    }
  }
  data
}

## Función para que todos los vectores dentro de una lista sean de la misma longitud
fill_vectors <- function(data, value = NA) {
  if (is.data.frame(data)) {
    return(data)
  }
  max_length <- data %>% 
    map_dbl(length) %>% 
    max
  data %>% 
    map(~c(.x, rep(value, max_length - length(.x)))) %>% 
    as_tibble()
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
  
  segs <- sprintf(
    '%d segundos',
    x %% 60
  )
  if (x >= 60) {
    mins <- sprintf(
      '%d minutos y ',
      x %/% 60
    )
  } else {
    mins <- ''
  }
  paste0(mins, segs)
}

## Escala de colores colorblind extendida
extended_colorblind_pal <- function(n) {
  x <- c(ggthemes::colorblind_pal()(8), 'darkblue', 'gray30', 'gray70', 'gray 90', 'red', 'magenta', 'gold', 'darkgreen', 'purple')
  if (length(x) < n) {
    x <- c(x, viridis::viridis_pal(option = 'A')(n - length(x)))
  }
  head(x, n)
}

## Run a query in dev or prod
sql_query <- function(ch = NULL, connector = NULL, query, stringsAsFactors = FALSE, ...) {
  if (is.null(ch)) {
    res <- purrr::safely(mlutils::dataset.load)(name = connector, query = query, ...)
    if (is.data.frame(res$result)) {
      res <- res$result
      if (!stringsAsFactors) {
        res <- res %>%
          dplyr::mutate_if(is.factor, as.character)
      }
    }
  } else {
    res <- RODBC::sqlQuery(ch, query, stringsAsFactors = stringsAsFactors, ...)
    if (!is.data.frame(res)) {
      res <- paste(res, collapse = ' ')
    }
  }
  if (!is.data.frame(res)) {
    errmsg <- sprintf('QUERY FAILED %s', substr(res, 1, 200))
    try(futile.logger::flog.debug(errmsg))
    stop(errmsg)
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
build_callback <- function(columns, column_info) {
  descriptions <- remap_names(columns, column_info, to_col = 'description')
  themes <- remap_names(columns, column_info, to_col = 'theme')

  JS(
    sprintf("
      let descriptions = ['%s'];
      let themes = ['%s'];
      let header = table.columns().header();
      for (let i = 0; i < descriptions.length; i++) {
        $(header[i]).attr('title', descriptions[i]);
        header[i].classList.add(themes[i]);
      }
      ", 
      paste(c('', descriptions), collapse = "', '"),
      paste(c('default', themes), collapse = "', '")
    )
  )
}

## Generar datatable con parámetros comunes
generate_basic_datatable <- function(x, column_info, scrollX = FALSE, scrollY = '30vh') {
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
      callback = build_callback(names(.), column_info)
    ) %>%
    format_columns(column_info)
}
