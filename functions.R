
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
