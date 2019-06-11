
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

## Generar input de ejemplo que siempre funcione
generate_sample_input <- function(calendar_day) {
  fcst_wks <- calendar_day %>% 
    filter(date >= Sys.Date() + 0 & date <= Sys.Date() + 28) %>% 
    pull(wm_yr_wk) %>%
    range()
  sales_wks <- calendar_day %>% 
    filter(date >= Sys.Date() - 90 & date <= Sys.Date() - 60) %>% 
    pull(wm_yr_wk) %>%
    range()
  tibble(
    feature_name = c('MARUCHAN', 'MARUCHAN', 'MARUCHAN', 'MARUCHAN', 'MARUCHAN_MINI', 'MARUCHAN_MINI'),
    user = 'm1234xy',
    dept_nbr = 95,
    negocio = 'BAE',
    old_nbr = c(9506783, 9506804, 9574857, 9506748, 9574857, 9506748),
    min_feature_qty = c(600, 600, 600, 600, 300, 300),
    max_feature_qty = c(3000, 3000, 3000, 3000, 2000, 2000),
    max_ddv = 30,
    fcst_or_sales = c('S', 'S', 'S', 'S', 'F', 'F'),
    semana_ini = c(rep(sales_wks[1], 4), rep(fcst_wks[1], 2)),
    semana_fin = c(rep(sales_wks[2], 4), rep(fcst_wks[2], 2)),
    StartDate = c(rep(Sys.Date() + 7, 4), rep(Sys.Date() + 14, 2)),
    EndDate = c(rep(Sys.Date() + 35, 4), rep(Sys.Date() + 49, 2)),
    Priority = 12
  )
}

