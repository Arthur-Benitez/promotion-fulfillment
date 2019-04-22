
## Función para encadenar condiciones dentro de validate()
`%then%` <- shiny:::`%OR%`

## Renombrar variables para mostrar
get_pretty_names <- function(x) {
  fg::maybe_extract(x, lang)
}
