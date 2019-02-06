
## Leer entrada
parse_input <- function(input_file, cols) {
  tryCatch({
    x <- read_excel(
      path = input_file,
      sheet = 1,
      col_names = TRUE,
      col_types = 'guess',
      guess_max = 50000
    ) %>% 
      set_names(tolower(names(.)))
    for (v in names(x)) {
      x[[v]] <- as(x[[v]], cols[[v]])
    }
    if (anyNA(x)) {
      flog.info('MISSING VALUES PRESENT. INPUT PARSING ABORTED.')
      NULL
    } else {
      x
    }
  }, error = function(e){
    NULL
  })
}

## Validar inputs
validate_input <- function(data, gl) {
  if (
    ## Condiciones básicas
    !is.data.frame(data) ||
    nrow(data) == 0 ||
    length(setdiff(names(gl$cols), names(data))) > 0 ||
    any(map_chr(data[names(gl$cols)], class) != gl$cols)
  ) {
    FALSE
  } else {
    ## Checar las columnas que deben ser constantes por feature
    cond1 <- data %>% 
      group_by(feature_nbr) %>% 
      summarise_at(gl$feature_const_cols, funs(length(unique(.)))) %>% 
      ungroup() %>% 
      select_at(gl$feature_const_cols) %>% 
      equals(1) %>% 
      all()
    ## Checar los formatos
    cond2 <- all(data$formato %in% gl$formatos)
    cond1 && cond2
  }
}

## Correr query
run_query <- function(ch, input_data) {
  
  Sys.sleep(1)
  sqlQuery(ch, 'select top 10 * from mx_cf_vm.calendar_day')
}

## Lógica en R
perform_computations <- function(data) {
  Sys.sleep(1)
  data
}

## Función para encadenar condiciones dentro de validate()
`%then%` <- shiny:::`%OR%`