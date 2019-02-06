
## Leer entrada
parse_input <- function(input_file, gl) {
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
      x[[v]] <- as(x[[v]], gl$cols[[v]])
    }
    if (anyNA(x)) {
      flog.info('MISSING VALUES PRESENT. INPUT PARSING ABORTED.')
      return(NULL)
    }
    if (!validate_input(x, gl)) {
      flog.info('INVALID INPUT. INPUT PARSING ABORTED.')
      return(NULL)
    }
    prepare_input(x)
  }, error = function(e){
    NULL
  })
}


## Validar inputs
validate_input <- function(data, gl) {
  # browser()
  if (
    ## Condiciones básicas
    !is.data.frame(data) ||
    nrow(data) == 0 ||
    length(setdiff(names(gl$cols), names(data))) > 0 ||
    any(map_chr(data[names(gl$cols)], class) != gl$cols)
  ) {
    FALSE
  } else {
    cond <- list()
    ## Checar las columnas que deben ser constantes por feature
    cond[[1]] <- data %>% 
      group_by(feature_nbr) %>% 
      summarise_at(gl$feature_const_cols, funs(length(unique(.)))) %>% 
      ungroup() %>% 
      select_at(gl$feature_const_cols) %>% 
      equals(1) %>% 
      all()
    ## Checar los negocios
    cond[[2]] <- all(data$negocio %in% gl$negocios)
    ## No se deben repetir artículos por feature
    cond[[3]] <- data %>% 
      group_by(feature_nbr) %>% 
      summarise(n_dups = sum(duplicated(old_nbr))) %>% 
      pull(n_dups) %>% 
      sum() %>% 
      equals(0)
    all(unlist(cond))
  }
}

## Preparar inputs
prepare_input <- function(data) {
  ## Requiere que data haya pasado validate_input
  data %>% 
    mutate(
      display_key = paste(dept_nbr, old_nbr, negocio, sep = '.')
    )
}

## Correr query
prepare_query <- function(query, keys, wk_inicio, wk_final) {
  # browser()
  query %>% 
    str_replace_all('\\?KEY', paste0("'", paste(keys, collapse = "','"), "'")) %>% 
    str_replace_all('\\?WK_INICIO', as.character(wk_inicio)) %>% 
    str_replace_all('\\?WK_FINAL', as.character(wk_final)) %>% 
    paste(collapse = '\n')
}
run_query_once <- function(ch, input_data) {
  # browser()
  wk_inicio <- unique(input_data$semana_ini)
  wk_final <- unique(input_data$semana_fin)
  keys <- input_data$display_key
  type <- unique(input_data$fcst_or_sales)
  if (toupper(type) == 'F') {
    query <- read_lines('sql/exhibiciones-fcst.sql')
  } else {
    query <- read_lines('sql/exhibiciones-pos.sql')
  }
  query <- prepare_query(query, keys, wk_inicio, wk_final)
  tryCatch({
    res <- sqlQuery(ch, query) %>% 
      as_tibble() %>% 
      set_names(tolower(names(.))) %>% 
      mutate_if(is.factor, as.character)
    input_data %>% 
      left_join(res)
  }, error = function(e){
    NULL
  })
}
# ch <- odbcDriverConnect(sprintf("Driver={Teradata};DBCName=WMG;UID=f0g00bq;AUTHENTICATION=ldap;AUTHENTICATIONPARAMETER=%s", rstudioapi::askForPassword()))
run_query <- function(ch, input_data) {
  res <- input_data %>% 
    mutate(rango_semanas = paste(semana_ini, semana_fin, sep = '-')) %>% 
    split(., .$rango_semanas) %>% 
    map(safely(function(x){
      run_query_once(ch, x)
    })) %>% 
    map('result') %>% 
    discard(is.null)
  if (length(res) > 0) {
    res <- bind_rows(res)
  } else {
    res <- NULL
  }
  res
}

## Lógica en R
perform_computations <- function(data) {
  Sys.sleep(1)
  data
}

## Función para encadenar condiciones dentro de validate()
`%then%` <- shiny:::`%OR%`