
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
    x <- prepare_input(x)
    if (!validate_input(x, gl)) {
      flog.info('INVALID INPUT. INPUT PARSING ABORTED.')
      return(NULL)
    }
    x
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
    return(FALSE)
  } else {
    tryCatch({
      cond <- c(
        ## Checar las columnas que deben ser constantes por feature
        data %>% 
          group_by(feature_nbr) %>% 
          summarise_at(gl$feature_const_cols, funs(length(unique(.)))) %>% 
          ungroup() %>% 
          select_at(gl$feature_const_cols) %>% 
          equals(1) %>% 
          all(),
        ## Checar los negocios
        all(data$negocio %in% gl$negocios),
        ## No se deben repetir artículos por feature
        data %>% 
          group_by(feature_nbr) %>% 
          summarise(n_dups = sum(duplicated(old_nbr))) %>% 
          pull(n_dups) %>% 
          sum() %>% 
          equals(0),
        ## Checar que el tipo sea F ó S
        all(toupper(data$fcst_or_sales) %in% c('F', 'S')),
        ## Máximo de renglones
        nrow(data) <= gl$max_input_rows,
        ## Máximo de queries a correr
        length(unique(data$split_var)) <= gl$max_input_queries,
        ## Semana ini <= semana fin
        with(data, all(semana_ini <= semana_fin))
      )
      
      return(all(cond))
    }, error = function(e){
      return(FALSE)
    })
  }
}

## Preparar inputs
prepare_input <- function(data) {
  ## Requiere que data haya pasado validate_input
  data %>% 
    mutate(
      display_key = paste(dept_nbr, old_nbr, negocio, sep = '.'),
      split_var = paste(semana_ini, semana_fin, fcst_or_sales, sep = '-')
    )
}

## Correr query
prepare_query <- function(query, keys, old_nbrs, wk_inicio, wk_final) {
  # browser()
  query %>% 
    str_replace_all('\\?KEY', paste0("'", paste(keys, collapse = "','"), "'")) %>%
    str_replace_all('\\?OLD_NBRS', paste(old_nbrs, collapse = ",")) %>%
    str_replace_all('\\?WK_INICIO', as.character(wk_inicio)) %>% 
    str_replace_all('\\?WK_FINAL', as.character(wk_final)) %>% 
    paste(collapse = '\n')
}
run_query_once <- function(ch, input_data) {
  # browser()
  wk_inicio <- unique(input_data$semana_ini)
  wk_final <- unique(input_data$semana_fin)
  type <- toupper(unique(input_data$fcst_or_sales))
  if (type == 'F') {
    query <- read_lines('sql/exhibiciones-fcst.sql')
    value <- sym('avg_dly_fcst')
  } else if (type == 'S') {
    query <- read_lines('sql/exhibiciones-pos.sql')
    value <- sym('avg_dly_pos')
  } else { ## No debería pasar si los datos están validados
    return(NULL)
  }
  query <- prepare_query(
    query = query,
    keys = input_data$display_key,
    old_nbrs = input_data$old_nbr,
    wk_inicio = wk_inicio,
    wk_final = wk_final
  )
  tryCatch({
    if (is.null(ch)) {
      res <- mlutils::dataset.load(name = 'WMG', query = query)
    } else {
      res <- sqlQuery(ch, query)
    }
    res <- res %>% 
      as_tibble() %>% 
      set_names(tolower(names(.))) %>% 
      mutate_if(is.factor, as.character) %>% 
      rename(avg_dly_pos_or_fcst = !!value)
    input_data %>% 
      left_join(res)
  }, error = function(e){
    NULL
  })
}
run_query <- function(ch, input_data) {
  res <- input_data %>% 
    split(., .$split_var) %>% 
    map(safely(function(x){
      run_query_once(ch, x)
    })) %>% 
    map('result') %>% 
    discard(is.null)
  if (length(res) > 0) {
    res <- bind_rows(res) %>% 
      replace_na(list(
        avg_dly_pos_or_fcst = 0
      ))
  } else {
    res <- NULL
  }
  res
}

## Lógica en R
perform_computations <- function(data) {
  initial_columns <- names(data)
  data <- data %>% 
    group_by(feature_nbr, store_nbr) %>% 
    mutate(
      feature_perc_pos_or_fcst = avg_dly_pos_or_fcst / sum(avg_dly_pos_or_fcst)
    ) %>% 
    ungroup() %>% 
    mutate(
      feature_qty_req = feature_perc_pos_or_fcst * max_feature_qty,
      feature_ddv_req = feature_qty_req / avg_dly_pos_or_fcst,
      feature_ddv_fin = pmin(feature_ddv_req, max_ddv),
      feature_ddv_bound_active = ifelse(feature_ddv_req > max_ddv, 1, 0),
      feature_qty_fin = feature_ddv_fin * avg_dly_pos_or_fcst,
      store_tot_cost = cost * feature_qty_fin,
      vnpk_fin = feature_qty_fin / vnpk_qty
    ) %>% 
    ungroup() %>% 
    select(
      feature_nbr,
      feature_name,
      store_nbr,
      dept_nbr,
      negocio,
      old_nbr,
      primary_desc,
      max_feature_qty,
      max_ddv,
      semana_ini,
      semana_fin,
      fcst_or_sales,
      avg_dly_pos_or_fcst,
      starts_with('feature_'),
      everything()
    ) %>% 
    arrange(feature_nbr, store_nbr)
  new_columns <- setdiff(names(data), initial_columns)
  data %>% 
    mutate_at(new_columns, funs(replace_na(., 0)))
}

## Tabla de resumen
summarise_data <- function(data) {
  data_summary <- data  %>%
    group_by(cid, old_nbr, feature_nbr, primary_desc) %>%
    summarise(store_qty = n(), ## Número de tiendas
              avg_sales = mean(avg_dly_pos_or_fcst[fcst_or_sales=='S']),
              avg_forecast = mean(avg_dly_pos_or_fcst[fcst_or_sales=='F']),
              total_cost = sum(store_tot_cost),
              avg_cost = mean(store_tot_cost),
              total_vnpk_fin = sum(vnpk_fin))
  ## Obtener totales
  temp <- data_summary %>%
    ungroup() %>% 
    summarise_at(vars(total_cost, avg_cost, total_vnpk_fin, avg_sales, avg_forecast),
                 funs(sum(., na.rm = TRUE)))
  result <- bind_rows(data_summary, temp)
  
  result[nrow(result),] <- result[nrow(result),] %>%
    replace(., is.na(.), "TOTAL")
  
  return(result)
}

## Función para encadenar condiciones dentro de validate()
`%then%` <- shiny:::`%OR%`
