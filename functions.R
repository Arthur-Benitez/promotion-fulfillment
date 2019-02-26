
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
      store_cost = cost * feature_qty_fin,
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
    arrange(feature_nbr, store_nbr, old_nbr)
  new_columns <- setdiff(names(data), initial_columns)
  data %>% 
    mutate_at(new_columns, funs(replace_na(., 0)))
}

## Tabla de resumen
summarise_data <- function(data, level = c('item', 'feature', 'total')) {
  ## Nivel de agregación inicial y final
  level <- level[1]
  grp0 <- c('feature_nbr', 'feature_name', 'cid', 'old_nbr')
  grp <- switch(
    level,
    item = grp0,
    feature = c('feature_nbr', 'feature_name'),
    total = 'feature_name'
  )
  if (level == 'total') {
    data <- data %>% 
      mutate(feature_name = 'Total')
  }
  ## Número de tiendas distintas por grupo
  n_stores <- data %>% 
    group_by(!!!syms(grp)) %>% 
    summarise(
      n_stores = n_distinct(store_nbr)
    ) %>% 
    ungroup()
  ## Sumarizar a nivel grp
  data_summary <- data  %>%
    group_by(!!!syms(grp0), store_nbr) %>%
    summarise(
      sales = ifelse(any(fcst_or_sales == 'S'), sum(avg_dly_pos_or_fcst[fcst_or_sales=='S']), NA_real_),
      forecast = ifelse(any(fcst_or_sales == 'F'), sum(avg_dly_pos_or_fcst[fcst_or_sales=='F']), NA_real_),
      store_cost = sum(store_cost),
      total_vnpk_fin = sum(vnpk_fin)
    ) %>% 
    group_by(!!!syms(grp0)) %>%
    summarise(
      #store_qty = n(), ## Ahora se calcula por separado porque no se puede hacer despuÃ©s del paso previo
      avg_sales = mean(sales, na.rm = TRUE),
      avg_forecast = mean(forecast, na.rm = TRUE),
      total_cost = sum(store_cost),
      avg_store_cost = mean(store_cost),
      total_vnpk = sum(total_vnpk_fin),
      avg_store_vnpk = mean(total_vnpk_fin)
    ) %>% 
    replace(., is.na(.), NA) %>% 
    ungroup() %>% 
    select(-one_of(setdiff(grp0, grp)))
  if (all.equal(grp, grp0) != TRUE) {
    data_summary <- data_summary %>% 
      group_by(!!!syms(grp)) %>% 
      summarise_all(function(x){
        if (all(is.na(x))) {
          NA
        } else {
          sum(x, na.rm = TRUE)
        }
      })
  }
  ## Juntar todo
  data_summary <- data_summary %>% 
    left_join(n_stores, by = grp) %>% 
    ungroup() %>% 
    arrange(!!!syms(grp)) %>% 
    select(!!!grp, n_stores, everything())
  
  return(data_summary)
}

## Función para encadenar condiciones dentro de validate()
`%then%` <- shiny:::`%OR%`
