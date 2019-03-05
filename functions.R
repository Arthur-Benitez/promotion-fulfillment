
## Generar espeficicación de columnas para readr
generate_cols_spec <- function(columns, date_format = '%Y-%m-%d') {
  cs <- cols(.default = col_character())
  for (x in names(columns)) {
    cs$cols[[x]] <- switch(
      columns[x],
      'integer' = col_integer(),
      'numeric' = col_double(),
      'character' = col_character(),
      'date' = col_date(format = date_format)
    )
  }
  cs
}

## Leer entrada
parse_input <- function(input_file, gl, date_format = '%Y-%m-%d') {
  tryCatch({
    flog.info(toJSON(list(
      message = 'PARSING ITEMS FILE',
      details = list(
        file = input_file
      )
    )))
    x <- read_csv(
      file = input_file,
      col_names = TRUE,
      col_types = generate_cols_spec(gl$cols, date_format = date_format)
    ) %>% 
      .[names(gl$cols)] %>% 
      mutate(
        display_key = paste(dept_nbr, old_nbr, negocio, sep = '.'),
        split_var = paste(semana_ini, semana_fin, fcst_or_sales, sep = '-')
      )
    val <- validate_input(x, gl)
    if (isTRUE(val)) {
      flog.info(toJSON(list(
        message = 'INPUT PARSING DONE',
        details = list(
          file = input_file
        )
      )))
      return(x)
    } else {
      flog.info(toJSON(list(
        message = 'INPUT PARSING ABORTED',
        details = list(
          file = input_file,
          reason = val
        )
      )))
      return(val)
    }
  }, error = function(e){
    flog.info(toJSON(list(
      message = 'INPUT PARSING ABORTED',
      details = list(
        file = input_file,
        reason = 'Error parsing file'
      )
    )))
    return('Error leyendo el archivo')
  })
}


## Validar inputs
validate_input <- function(data, gl) {
  if (
    ## Condiciones básicas
    !is.data.frame(data) ||
    nrow(data) == 0 ||
    length(setdiff(names(gl$cols), names(data))) > 0
  ) {
    return(FALSE)
  } else {
    tryCatch({
      cond <- tribble(
        ~message, ~passed,
        ## Checar que no haya valores faltantes
        'No puede haber valores faltantes (blanks)',
        !anyNA(data),
        ## Checar que feature_name sea de longitid <= 22 caracteres (para que en total sean <= 40 para GRS)
        'feature_name no puede tener más de 22 caracteres',
        all(nchar(data$feature_name) <= 22),
        ## Checar que feature_name no tenga espacios
        'feature_name no puede tener espacios',
        all(!str_detect(data$feature_name, ' ')),
        ## Checar las columnas que deben ser constantes por feature
        sprintf('Las siguientes columnas no deben variar para el mismo feature_name: %s',
                 paste(gl$feature_const_cols, collapse = ', ')),
        data %>% 
          group_by(feature_name) %>% 
          summarise_at(gl$feature_const_cols, funs(length(unique(.)))) %>% 
          ungroup() %>% 
          select_at(gl$feature_const_cols) %>% 
          equals(1) %>% 
          all(),
        ## Checar los negociosn
        sprintf('Los formatos de negocio deben ser uno de: %s', paste(gl$negocios, collapse = ', ')),
        all(data$negocio %in% gl$negocios),
        ## No se deben repetir artículos por feature
        'No se deben repetir artículos por feature_name',
        data %>% 
          group_by(feature_name) %>% 
          summarise(n_dups = sum(duplicated(old_nbr))) %>% 
          pull(n_dups) %>% 
          sum() %>% 
          equals(0),
        ## Checar que el tipo sea F ó S
        'fcst_or_sales debe ser F ó S',
        all(toupper(data$fcst_or_sales) %in% c('F', 'S')),
        ## Máximo de renglones
        sprintf('El archivo de entrada no puede tener más de %d renglones', gl$max_input_rows),
        nrow(data) <= gl$max_input_rows,
        ## Máximo de queries a correr
        sprintf('El archivo de entrada no puede tener más de %d combinaciones semana_ini-semana_fin-fcst_or_sales',
                gl$max_input_queries),
        length(unique(data$split_var)) <= gl$max_input_queries,
        ## Semana ini <= semana fin
        'semana_ini debe ser menor o igual a semana_fin',
        with(data, all(semana_ini <= semana_fin))
      )
      failed_idx <- which(!cond$passed)
      if (length(failed_idx) == 0) {
        return(TRUE)
      } else {
        return(cond$message[failed_idx[[1]]])
      }
      return(all(cond))
    }, error = function(e){
      return('Las pruebas fallaron')
    })
  }
}


## Correr query
prepare_query <- function(query, keys, old_nbrs, wk_inicio, wk_final) {
  # browser()
  query %>% 
    str_replace_all('\\?KEY', paste0("'", paste(keys, collapse = "','"), "'")) %>%
    str_replace_all('\\?OLD_NBRS', paste(old_nbrs, collapse = ",")) %>%
    str_replace_all('\\?WK_INICIO', as.character(wk_inicio)) %>% 
    str_replace_all('\\?WK_FINAL', as.character(wk_final)) %>% 
    str_replace_all('[^[:ascii:]]', '') %>% # quitar no ASCII porque truena en producción
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
    group_by(feature_name, store_nbr) %>% 
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
    arrange(feature_name, store_nbr, old_nbr)
  new_columns <- setdiff(names(data), initial_columns)
  data %>% 
    mutate_at(new_columns, funs(replace_na(., 0)))
}

## Tabla de resumen
summarise_data <- function(data, group = c('feature_name', 'cid')) {
  ## Checks
  stopifnot(is.null(group) || all(group %in% c('feature_name', 'store_nbr', 'cid')))
  ## Cambios a combinaciones específicas
  if ('cid' %in% group) {
    group <- c(group, 'old_nbr')
  }
  if (is.null(group)) {
    group <- 'feature_name'
    data <- data %>% 
      mutate(feature_name = 'Total')
  }
  ## Grupos de tabla de salida
  group_order <- c('feature_name', 'store_nbr', 'cid', 'old_nbr')
  grp <- group_order[group_order %in% group]
  ## Variables numéricas de tabla de salida
  vv <- c('avg_sales', 'avg_forecast', 'total_cost', 'total_qty', 'max_feature_qty', 'total_ddv', 'total_vnpk')
  if ('store_nbr' %in% grp) {
    val_vars <- vv
  } else {
    val_vars <- c('n_stores', vv)
  }
  ## Sumarizar
  data_summary <- data  %>%
    group_by(!!!syms(grp)) %>%
    summarise(
      n_stores = n_distinct(store_nbr),
      ## Las ventas ya son promedio, así que sumándolas dan las ventas promedio de una entidad más grande
      avg_sales = ifelse(any(fcst_or_sales == 'S'), sum(avg_dly_pos_or_fcst[fcst_or_sales=='S']), NA_real_),
      avg_forecast = ifelse(any(fcst_or_sales == 'F'), sum(avg_dly_pos_or_fcst[fcst_or_sales=='F']), NA_real_),
      total_cost = sum(store_cost),
      total_qty = sum(feature_qty_fin),
      max_feature_qty = mean(max_feature_qty),
      total_ddv = sum(feature_qty_fin) / sum(avg_dly_pos_or_fcst),
      total_vnpk = sum(vnpk_fin)
    ) %>% 
    ungroup() %>% 
    arrange(!!!syms(grp)) %>% 
    select(!!!syms(grp), !!!syms(val_vars))
  
  return(data_summary)
}

## Tabla de histograma
generate_histogram_data <- function(output_filtered_data, cut_values = seq(0, 1, 0.2)) {
  cut_labels <- paste(
    scales::percent(head(cut_values, -1)),
    scales::percent(cut_values[-1]),
    sep = ' - '
  )
  output_filtered_data %>% 
    summarise_data(group = c('feature_name', 'store_nbr')) %>% 
    ungroup() %>% 
    mutate(
      perc_max_feature_qty = round(total_qty / max_feature_qty, 5),
      perc_max_feature_qty_bin = cut(perc_max_feature_qty,
                                     breaks = cut_values,
                                     labels = cut_labels,
                                     include.lowest = TRUE),
      temp_cost = total_cost, # Creadas para evitar name clashes en el summarise
      temp_qty = total_qty
    ) %>% 
    group_by(perc_max_feature_qty_bin) %>% 
    summarise(
      n_stores = n(),
      total_cost = sum(temp_cost),
      avg_store_cost = mean(temp_cost),
      total_qty = sum(temp_qty),
      avg_store_qty = mean(temp_qty),
      max_feature_qty = mean(max_feature_qty),
      total_ddv = sum(temp_qty) / sum(coalesce(avg_sales, avg_forecast))
    ) %>% 
    ungroup() %>% 
    mutate(
      p_stores = n_stores / sum(n_stores)
    ) %>% 
    select(perc_max_feature_qty_bin, n_stores, p_stores, everything())
}

## Función para encadenar condiciones dentro de validate()
`%then%` <- shiny:::`%OR%`

## Generar el nombre de la promo para GRS
generate_promo_name <- function(dept_nbr, user, feature_name) {
  sprintf('MX_D%d_CM_%s_%s', dept_nbr, toupper(user), feature_name)
}

## Generar el id de tienda en formato para GRS
generate_loc_id <- function(store_nbr) {
  sprintf('MX_WMT_ST_%s', str_pad(store_nbr, 5, 'left', '0'))
}

## Generar el HEADER.csv para cargar al sistema
generate_header <- function(input_data, priority = 15) {
  input_data %>% 
    transmute(
      `*Promotion` = generate_promo_name(dept_nbr, user, feature_name),
      Description = '',
      StartDate,
      EndDate,
      ApprovedSw = 'TRUE',
      AdditiveSw = 'TRUE',
      `CLEANSE HIST` = 'TRUE',
      `REPLACE PRES/DISPLAY` = 'FALSE',
      Priority = priority,
      LiftType = 0,
      Cal = 'DMDWK',
      Lift = 0
    )
}

## Generar el DETAIL.csv para cargar al sistema
generate_detail <- function(output_data) {
  output_data %>% 
    transmute(
      `*Promotion` = generate_promo_name(dept_nbr, user, feature_name),
      `*StartDate` = StartDate,
      `*CID DMDUNIT NBR` = cid,
      `*DMDGroup` = '-',
      `*Loc` = generate_loc_id(store_nbr),
      `PRESENTATION PCT` = 0,
      `PRESENTATION QTY ` = feature_qty_fin,
      `OFFSET START DAYS` = 0,
      `OFFSET END DAYS` = 0,
      `SPMTL ORDER QTY` = 0,
      `DISPLAY QTY` = 0,
    )
}
