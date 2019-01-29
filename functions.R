
## Leer entrada
parse_input <- function(input_file) {
  data <- tryCatch({
    x <- read_excel(
      path = input_file,
      sheet = 1,
      col_names = TRUE,
      col_types = 'guess',
      guess_max = 50000
    ) %>% 
      set_names(tolower(names(.))) %>% 
      mutate_if(is.POSIXct, as.Date)
    na_rows <- which(apply(x, 1, function(y) any(is.na(y))))
    flog.info('OMITTING %s ROWS (%s)', scales::comma(length(na_rows)), paste(head(na_rows, min(10, length(na_rows))), collapse = ', '))
    if (length(na_rows) == 0) {
      x
    } else {
      x[-na_rows, ]
    }
  }, error = function(e){
    NULL
  })
  data
}

## Validar inputs
validate_input <- function(data, cols) {
  if (
    nrow(data) == 0 ||
    !setequal(names(data), names(gl$cols)) ||
    any(map_chr(data[names(cols)], class) != cols)
  ) {
    FALSE
  } else {
    TRUE
  }
}

## Lógica en R
perform_computations <- function(data) {
  data
}