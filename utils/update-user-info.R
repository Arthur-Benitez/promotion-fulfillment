## This script is intended to run in an Element automated workflow with the objective of automatically and daily, updating the User info database.
## This is necessary because a bug in the Element platform causes an error to prevent queries from being run in Teradata after this query has been run in the server.

library(tidyverse)
library(futile.logger)
library(jsonlite)

app_deployment_environment <- getArgument('app_deployment_environment', 'PARAMS')

if (app_deployment_environment == 'prod') {
  setwd('/home/rstudio/prod/promo-fulfillment-app-produccion')
} else {
  setwd('/home/rstudio/dev/DEV-promo-fulfillment-app')
}

flog.info(toJSON(list(
  message = 'WORKING DIRECTORY SET',
  details = list(
    app_deployment_environment = app_deployment_environment,
    working_dir = getwd()
  )
)))

functions_path <- 'utils/functions.R'
user_database_path <- 'data/user-info.rds'
user_query_path <- 'sql/user-info.sql'
connector <- 'WM3' # con un usuario personal porque el aplicativo no puede accesar WM_AD_HOC
user_backups_n <- 5

# This is necessary because the "backup_file" function is required to save a copy of the database.
tryCatch({
  source(functions_path)  
}, error = function(e){
  flog.error(toJSON(list(
    message = 'FUNCTIONS SOURCE NOT FOUND',
    details = list(
      functions_path = functions_path,
      working_dir = getwd()
    )
  )))
})

query <- readLines(user_query_path) %>% 
  paste(collapse = '\n')

res <- purrr::safely(mlutils::dataset.load)(name = connector, query = query)

if (is.data.frame(res$result)) {
  res <- res$result %>%
    mutate_if(is.factor, as.character) %>% 
    as_tibble() %>% 
    set_names(tolower(names(.))) %>% 
    mutate(puesto = str_replace_all(puesto, '_', ' ')) %>% 
    mutate_at(vars(tribu, name, puesto), str_to_title) %>% 
    mutate_at('user', tolower) %>% 
    group_by(user) %>% # por si ya no son únicos al cambiar las mayúsculas
    filter(row_number() == 1) %>% 
    ungroup()
} else {
  res <- paste(res$error, collapse = ' ')
  flog.error(toJSON(list(
    message = 'QUERY FAILED',
    details = list(
      error_msg = substr(res, 1, 200),
      query = substr(query, 1, 300)
    )
  )))
  stop(res)
}

tryCatch({
  backup_name <- backup_file(
    res,
    folder = dirname(user_database_path),
    complete_file_name = basename(user_database_path),
    preserved_backups = user_backups_n
  )
}, error = function(e){
  flog.error(toJSON(list(
    message = 'BACKUP NOT SUCCESSFUL',
    details = list(
      data = head(res, 2),
      path = user_database_path
    )
  )))
})
saveRDS(res, user_database_path)

flog.info(toJSON(list(
  message = 'DONE UPDATING USER INFO',
  details = list(
    backup_name = backup_name,
    backup_path = file.path(getwd(), dirname(user_database_path))
  )
)))

