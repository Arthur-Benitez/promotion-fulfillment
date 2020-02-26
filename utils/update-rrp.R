## This script is intended to run in an Element automated workflow with the objective of automatically and daily, update the RRP & GS1 Sync status database.
## This is necessary because a bug in the Element platform causes an error to prevent queries from being made in Teradata after a query using the db2 production connector has been previously used to run a query.

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
rrp_database_path <- 'data/item-rrp-sync-result.rds'
rrp_query_path <- 'sql/item_rrp_sync.sql'
connector <- 'db2-production-connector'
rrp_backups_n <- 5

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

query <- readLines(rrp_query_path) %>% 
  paste(collapse = '\n')
res <- purrr::safely(mlutils::dataset.load)(name = connector, query = query)

if (is.data.frame(res$result)) {
  res <- res$result %>%
    mutate_if(is.factor, as.character) %>% 
    as_tibble() %>% 
    set_names(tolower(names(.))) %>% 
    rename(
      old_nbr = item_nbr,
      rrp_ind = retail_ready_pack_ind
    )
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
    folder = dirname(rrp_database_path),
    complete_file_name = basename(rrp_database_path),
    preserved_backups = rrp_backups_n
  )
}, error = function(e){
  flog.error(toJSON(list(
    message = 'BACKUP NOT SUCCESSFUL',
    details = list(
      data = head(res, 2),
      path = rrp_database_path
    )
  )))
})
saveRDS(res, rrp_database_path)

flog.info(toJSON(list(
  message = 'DONE UPDATING RRP INFO',
  details = list(
    backup_name = backup_name,
    backup_path = file.path(getwd(), dirname(rrp_database_path))
  )
)))

