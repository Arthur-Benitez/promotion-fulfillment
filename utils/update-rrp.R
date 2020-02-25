## This script is intended to run in an Element automated workflow with the objective of automatically and daily, update the RRP & GS1 Sync status database.
## This is necessary because a bug in the Element platform causes an error to prevent queries from being made in Teradata after a query using the db2 production connector has been previously used to run a query.

library(tidyverse)

app_deployment_environment <- getArgument('app_deployment_environment', 'PARAMS')

if (app_deployment_environment == 'prod') {
  setwd('/home/rstudio/prod/promo-fulfillment-app-produccion')
} else {
  setwd('/home/rstudio/dev/DEV-promo-fulfillment-app')
}

functions_path <- 'utils/functions.R'
rrp_database_path <- 'data/item-rrp-sync-result.rds'
rrp_query_path <- 'sql/item_rrp_sync.sql'
connector <- 'db2-production-connector'
rrp_backups_n <- 5
source(functions_path)

query <- readLines(rrp_query_path) %>% 
  paste(collapse = '\n')
res <- mlutils::dataset.load(
  name = connector,
  query = query
  ) %>% 
  as_tibble() %>% 
  set_names(tolower(names(.))) %>% 
  rename(
    old_nbr = item_nbr,
    rrp_ind = retail_ready_pack_ind
  )
backup_file(
  res,
  folder = dirname(rrp_database_path),
  complete_file_name = basename(rrp_database_path),
  preserved_backups = rrp_backups_n
)
saveRDS(res, rrp_database_path)

