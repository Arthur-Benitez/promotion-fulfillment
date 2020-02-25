## This script is intended to run in an Element automated workflow with the objective of automatically and daily, update the RRP & GS1 Sync status database.
## This is necessary because a bug in the Element platform causes an error to prevent queries from being made in Teradata after a query using the db2 production connector has been previously used to run a query.

library(tidyverse)

working_dir <- '/home/rstudio/dev/DEV-promo-fulfillment-app'
functions_path <- 'utils/functions.R'
rrp_database_path <- file.path(working_dir, 'data/item-rrp-sync-result.rds')
rrp_query_path <- 'sql/item_rrp_sync.sql'
connector <- 'db2-production-connector'
rrp_backups_n <- 5
source(file.path(working_dir, functions_path))

query <- readLines(file.path(working_dir, rrp_query_path)) %>% 
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

