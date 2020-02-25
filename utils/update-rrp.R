## This script is intended to run in an Element automated workflow with the objective of automatically and daily, update the RRP & GS1 Sync status database.
## This is necessary because a bug in the Element platform causes an error to prevent queries from being made in Teradata after a query using the db2 production connector has been previously used to run a query.

query <- readLines('sql/item_rrp_sync.sql') %>% 
  paste(collapse = '\n')
res <- sql_query(
  ch = NULL,
  connector =  'db2-production-connector',
  query = query,
  stringsAsFactors = FALSE
) %>% 
  as_tibble() %>% 
  set_names(tolower(names(.))) %>% 
  rename(
    old_nbr = item_nbr,
    rrp_ind = retail_ready_pack_ind
  )
backup_file(
  res,
  folder = dirname(gl$rrp_sync_database),
  complete_file_name = basename(gl$rrp_sync_database),
  preserved_backups = gl$rrp_sync_backups_n
)
saveRDS(res, gl$rrp_sync_database)

