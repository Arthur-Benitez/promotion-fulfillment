#' Descargar semanas Walmart con sus fechas de inicio
#' 
#' Autor: Felipe Gerard <felipe.gerard@walmart.com>
#' 

library(tidyverse)


## Conectarse a la base
con <- DBI::dbConnect(
  odbc::odbc(),
  Driver = 'Teradata',
  DBCName = 'WMG',
  AUTHENTICATION = 'ldap',
  UID = 'f0g00bq',
  PWD = rstudioapi::askForPassword('Password')
)

## Calendar day
calendar_day <- tbl(con, sql('select wm_yr_wk as "wm_yr_wk", min(gregorian_date) as "date" from mx_cf_vm.calendar_day group by 1')) %>% collect()

if (FALSE) {
  write_tsv(calendar_day, 'data/calendar-day.tsv')
}
