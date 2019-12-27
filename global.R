
library(magrittr)
library(plotly)
library(tidyverse)
library(ggthemes)
library(readxl)
library(lubridate)
library(jsonlite) # debe ir antes de shiny para no enmascarar validate()
library(shiny)
library(shinyjs)
library(shinycssloaders)
# library(htmlwidgets)
library(DT)
library(shinydashboard)
library(futile.logger)
library(shinyalert)
library(RODBC)
library(future)
library(promises)
library(openxlsx)

## Procesamiento paralelo de future
plan(multiprocess)

## Para que readr no truene
options(readr.default_locale=readr::locale(tz = ''))

## Loggear session info al levantar el deployment
flog.info(toJSON(list(
  message = "R SESSION INFO",
  details = list(
    session_info = paste(capture.output(sessionInfo()), collapse = '\n')
  )
)))

## Asegurar que los locales están bien seteados
if (Sys.info()[['sysname']] != 'Windows') {
  flog.info(toJSON(list(
    message = "INITIAL R LOCALE",
    details = list(
      locale = Sys.getlocale()
    )
  )))
  tribble(
    ~category, ~locale,
    "LC_CTYPE", "en_US.UTF-8",
    "LC_NUMERIC", "C",
    "LC_TIME", "en_US.UTF-8",
    "LC_COLLATE", "en_US.UTF-8",
    "LC_MONETARY", "en_US.UTF-8",
    "LC_MESSAGES", "en_US.UTF-8"
  ) %>% 
    pwalk(function(category, locale){
      tryCatch({
        old_locale <- Sys.getlocale(category)
        if (locale != old_locale) {
          flog.info(toJSON(list(
            message = "CHANGING LOCALE",
            details = list(
              category = category,
              locale = old_locale
            )
          )))
          Sys.setlocale(category, locale)
          flog.info(toJSON(list(
            message = "CHANGED LOCALE",
            details = list(
              category = category,
              locale = list(
                old = old_locale,
                new = locale
              )
            )
          )))
        }
      }, error = function(e){
        flog.info(toJSON(list(
          message = "FAILED CHANGING LOCALE",
          details = list(
            category = category,
            locale = old_locale
          )
        )))
      })
    })
}
flog.info(toJSON(list(
  message = "R LOCALE",
  details = list(
    locale = Sys.getlocale()
  )
)))

# Módulos -----------------------------------------------------------------

## Usar esto en lugar de source(., encoding = 'UTF-8') porque truena a menos que cambiemos el locale del sistema con Sys.setlocale('LC_CTYPE', 'en_US.UTF-8') 
## Ver: https://stackoverflow.com/questions/5031630/how-to-source-r-file-saved-using-utf-8-encoding
eval(parse('functions.R', encoding = 'UTF-8'))
eval(parse('lang.R', encoding = 'UTF-8'))
eval(parse('modules/compute-promotions.R', encoding = 'UTF-8'))
eval(parse('modules/login.R', encoding = 'UTF-8'))
eval(parse('modules/usage-stats.R', encoding = 'UTF-8'))
eval(parse('modules/notifications.R', encoding = 'UTF-8'))

# Parámetros globales -----------------------------------------------------


gl <- list(
  app_deployment_environment = {
    if (dir.exists('prod')) 'prod'
    else if (dir.exists('dev')) 'dev'
    else {
      message('No directory named dev/ or prod/ found. Creating dev/')
      dir.create('dev')
      'dev'
    }
  },
  app_version = '1.6.0',
  app_version_date = '2019-11-28',
  ## Compute promotions
  cols = tryCatch({
    read_tsv('data/column-info.txt', col_types = 'ccllccncc')
  }, error = function(e){
    tribble(~name, ~type, ~is_constant_by_feature, ~pretty_name, ~description)
  }),
  negocios = c(
    'SUPERCENTER',
    'BODEGA',
    'SUPERAMA',
    'MIBODEGA',
    'BAE',
    'MEDIMART',
    'OTRO'
  ),
  shelves = c(
    'BASE',
    'MEDIA BASE',
    'CABECERA ALTA',
    'CABECERA BAJA',
    'CHIMENEA'
  ),
  max_input_rows = 500,
  max_output_rows = 100000,
  max_input_queries = 15,
  timeout_warning_duration = 300,
  plotly_height = '40vh',
  table_height = list(
    short = '30vh',
    medium = '40vh',
    tall = '50vh'
  )
)
gl$is_dev <- gl$app_deployment_environment == 'dev'
gl$app_version_text <- sprintf('Versión %s (%s)', gl$app_version, gl$app_version_date)
gl$feature_const_cols <- gl$cols$name[gl$cols$is_input & gl$cols$is_constant_by_feature]

## Login & Auth
### Path a base de datos de usuarios
gl$user_data_path <- paste0(gl$app_deployment_environment, '/etc/psswd')
if (!dir.exists(dirname(gl$user_data_path))) {
  dir.create(dirname(gl$user_data_path), recursive = TRUE)
}

### Path de base de datos con los datos de los muebles de las tiendas
gl$shelves_database <- 'data/stores-shelves.csv'
### Path de base de datos con los estatuts de RRP y sincronización en GS1
gl$rrp_sync_database <- 'data/item-rrp-sync-result.rds'
### Periodo de actualización de base de datos de RRP y Sync (días)
gl$rrp_sync_update_period <- 7
### Número de archivos de respaldo de la base de datos
gl$rrp_sync_backups_n <- 5
### Nivel de permisos por tipo de usuario
gl$clearance_levels <- c(
  'owner' = 0,
  'admin' = 1,
  'basic' = 2
)

gl$clearance_pal <- c(
  all = rgb(0, 56, 150, maxColorValue = 255),
  owner = rgb(26, 117, 207, maxColorValue = 255),
  admin = rgb(253, 187, 48, maxColorValue = 255),
  basic = rgb(51, 115, 33, maxColorValue = 255)
)


