
library(fg)
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
eval(parse('modules/compute-promotions.R', encoding = 'UTF-8'))
eval(parse('modules/login.R', encoding = 'UTF-8'))
eval(parse('modules/usage-stats.R', encoding = 'UTF-8'))

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
  app_version = '0.2.0-beta-2',
  app_version_date = '2019-04-16',
  ## Compute promotions
  cols = c(
    'feature_name' = 'character',
    'user' = 'character',
    'dept_nbr' = 'numeric',
    'negocio' = 'character',
    'old_nbr' = 'numeric',
    'max_feature_qty' = 'numeric',
    'max_ddv' = 'numeric',
    'semana_ini' = 'numeric',
    'semana_fin' = 'numeric',
    'fcst_or_sales' = 'character',
    'StartDate' = 'date',
    'EndDate' = 'date'
  ),
  feature_const_cols = c(
    'user',
    'dept_nbr',
    'negocio',
    'max_feature_qty',
    'semana_ini',
    'semana_fin',
    'fcst_or_sales',
    'StartDate',
    'EndDate'
  ),
  negocios = c(
    'SUPERCENTER',
    'BODEGA',
    'SUPERAMA',
    'MIBODEGA',
    'BAE',
    'MEDIMART',
    'OTRO'
  ),
  max_input_rows = 100,
  max_input_queries = 10
)
gl$app_version_text <- sprintf('Versión %s (%s)', gl$app_version, gl$app_version_date)

## Login & Auth
### Path a base de datos de usuarios
gl$user_data_path <- paste0(gl$app_deployment_environment, '/etc/psswd')
if (!dir.exists(dirname(gl$user_data_path))) {
  dir.create(dirname(gl$user_data_path), recursive = TRUE)
}
### Nivel de permisos por tipo de usuario
gl$clearance_levels <- c(
  'owner' = 0,
  'admin' = 1,
  'basic' = 2
)

# Llamar módulos ----------------------------------------------------------

eval(parse('functions.R', encoding = 'UTF-8'))
eval(parse('lang.R', encoding = 'UTF-8'))
