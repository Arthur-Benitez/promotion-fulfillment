
library(magrittr)
library(plotly)
library(tidyverse)
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


options(readr.default_locale=readr::locale(tz = ''))



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
  app_version = '0.1.8',
  app_version_date = '2019-03-06',
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
gl$app_version_text <- sprintf('%s (%s)', gl$app_version, gl$app_version_date)


# Llamar módulos ----------------------------------------------------------

eval(parse('functions.R', encoding = 'UTF-8'))
eval(parse('lang.R', encoding = 'UTF-8'))
