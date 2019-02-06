
library(magrittr)
library(tidyverse)
library(readxl)
library(lubridate)
library(shiny)
library(shinyjs)
library(shinycssloaders)
# library(htmlwidgets)
library(DT)
library(shinydashboard)
library(futile.logger)


options(readr.default_locale=readr::locale(tz = ''))



# Parámetros globales -----------------------------------------------------


gl <- list(
  app_version = 'v0.1.0',
  app_version_date = '2019-01-28',
  cols = c(
    'feature_nbr' = 'numeric',
    'feature_name' = 'character',
    'dept_nbr' = 'numeric',
    'negocio' = 'character',
    'old_nbr' = 'numeric',
    'max_feature_qty' = 'numeric',
    'max_ddv' = 'numeric',
    'semana_ini' = 'numeric',
    'semana_fin' = 'numeric',
    'fcst_or_sales' = 'character'
  ),
  feature_const_cols = c(
    'feature_name',
    'dept_nbr',
    'negocio',
    'max_feature_qty',
    'semana_ini',
    'semana_fin',
    'fcst_or_sales'
  ),
  negocios = c(
    'SUPERCENTER',
    'BODEGA',
    'SUPERAMA',
    'MIBODEGA',
    'BAE',
    'MEDIMART',
    'OTRO'
  )
)
gl$app_version_text <- sprintf('%s, (%s)', gl$app_version, gl$app_version_date)


# Llamar módulos ----------------------------------------------------------

eval(parse('functions.R', encoding = 'UTF-8'))
eval(parse('lang.R', encoding = 'UTF-8'))
