
library(tidyverse)
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
  app_name = 'Promo Fulfillment',
  app_version = 'v0.1.0',
  app_version_date = '2019-01-28'
)
gl$app_version_text <- sprintf('%s, (%s)', gl$app_version, gl$app_version_date)


# Llamar módulos ----------------------------------------------------------


