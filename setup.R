
pkgs <- c(
  'magrittr',
  'tidyverse',
  'readxl',
  'lubridate',
  'shiny',
  'shinyjs',
  'shinycssloaders',
  # 'htmlwidgets',
  'shinyWidgets',
  'DT',
  'shinydashboard',
  'futile.logger',
  'zip',
  'future',
  'promises',
  'openxlsx'
)

sapply(pkgs, function(pk){
  cat('Installing', pk, '\n')
  install.packages(pk)
})
