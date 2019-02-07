
pkgs <- c(
  'magrittr',
  'tidyverse',
  'readxl',
  'lubridate',
  'shiny',
  'shinyjs',
  'shinycssloaders',
  # 'htmlwidgets',
  'DT',
  'shinydashboard',
  'futile.logger',
  'zip'
)

sapply(pkgs, function(pk){
  cat('Installing', pk, '\n')
  install.packages(pk)
})
