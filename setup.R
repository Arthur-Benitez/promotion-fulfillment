
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
  'zip',
  'future',
  'promises'
)

sapply(pkgs, function(pk){
  cat('Installing', pk, '\n')
  install.packages(pk)
})
