
#' Este código sirve para generar el zip para liberar a producción
#'
#' Después de generar el zip, basta subirlo a la carpeta en Element para que se
#' suba el código, y luego hay que apagar y prender el deployment de la app para
#' que se jalen los últimos cambios.
#'
#' WARNING: Es MUY IMPORTANTE que localmente la carpeta "prod" NO exista para no
#' sobreescribir los archivos de producción (ej. logs, contraseñas, etc)

eval(parse('global.R', encoding = 'UTF-8'))

## IMPORTANTE: Hice el cambio de carpeta porque si intentamos darle "." en files
## a zip::zip, expande la ruta y genera que (1) Windows no pueda deszippearla y
## (2) el nombre de la carpeta en el servidor sea la ruta expandida de Windows
## (con "C:\" y todo...)

## Se debe iniciar en la carpeta building-blocks-app
setwd('..')
zip::zip(
  zipfile = sprintf('releases/promo-fulfillment-app_v%s_%s.zip', gl$app_version, Sys.Date()),
  files = 'promo-fulfillment-app',
  recurse = TRUE,
  compression_level = 9
)
setwd('promo-fulfillment-app/')
