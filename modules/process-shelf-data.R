library(tidyverse)

# Variables ---------------------------------------------------------------

path_zonas <- 'data/deptos_zona.csv'
path_depts <- 'data/deptos_vp.csv'
path_save <- 'data/stores_shelfs.csv'

stores_paths <- list(
  sc = 'data/bases_cabeceras_sc.csv',
  sp = 'data/bases_cabeceras_sp.csv',
  ba = 'data/bases_cabeceras_ba.csv',
  mb = 'data/bases_cabeceras_mb.csv',
  bae = 'data/bases_cabeceras_bae.csv'
)

previous_levels <- c("ABARROTES Y VINOS", "APERTURAS", "CONSUMIBLES", "FARMACIA", "FRESH", "MG", "PERECEDEROS", "ROPA", "SALUD Y NUEVOS NEGOCIOS", 'PRICHOS')

new_levels <- c('ABARROTES', 'APERTURAS', 'CONSUMIBLES', 'FARMACIA', 'ABARROTES FRESCOS', 'MERCANCIAS GENERALES', 'PERECEDEROS', 'ROPA', 'MERCANCIAS GENERALES', 'PRICHOS')


# Files reading -----------------------------------------------------------

zonas <- read_csv(path_zonas) %>% 
  filter(conocido == TRUE)

deptos_vp <- read_csv(path_depts) %>% 
  mutate(vp = as.character(factor(vp, levels = previous_levels, labels = new_levels)))

stores_tables <- lapply(stores_paths, function(x){
  x %>% 
    read_csv %>% 
    na.omit %>% 
    set_names(tolower(names(.))) %>% 
    gather('zona', 'cantidad', -(store_nbr:total)) %>% 
    filter(cantidad > 0)
})


# Dictionary processing ---------------------------------------------------

no_depto_df <- zonas %>% 
  filter(is.na(depto1)) %>% 
  mutate(vps = ifelse(!is.na(vp1) & !is.na(vp2), paste(vp1, vp2, sep = '-'), coalesce(vp1, vp2))) %>% 
  gather('key', 'vp', vp1, vp2, na.rm = TRUE) %>% 
  left_join(deptos_vp, by = 'vp') %>% 
  select(zona, vps, dept_nbr)
  
depto_df <- zonas %>% 
  gather('key', 'dept_nbr', depto1:depto4, na.rm = TRUE) %>% 
  mutate(vps = ifelse(!is.na(vp1) & !is.na(vp2), paste(vp1, vp2, sep = '-'), coalesce(vp1, vp2))) %>%
  select(zona, vps, dept_nbr)

dictionary <- depto_df %>% 
  bind_rows(no_depto_df) %>% 
  left_join(deptos_vp, by = 'dept_nbr') %>%
  select(zona, vp, dept_nbr, vps) %>% 
  mutate(
    zona = tolower(zona),
    vp = as.character(vp),
    vps = coalesce(vps, vp)
  ) %>% 
  arrange(zona)


# Stores tables processing ------------------------------------------------

stores_shelfs <- stores_tables %>% 
  do.call(rbind, .) %>% 
  left_join(dictionary, by = 'zona') %>% 
  mutate(
    shelf = toupper(paste(nombre_mueble, tipo, sep = ' '))
  ) %>% 
  write_csv(path_save)


