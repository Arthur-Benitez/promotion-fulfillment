library(tidyverse)

# Variables ---------------------------------------------------------------

path_zonas <- 'data/deptos-zona.csv'
path_depts <- 'data/deptos-vp.csv'
path_save <- 'data/stores-shelves.csv'

stores_paths <- list(
  sc = 'data/bases-cabeceras-sc.csv',
  sp = 'data/bases-cabeceras-sp.csv',
  ba = 'data/bases-cabeceras-ba.csv',
  mb = 'data/bases-cabeceras-mb.csv',
  bae = 'data/bases-cabeceras-bae.csv'
)

# Files reading -----------------------------------------------------------

zonas <- read_csv(path_zonas) %>% 
  filter(conocido == TRUE)

deptos_vp <- read_csv(path_depts)

stores_tables <- lapply(stores_paths, function(x){
  x %>% 
    read_csv %>% 
    na.omit %>% 
    set_names(tolower(names(.))) %>% 
    gather('zona', 'shelves_qty', -(store_nbr:total)) %>% 
    filter(shelves_qty > 0)
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

stores_shelves <- stores_tables %>% 
  do.call(rbind, .) %>% 
  left_join(dictionary, by = 'zona') %>% 
  mutate(
    shelf = toupper(paste(nombre_mueble, tipo, sep = ' '))
  ) %>% 
  write_csv(path_save)


