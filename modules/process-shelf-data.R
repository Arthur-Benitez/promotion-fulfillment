library(tidyverse)

# Variables ---------------------------------------------------------------

path_muebles <- 'data/deptos_mueble.csv'
path_depts <- 'data/deptos_vp.csv'
# path_sc <- 'C:/Users/a0b01eb/Desktop/Trabajos/DataScience/PromoFulfillment/Staff/2.0/20190624_bases_cabeceras_sc.csv'

previous_levels <- c("ABARROTES Y VINOS", "APERTURAS", "CONSUMIBLES", "FARMACIA", "FRESH", "MG", "PERECEDEROS", "ROPA", "SALUD Y NUEVOS NEGOCIOS", 'PRICHOS')

new_levels <- c('ABARROTES', 'APERTURAS', 'CONSUMIBLES', 'FARMACIA', 'ABARROTES FRESCOS', 'MERCANCIAS GENERALES', 'PERECEDEROS', 'ROPA', 'MERCANCIAS GENERALES', 'PRICHOS')


# Files reading -----------------------------------------------------------

muebles <- read_csv(path_muebles) %>% 
  filter(conocido == TRUE)

deptos_vp <- read_csv(path_depts) %>% 
  mutate(vp = factor(vp, levels = previous_levels, labels = new_levels))

table_sc <- read_csv(path_sc)


# Dictionary processing ---------------------------------------------------

no_depto_df <- muebles %>% 
  filter(is.na(depto1)) %>% 
  mutate(vps = ifelse(!is.na(vp1) & !is.na(vp2), paste(vp1, vp2, sep = '-'), coalesce(vp1, vp2))) %>% 
  gather('key', 'vp', vp1, vp2, na.rm = TRUE) %>% 
  left_join(deptos_vp, by = 'vp') %>% 
  select(mueble, vps, dept_nbr)
  
depto_df <- muebles %>% 
  gather('key', 'dept_nbr', depto1:depto4, na.rm = TRUE) %>% 
  mutate(vps = ifelse(!is.na(vp1) & !is.na(vp2), paste(vp1, vp2, sep = '-'), coalesce(vp1, vp2))) %>%
  selecpt(mueble, vps, dept_nbr)

dictionary <- depto_df %>% 
  bind_rows(no_depto_df) %>% 
  left_join(deptos_vp, by = 'dept_nbr') %>%
  select(mueble, vp, dept_nbr, vps) %>% 
  mutate(
    vp = as.character(vp),
    vps = coalesce(vps, vp)
  ) %>% 
  arrange(mueble)
