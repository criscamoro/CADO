# Programa de monitoreo de la limnología de cuerpos de agua dulce de Occidente

#### Paquetes ####
library(tidyverse)
library(readxl)

#### Preparación de datos 
#### Tidy #### 

# Datos ambientales
env.raw <- read_excel('datos/crudos/Cajititlán_Env.xlsx', sheet = 'EnvP', skip = 1) %>% 
  select(-c('...1', '...39'))

write.csv(env.raw,'datos/ambiental.csv', row.names = F, na = '')

# Abundancia de zooplancton

zoo.raw <- read_excel('datos/crudos/Cajititlán_Bio.xlsx', 
                      sheet = 'ZooP', col_names = F, skip = 1) %>% 
  slice(-c(1,8)) %>%
  column_to_rownames('...1') %>%
  t

write.csv(zoo.raw, 'datos/zooplancton.csv', row.names = F)

# Abundancia de fitoplancton

fito.raw <- read_excel('datos/crudos/Cajititlán_Bio.xlsx', sheet = 'FitoP', skip = 1) %>%
  slice(-c(1,62,66)) %>%
  select(-c('...317','Toxicidad','División')) %>%
  column_to_rownames('...1') %>% 
  t

write.csv(fito.raw, 'datos/fitoplancton.csv', row.names = F, na = '0')

#### Rectangular ####
# Ambientales
env.rect <- read_csv('datos/ambiental.csv') %>% 
  pivot_longer(cols = c(1:37), names_to = 'var', values_to = 'value')

write.csv(env.rect, 'datos/rectangulares/ambiental_rect.csv')

# Zooplancton
zoo.rect <- read_csv('datos/zooplancton.csv') %>% 
  pivot_longer(cols = c(1:6), names_to = 'taxa', values_to = 'count')

write.csv(zoo.rect, 'datos/rectangulares/zooplancton_rect.csv')

# Fitoplancton
fito.rect <- read_csv('datos/fitoplancton.csv') %>% 
  pivot_longer(cols = c(1:60), names_to = 'taxa', values_to = 'count')

write.csv(fito.rect, 'datos/rectangulares/fitoplancton_rect.csv')
