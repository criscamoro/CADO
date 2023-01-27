# Programa de monitoreo de la limnología de cuerpos de agua dulce de Occidente

#### Paquetes ####
library(tidyverse)
library(readxl)


#### Preparación de datos ####
# Acomodo de los datos de forma rectangular (tidy)

# Datos ambientales
env.raw <- read_excel('datos/crudos/Cajititlán_Env.xlsx', sheet = 'EnvP', skip = 1) %>% 
  select(-'...1')

write.csv(env.raw,'datos/ambiental.csv', row.names = F, na = '')

# Abundancia de zooplancton

zoo.raw <- read_excel('datos/crudos/Cajititlán_Bio.xlsx', sheet = 'ZooP', col_names = F, skip = 1) %>% 
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






