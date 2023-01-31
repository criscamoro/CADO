# Programa de monitoreo de la limnología de cuerpos de agua dulce de Occidente

#### Paquetes ####
library(tidyverse)
library(readxl)
library(qdap)

#### Preparación de datos

#### Datos ambientales ####
# Etiquetas de los parámetros de calidad de agua
id.par <- read_excel('datos/crudos/LagunaCajititlan.xlsx', sheet = 'Parametros') # ID de los parámetros

# Etiquetas de las estaciones de muestreo
id.est <- read_excel('datos/crudos/LagunaCajititlan.xlsx', sheet = 'Puntos de Muestreo') %>%
  slice(-15) # fila de valores NA 

# Estaciones oficiales: LC-01 a LC-05, los datos crudos tienen otras estaciones
id.est_of <- id.est %>% 
  slice(c(11:15)) # a partir de septiembre del 30/09/2013 sólo se utilizan estas estaciones

# Base de datos ambientales rectangular (key-value) sin excluir estaciones 
amb.rect <- read_excel('datos/crudos/LagunaCajititlan.xlsx', sheet = 'Laguna de Cajititlán') %>%
  select(-1) %>%
  slice(-n()) %>% 
  mutate(valor = as.character(gsub('<', '', valor))) %>% 
  mutate(valor = as.numeric(gsub('-', '', valor))) %>% # "-" son valores NA
  mutate(idParametro = as.character(idParametro)) %>%
  mutate(idParametro = as.character(mgsub(id.par$idParametros, id.par$param, idParametro)))

# Base de datos ambientales rectangular (key-value)
amb.rect_of <- amb.rect %>% 
  mutate(idPuntoMuestreo = as.character(mgsub(id.est_of$idPunto, id.est_of$clave, idPuntoMuestreo))) %>% 
  filter(idPuntoMuestreo == 'LC-01' | # filtrando para las estaciones LC-01 a LC-05
           idPuntoMuestreo == 'LC-02' |
           idPuntoMuestreo == 'LC-03' |
           idPuntoMuestreo == 'LC-04' |
           idPuntoMuestreo == 'LC-05')

write.csv(amb.rect_of, 'datos/rectangulares/ambiental_rect.csv', row.names = F, na = '')

# Base de datos ambientales formato tidy
amb.tidy <- amb.rect_of %>%
  pivot_wider(names_from = 'idParametro', values_from = 'valor') 

write.csv(amb.tidy, 'datos/ambiental_tidy.csv', row.names = F, na = '')

#### Zooplancton ####
# Base de datos de zooplancton formato tidy
zoo.tidy <- read_excel('datos/crudos/Cajititlán_Bio.xlsx', 
                      sheet = 'ZooP', col_names = F, skip = 1) %>% 
  slice(-c(1,8)) %>%
  column_to_rownames('...1') %>%
  t

write.csv(zoo.tidy, 'datos/zooplancton_tidy.csv', row.names = F)

# Base de datos de zooplancton rectangular (key-value)
zoo.rect <- read_csv('datos/zooplancton_tidy.csv') %>% 
  pivot_longer(cols = c(1:6), names_to = 'taxa', values_to = 'conteo')

write.csv(zoo.rect, 'datos/rectangulares/zooplancton_rect.csv', row.names = F)

#### Abundancia de fitoplancton ####
# Base de datos de fitoplancton formato tidy
fito.tidy <- read_excel('datos/crudos/Cajititlán_Bio.xlsx', sheet = 'FitoP', skip = 1) %>%
  slice(-c(1,62,66)) %>%
  select(-c('...317','Toxicidad','División')) %>%
  column_to_rownames('...1') %>% 
  t

write.csv(fito.tidy, 'datos/fitoplancton_tidy.csv', row.names = F, na = '0')

# Base de datos de fitoplancton rectangular (key-value)
fito.rect <- read_csv('datos/fitoplancton_tidy.csv') %>% 
  pivot_longer(cols = c(1:60), names_to = 'taxa', values_to = 'conteo')

write.csv(fito.rect, 'datos/rectangulares/fitoplancton_rect.csv', row.names = F)
