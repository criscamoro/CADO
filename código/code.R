# Programa de monitoreo de la limnología de cuerpos de agua dulce de Occidente

#### Paquetes ####
library(tidyverse)
library(readxl)
library(qdap)
library(lubridate)
library(ggside)
library(ggforce) #facet_wrap_paginate
#### Preparación de datos
#### Datos ambientales ####
# Descargar datos abiertos del portal de la CEA Jalisco
download.file('https://www.ceajalisco.gob.mx/contenido/datos_abiertos/LagunaCajititlan.xlsx', 
              'datos/crudos/LagunaCajititlan.xlsx', quiet = T, mode = 'wb') # "mode = 'wb'" para Windows

# Etiquetas de los parámetros de calidad de agua
id.par <- read_excel('datos/crudos/LagunaCajititlan.xlsx', sheet = 'Parametros') # ID de los parámetros

# Etiquetas de las estaciones de muestreo
id.est <- read_excel('datos/crudos/LagunaCajititlan.xlsx', sheet = 'Puntos de Muestreo') %>%
  slice(-15) # fila de valores NA 

# Estaciones fijas: LC-01 a LC-05; no se registran datos en las demás estaciones desde el 30/09/2013
id.est.fij <- id.est %>% 
  slice(c(11:15)) 

# Base de datos ambientales rectangular (key-value)  
amb.rect <- read_excel('datos/crudos/LagunaCajititlan.xlsx', sheet = 'Laguna de Cajititlán') %>%
  select(-1) %>%
  slice(-n()) %>% 
  mutate(valor = as.character(gsub('<', '', valor))) %>% 
  mutate(valor = as.numeric(gsub('-', '', valor))) %>% # "-" son valores NA
  mutate(idParametro = as.character(idParametro)) %>%
  mutate(idParametro = as.character(mgsub(id.par$idParametros, id.par$param, idParametro)))

# Sustitución de etiquetas y columna binaria de estación fija (LC-01 a LC-05 = TRUE)
amb.rect.fij <- amb.rect %>% 
  mutate(idPuntoMuestreo = as.character(mgsub(id.est.fij$idPunto, id.est.fij$clave, idPuntoMuestreo))) %>%
  mutate(est_fijas = as.logical(idPuntoMuestreo == 'LC-01' |
                                  idPuntoMuestreo == 'LC-02' |
                                  idPuntoMuestreo == 'LC-03' |
                                  idPuntoMuestreo == 'LC-04' |
                                  idPuntoMuestreo == 'LC-05'))
  
write.csv(amb.rect.fij, 'datos/rectangulares/ambiental_rect.csv', row.names = F, na = '')

# Base de datos ambientales formato tidy
amb.tidy <- amb.rect.fij %>%
  pivot_wider(names_from = 'idParametro', values_from = 'valor') %>%
  relocate(fecha, .after = 'Materia flotante') %>% 
  mutate(año = as.numeric(year(fecha), origin = fecha)) %>% 
  mutate(mes = as.numeric(month(fecha), origin = fecha)) %>% 
  rename(est = idPuntoMuestreo) %>% 
  relocate(est, .after = mes) %>% 
  relocate(est_fijas, .after = fecha)

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

#### Fitoplancton ####
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

#### Análisis exploratorio ####

# Gráfico de series temporales, localidades completas, agrupados por parámetro

ggplot(data = amb.rect.fij) +
  geom_line(
    mapping = aes(x = fecha, y = valor, color = idPuntoMuestreo)
  ) +
  facet_wrap(~ idParametro, scales = 'free')

# Gráfico anterior, considerando sólo estaciones fijas

ggplot(data = amb.rect.fij %>% 
         filter(est_fijas == T)) +
  geom_line(
    mapping = aes(x = fecha, y = valor, color = idPuntoMuestreo)
  ) +
  facet_wrap(~ idParametro, scales = 'free')

# Mismo gráfico, facets paginados

for (i in 1:5) {
  print(
    ggplot(data = amb.rect.fij %>% 
             filter(est_fijas == T)) +
      geom_line(
        mapping = aes(x = fecha, y = valor, color = idPuntoMuestreo)
      ) +
      facet_wrap_paginate(~ idParametro, scales = 'free', nrow = 3, ncol = 3, page = i)
  )
}

