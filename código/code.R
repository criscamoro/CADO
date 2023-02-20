# Programa de monitoreo de la limnología de cuerpos de agua dulce de Occidente

#### Paquetes ####
library(tidyverse)
library(readxl)
library(qdap)
library(lubridate)
library(ggside)
library(ggforce)
library(corrplot)
library(vtable)
library(vegan)
library(clustsig)
#### Funciones ####
# Coeficiente de variación 
cv <- function(x) {
  c.v <- (sd(x, na.rm = T)/mean(x, na.rm = T)*100)
}

###### Preparación de datos ####
#### Datos ambientales ####
# Descargar datos abiertos del portal de la CEA Jalisco
download.file('https://www.ceajalisco.gob.mx/contenido/datos_abiertos/LagunaCajititlan.xlsx', 
              'datos/crudos/LagunaCajititlan.xlsx', quiet = T, mode = 'wb') # "mode = 'wb'" para Windows

# Etiquetas de los parámetros de calidad de agua
id.par <- read_excel('datos/crudos/LagunaCajititlan.xlsx', sheet = 'Parametros') # ID de los parámetros

# Unidades de los parámetros de calidad de agua
id.uni <- read_csv('datos/crudos/unidades.csv')

# Etiquetas de las estaciones de muestreo
id.est <- read_excel('datos/crudos/LagunaCajititlan.xlsx', sheet = 'Puntos de Muestreo') %>%
  slice(-15) # fila de valores NA 

# Estaciones fijas: LC-01 a LC-05; no se registran datos en las demás estaciones desde el 30/09/2013
id.est.fij <- id.est %>% 
  slice(c(11:15)) 

# Base de datos ambientales rectangular (key-value)  
amb.rect <- read_excel('datos/crudos/LagunaCajititlan.xlsx', sheet = 'Laguna de Cajititlán') %>%
  select(-1) %>% # quitar columna de idMuestra
  slice(-n()) %>% 
  slice(-(22155:22404)) %>% # contiene fechas erróneas, datos duplicados y estaciones sin observaciones
  mutate(fecha = as.Date(as.character(gsub('2017-04-24', '2017-04-27', fecha)))) %>% # fecha incorrecta
  mutate(valor = as.character(gsub('<', '', valor))) %>% 
  mutate(valor = as.numeric(gsub('-', '', valor))) %>% # "-" son valores NA
  mutate(idParametro = as.character(idParametro)) %>%
  mutate(unidad = as.character(mgsub(id.par$idParametros, id.uni$unidad, idParametro))) %>% # unidades
  mutate(idParametro = as.character(mgsub(id.par$idParametros, id.par$param, idParametro))) %>% 
  relocate(unidad, .after = idParametro)

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
amb.tidy <- amb.rect.fij %>% select(-unidad) %>% # filtrar unidades para evitar pivotarlas
  pivot_wider(names_from = 'idParametro', values_from = 'valor') %>%
  relocate(fecha, .after = 'Materia flotante') %>% 
  mutate(año = as.factor(year(fecha))) %>% 
  mutate(mes = as.factor(month(fecha))) %>% 
  rename(est = idPuntoMuestreo) %>% 
  mutate(est = as.factor(est)) %>% 
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


###### Análisis exploratorio ####
#### Cuadros de resumen #####
# por año en formato csv
for (i in 2009:2022) {
  amb.tidy %>%
    filter(año == i) %>% 
    select(1:45) %>% 
  st(
    title = paste('Variables ambientales del', as.character(i)),
    summ = list(c('notNA(x)', 'mean(x)', 'sd(x)', 'min(x)', 'max(x)', 'cv(x)')),
    summ.names = list(c('N', 'Media', 'D.E.', 'Min', 'Max', 'C.V.')),
    out = 'csv',
    file = paste('figuras/cuadros/csv/yr/cuadro_', as.character(i), '.csv', sep = '')
    )
}

# por año en formato html
for (i in 2009:2022) {
  amb.tidy %>%
    filter(año == i) %>% 
    select(1:45) %>% 
    st(
      title = paste('Variables ambientales del', as.character(i)),
      summ = list(c('notNA(x)', 'mean(x)', 'sd(x)', 'min(x)', 'max(x)', 'cv(x)')),
      summ.names = list(c('N', 'Media', 'D.E.', 'Min', 'Max', 'C.V.')),
      out = 'htmlreturn',
      file = paste('figuras/cuadros/html/yr/cuadro_', as.character(i), sep = '')
    )
}

# por parámetro, por año, en formato csv
for (i in 1:45) {
  amb.tidy %>%
    select(i, año) %>%
    group_by(año) %>% 
    mutate(rn = row_number()) %>% 
    pivot_wider(names_from = 2, values_from = 1) %>% 
    select(-1) %>% 
    st(
      title = as.character(amb.tidy[i]),
      summ = list(c('notNA(x)', 'mean(x)', 'sd(x)', 'min(x)', 'max(x)', 'cv(x)')),
      summ.names = list(c('N', 'Media', 'D.E.', 'Min', 'Max', 'C.V.')),
      out = 'csv',
      file = paste('figuras/cuadros/csv/par_yr/cuadro_', colnames(amb.tidy[i]), '.csv', sep = '')
    )
}

# por parámetro, por año, en formato html
for (i in 1:45) {
  amb.tidy %>%
    select(i, año) %>%
    group_by(año) %>% 
    mutate(rn = row_number()) %>% 
    pivot_wider(names_from = 2, values_from = 1) %>% 
    select(-1) %>% 
    st(
      title = colnames(amb.tidy[i]),
      summ = list(c('notNA(x)', 'mean(x)', 'sd(x)', 'min(x)', 'max(x)', 'cv(x)')),
      summ.names = list(c('N', 'Media', 'D.E.', 'Min', 'Max', 'C.V.')),
      out = 'htmlreturn',
      file = paste('figuras/cuadros/html/par_yr/cuadro_', colnames(amb.tidy[i]), sep = '')
    )
}
#### Gráficos de series temporales ####

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

#### Matriz de correlaciones ####
# para identificar posibles variables redundantes

amb.tidy.cor <- cor(amb.tidy[,1:45], use = 'pairwise.complete.obs') # matriz de correlación

corrplot(amb.tidy.cor, type = 'upper', 
         col = brewer.pal(n = 8, name = 'RdYlBu'))

# Aisalar esas variables
amb.tidy.cor.cured <- amb.tidy %>% 
  select(Conductividad, `Alcalinidad total`, `Cloruros totales`, `Nitrógeno total`, 
         `Nitrógeno total Kjeldahl`, SST, Sodio, `Sólidos disueltos tot.`, `Sólidos totales`, 
         `Fósforo total`) %>% 
  cor(use = 'pairwise.complete.obs')

corrplot(amb.tidy.cor.cured, type = 'upper', 
         col = brewer.pal(n = 8, name = 'RdYlBu'))

#### Series temporales del coeficiente de variación ####

# Coeficiente de variación por año
amb.tidy.cv.a <- amb.tidy %>% 
  filter(est_fijas == T) %>% 
  group_by(año) %>% 
  summarise(across(
    Temperatura:`Materia flotante`, 
    cv))

for (i in colnames(amb.tidy.cv.a[2:46])) {
  ggsave(paste('figuras/cov/año/', i, '_cov.png', sep = ''), 
         plot = 
           ggplot(data = amb.tidy.cv.a) + 
           geom_line(mapping = aes(x = año, y = .data[[i]], group = 1)) +
           labs(title = paste(i, '(coeficiente de variación)'),
                y = 'COV') +
           theme(plot.title = element_text(hjust = 0.5)),
         width = 1920, height = 1080, units = 'px', pointsize = 12, 
         bg = 'white',dpi = 300)
}

# Coeficiente de variación por año y mes (fecha)
amb.tidy.cv.am <- amb.tidy %>% 
  filter(est_fijas == T) %>% 
  group_by(fecha) %>% 
  summarise(across(
    Temperatura:`Materia flotante`,
    cv))

for (i in colnames(amb.tidy.cv.am[2:46])) {
  ggsave(paste('figuras/cov/año+mes/', i, '_cov_am.png', sep = ''), 
       plot = 
         ggplot(data = amb.tidy.cv.am) + 
         geom_line(mapping = aes(x = fecha, y = .data[[i]], group = 1)) +
         labs(title = paste(i, '(coeficiente de variación)'),
              x = 'fecha', y = 'COV') +
         theme(plot.title = element_text(hjust = 0.5)),
       width = 1920, height = 1080, units = 'px', pointsize = 12, 
       bg = 'white',dpi = 300)
}

# Coeficiente de variación por estación y año
amb.tidy.cv.ea <- amb.tidy %>% 
  filter(est_fijas == T) %>% 
  group_by(año, est) %>% 
  summarise(across(
    Temperatura:`Materia flotante`,
    cv))
# 2020 sólo tiene una observación, por lo que no se puede evaluar cv() para ese año

#### SIMPROF y coherence plots ####
# estandarizar media = 0, varianza = 1
amb.tidy.stand_max <- amb.tidy %>%
  mutate(across(Temperatura:Clorofilas, # "across()" no permite evaluar argumentos ...
                ~decostand(., method = 'standardize', na.rm = T))) # ...se debe usar función anónima "~"

# Matriz de correlación (Pearson)
amb.tidy.stm.cor <- as_tibble(cor(amb.tidy.stand_max[1:44],
                                  method = 'pearson',
                                  use = 'pairwise.complete.obs'))

# SIMPROF
amb.tidy.simprof <- simprof(amb.tidy.stm.cor,
                            sample.orientation = 'column',
                            method.cluster = 'average',
                            num.expected = 1000,
                            num.simulated = 999,
                            alpha = 0.05)

simprof.plot(amb.tidy.simprof)

# Coherence plots  
cl <- colors() # para evaluar un color aleatorio 
for (i in 1:14) {
  p <- ggplot(data = amb.tidy.stand_max, aes(x = fecha, col = 'red'))
    for (x in amb.tidy.simprof$significantclusters[[i]]) {
      p <- p +
        geom_line(aes(y = .data[[x]], 
                      color = cl[sample(9:500, 1)])) # pendiente alternativa sin aleatorización
    }
  p <- p + 
    scale_color_hue(name = 'Parámetro', 
                    labels = amb.tidy.simprof$significantclusters[[i]]) +
    labs(title = paste(amb.tidy.simprof$significantclusters[[i]], collapse = ', '),
         x = 'fecha', y = 'valor')
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(paste('figuras/coherence/cohplot_', i, '.png'), plot = p,
         width = 1920, height = 1080, units = 'px', pointsize = 12, 
         bg = 'white',dpi = 300)
}

#### test de Mantel
# Matriz de correlación (Pearson)
amb.tidy.stm.cor <- as_tibble(cor(amb.tidy.stand_max[1:44], use = 'complete.obs'))
noc <- amb.tidy.stm.cor %>% 
  select(-c(15, 22, 29, 33)) %>% 
  slice(-c(15, 22, 29, 33))

# Distancia euclidiana
amb.tidy.stm.euc <- as_tibble(as.matrix((dist(c, method = 'euclidean')))) 
noc2 <- amb.tidy.stm.euc %>% 
  select(-c(15, 22, 29, 33)) %>% 
  slice(-c(15, 22, 29, 33))

# test de Mantel
mantel(xdis = as.dist(noc), ydis = as.dist(noc2), method = 'spearman', permutations = 999)
