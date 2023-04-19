# Obtener y limpiar datos de la Comisión Estatal del Agua Jalisco

# Argumentos para la función ----
caji <- c("https://www.ceajalisco.gob.mx/contenido/datos_abiertos/LagunaCajititlan.xlsx", "Laguna de Cajititlán", "Puntos de Muestreo")
zapo <- c("https://www.ceajalisco.gob.mx/contenido/datos_abiertos/LagunaZapotlan.xlsx", "Laguna Zapotlán", "Puntos de Muestreo")
verde <- c("https://www.ceajalisco.gob.mx/contenido/datos_abiertos/RioVerde.xlsx", "Río Verde", "Puntos de Muestreo")
lerma <- c("https://www.ceajalisco.gob.mx/contenido/datos_abiertos/RioZula-Lerma.xlsx", "Río Zula-Lerma", "Puntos de Muestro")
santi <- c("https://www.ceajalisco.gob.mx/contenido/datos_abiertos/RioSantiago.xlsx", "Río Santiago", "Puntos de Muestro")

# Función para procesar los datos ----
data_process <- function(x) {
  datos_amb <- as_tibble(read.xlsx(x[1], sheet = x[2], detectDates = T)) %>%
    mutate(idMuestra = as.numeric(idMuestra)) %>%
    filter(fecha != "NULL") %>%
    filter(
      !between(idMuestra, 112361, 112610),
      !between(idMuestra, 99587, 99631),
      !between(idMuestra, 122491, 122534), !between(idMuestra, 77532, 77903),
      !idPuntoMuestreo == 34,
      !idMuestra %in% c(12893, 12890, 12981, 52932, 52885, 52888, 52976, 53020)
    ) %>%
    select(-1) %>%
    mutate(fecha = as.Date(gsub("2017-04-24", "2017-04-27", fecha))) %>%
    mutate(valor = as.character(gsub("<", "", valor))) %>%
    mutate(valor = as.numeric(gsub("-", "", valor))) %>%
    mutate(idParametro = as.character(idParametro)) %>%
    mutate(idParametro = as.character(mgsub(
      as_tibble(read.xlsx(x[1], sheet = "Parametros"))$idParametros,
      as_tibble(read.xlsx(x[1], sheet = "Parametros"))$param, idParametro
    ))) %>%
    mutate(idPuntoMuestreo = as.character(mgsub(
      (as_tibble(read.xlsx(x[1], sheet = x[3], fillMergedCells = T)) %>%
        filter(!is.na(idPunto)) %>%
        mutate(idPunto = as.numeric(idPunto)) %>%
        filter(!between(idPunto, 14, 23)))$idPunto,
      (as_tibble(read.xlsx(x[1], sheet = x[3], fillMergedCells = T)) %>%
        filter(!is.na(idPunto)) %>%
        mutate(idPunto = as.numeric(idPunto)) %>%
        filter(!between(idPunto, 14, 23)))$clave, idPuntoMuestreo
    ))) %>%
    pivot_wider(names_from = "idParametro", values_from = "valor") %>%
    rename(est = idPuntoMuestreo) %>%
    mutate(est = as.factor(est)) %>%
    mutate(año = as.factor(year(fecha))) %>%
    mutate(mes = as.factor(month(fecha))) %>%
    relocate(est, .after = mes) %>%
    relocate(fecha, .before = año)
  assign(paste(deparse(substitute(x)), "_amb_tidy", sep = ""), datos_amb, .GlobalEnv)
}

# Cargar datos en el Global Enviroment ----

data_process(caji)
data_process(zapo)
data_process(verde)
data_process(lerma)
data_process(santi)

# Guardar objetos en RDS ----

saveRDS(caji_amb_tidy, "datos/tidy/caji_amb_tidy.rds")
saveRDS(zapo_amb_tidy, "datos/tidy/zapo_amb_tidy.rds")
saveRDS(verde_amb_tidy, "datos/tidy/verde_amb_tidy.rds")
saveRDS(lerma_amb_tidy, "datos/tidy/lerma_amb_tidy.rds")
saveRDS(santi_amb_tidy, "datos/tidy/santi_amb_tidy.rds")
