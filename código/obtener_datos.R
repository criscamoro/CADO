# Obtener y limpiar datos de la Comisión Estatal del Agua Jalisco

# Argumentos para la función ----
# 1: url que contiene los datos en el portal de la CEA Jalisco
# 2: Nombre del cuerpo de agua, usado para accedera a la pestaña con los datos
# 3: Puntos de muestreo: Para acceder a la pestaña con los nombres de las estaciones, debido a que algunas están mal escritas
caji <- c("https://www.ceajalisco.gob.mx/contenido/datos_abiertos/LagunaCajititlan.xlsx", "Laguna de Cajititlán", "Puntos de Muestreo")
zapo <- c("https://www.ceajalisco.gob.mx/contenido/datos_abiertos/LagunaZapotlan.xlsx", "Laguna Zapotlán", "Puntos de Muestreo")
verde <- c("https://www.ceajalisco.gob.mx/contenido/datos_abiertos/RioVerde.xlsx", "Río Verde", "Puntos de Muestreo")
lerma <- c("https://www.ceajalisco.gob.mx/contenido/datos_abiertos/RioZula-Lerma.xlsx", "Río Zula-Lerma", "Puntos de Muestro") #"Muestro"
santi <- c("https://www.ceajalisco.gob.mx/contenido/datos_abiertos/RioSantiago.xlsx", "Río Santiago", "Puntos de Muestro") #"Muestro"

# Función para procesar los datos ----
# se corrigen irregularidades en los datos, mencionando entre paréntesis el set de datos correspondiente
# Cuatro variables cualitativas son excluidas: Color, Color verdadero, Olor y Gasto (41, 42, 43, 50)
# se utiliza "suppressWarnings()" para evitar la advertencia "! NAs introduced by coercion", esperable en todos los sets de datos
procesar_datos <- function(x) {
  datos_amb <- as_tibble(read.xlsx(x[1], sheet = x[2], detectDates = T)) %>%
    mutate(idMuestra = suppressWarnings(as.numeric(idMuestra))) %>%
    filter(fecha != "NULL") %>%
    filter(
      !between(idMuestra, 112361, 112610), # (caji) datos de "Materia flotante" valor = 0 y fechas inconexas
      !between(idMuestra, 122491, 122534), # (zapo) muestras duplicadas (se toma la idMuestra más antigua)
      !between(idMuestra, 99587, 99631), # (verde) datos duplicados
      !between(idMuestra, 77532, 77903), # (verde) puntos de muestreo que no correponden a este cuerpo de agua
      !idMuestra %in% c(12890, 12981, 12893, 52932, 52885, 52888, 52976, 53020), # (santi) muestras duplicadas
      !idPuntoMuestreo == 34, # No pertenece a ninguno de los puntos de muestreo de los 5 cuerpos de agua
      !idParametro %in% c(41, 42, 43, 50, 39), # Variables cualitativas
      !valor == "-"
    ) %>%
    mutate(valor = replace(valor, idMuestra == 68975, 2.98)) %>%  # (caji) 06/2015 est. 04 - Sulfatos (antes 298)
    mutate(valor = replace(valor, idMuestra == 81675, 1.2)) %>%  # (caji) 06/2016 est. 02 - Aluminio (antes 120)
    mutate(valor = replace(valor, idMuestra == 81676, 0.00286)) %>%  # (caji) 06/2016 est. 02 - Arsénico (antes 0.286)
    mutate(valor = replace(valor, idMuestra == 81685, 0.001)) %>% # (caji) 06/2016 est. 02 - Plomo (antes 0.1)
    mutate(valor = replace(valor, idMuestra == 81687, 0.1)) %>% # (caji) 06/2016 est. 02 - Zinc (antes 111.1)
    mutate(valor = replace(valor, idMuestra == 83003, 746)) %>%  # (caji) 12/2016 est. 02 - Conductividad (antes 7.46)
    mutate(valor = replace(valor, idMuestra == 86135, 741)) %>%  # (caji) 12/2016 est. 05 - Conductividad (antes 7.41)
    select(-1) %>%
    mutate(fecha = as.Date(gsub("2017-04-24", "2017-04-27", fecha))) %>% #fecha errónea
    mutate(valor = as.character(gsub("<", "", valor))) %>%
    mutate(valor = suppressWarnings(as.numeric(valor))) %>%
    mutate(idParametro = as.character(idParametro)) %>%
    mutate(idParametro = as.character(mgsub(
      as_tibble(read.xlsx(x[1], sheet = "Parametros"))$idParametros,
      as_tibble(read.xlsx(x[1], sheet = "Parametros"))$param, idParametro
    ))) %>%
    mutate(idParametro = replace(idParametro, idParametro == "DBO Turbiedad", "DBO")) %>%  # DBO está como "DBO 5"
    mutate(idPuntoMuestreo = as.character(mgsub(
      (as_tibble(read.xlsx(x[1], sheet = x[3], fillMergedCells = T)) %>%
        filter(!is.na(idPunto)) %>%
        mutate(idPunto = suppressWarnings(as.numeric(idPunto))) %>%
        filter(!between(idPunto, 14, 23)))$idPunto, # (caji) estaciones abandonadas (provocan errores al pivotar)
      (as_tibble(read.xlsx(x[1], sheet = x[3], fillMergedCells = T)) %>%
        filter(!is.na(idPunto)) %>%
        mutate(idPunto = suppressWarnings(as.numeric(idPunto))) %>%
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

procesar_datos(caji)
procesar_datos(zapo)
procesar_datos(verde)
procesar_datos(lerma)
procesar_datos(santi)

# Guardar como csv ----

write.csv(caji_amb_tidy, "datos/tidy/caji_amb_tidy.csv", row.names = F, na = "")
write.csv(zapo_amb_tidy, "datos/tidy/zapo_amb_tidy.csv", row.names = F, na = "")
write.csv(verde_amb_tidy, "datos/tidy/verde_amb_tidy.csv", row.names = F, na = "")
write.csv(lerma_amb_tidy, "datos/tidy/lerma_amb_tidy.csv", row.names = F, na = "")
write.csv(santi_amb_tidy, "datos/tidy/santi_amb_tidy.csv", row.names = F, na = "")
