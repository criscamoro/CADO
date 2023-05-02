# Programa de monitoreo de la limnología de cuerpos de agua dulce de Occidente

#### Paquetes ####
library(tidyverse)
library(openxlsx)
library(qdap)
library(lubridate)
library(ggtext)
library(ggside)
library(ggforce)
library(corrplot)
library(vtable)
library(vegan)
library(clustsig)
library(treemapify)

#### Preparación de datos ####
# Datos ambientales ----
# Nombres
nom <- c(
  "caji", # Laguna de Cajititlán
  "zapo", # Laguna de Zapotlán
  "verde", # Río Verde
  "lerma", # Río Zula-Lerma
  "santi" # Río Santiago
) 

# Actualizar datos
source("código/get_data.R")

# Cargar datos en el Global Enviroment
datos <- function(n) {
  assign(
    paste(n, "_amb_tidy", sep = ""),
    read_csv(paste("datos/tidy/", n, "_amb_tidy.csv", sep = "")),
    .GlobalEnv
  )
}

lapply(nom, datos)

# Formato rectangular (key-value)
guardar_rect <- function(n) {
  write.csv(
    get(paste(n, "_amb_tidy", sep = "")) %>%
      pivot_longer(
        cols = !c(fecha, año, mes, est),
        names_to = "parámetro",
        values_to = "valor"
      ),
    paste("datos/rectangulares/", n, "_amb_rect.csv", sep = ""),
    row.names = F,
    na = ""
  )
}

lapply(nom, guardar_rect)

# Cargar datos rectangulares en el Global Enviroment
datos_rect <- function(n) {
  assign(
    paste(n, "_amb_rect", sep = ""),
    read_csv(paste("datos/rectangulares/", n, "_amb_rect.csv", sep = "")),
    .GlobalEnv
  )
}

lapply(nom, datos_rect)

# Zooplancton de la Laguna de Cajititlán ----
# Base de datos de zooplancton formato tidy
write.csv(
  as_tibble(read.xlsx("datos/crudos/Cajititlán_Bio.xlsx", sheet = "ZooP", colNames = F, startRow = 3)) %>%
    column_to_rownames("X1") %>% t(),
  "datos/tidy/caji_zoo_tidy.csv",
  row.names = F,
  na = "0"
)

caji_zoo_tidy <- read_csv("datos/tidy/caji_zoo_tidy.csv")

# Base de datos de zooplancton rectangular (key-value)
write.csv(
  caji_zoo_tidy %>%
    pivot_longer(
      cols = !c(año, mes, est),
      names_to = "taxa",
      values_to = "conteo"
    ),
  "datos/rectangulares/caji_zoo_rect.csv",
  row.names = F,
  na = "0"
)

caji_zoo_rect <- read_csv("datos/rectangulares/caji_zoo_rect.csv")

# Fitoplancton de la Laguna de Cajititlán ----
# Base de datos de fitoplancton formato tidy
write.csv(
  read.xlsx("datos/crudos/Cajititlán_Bio.xlsx", sheet = "FitoP", colNames = F, startRow = 3, cols = c(1:317)) %>%
    slice(-65) %>%
    column_to_rownames("X1") %>% 
    t() %>% as_tibble() %>% 
    mutate(fecha = make_date(año, mes)) %>% 
    relocate(fecha, .before = año),
  "datos/tidy/caji_fito_tidy.csv",
  row.names = F,
  na = "0"
)

caji_fito_tidy <- read_csv("datos/tidy/caji_fito_tidy.csv")

# Información taxonómica
write.csv(
  as_tibble(read.xlsx("datos/crudos/Cajititlán_Bio.xlsx", sheet = "FitoT")),
  "datos/crudos/caji_fito_taxo.csv", 
  row.names = F
)

caji_fito_taxo <- read_csv("datos/crudos/caji_fito_taxo.csv")

# Base de datos de fitoplancton rectangular (key-value), con datos taxonómicos
write.csv(
  caji_fito_tidy %>%
    pivot_longer(
      cols = !c(fecha, año, mes, est),
      names_to = "taxa",
      values_to = "conteo"
    ) %>% 
    mutate(Género = word(taxa, 1)) %>% 
    left_join(caji_fito_taxo, by = "Género") %>% 
    relocate(Género, .after = Familia) %>% 
    relocate(taxa, .after = Género) %>% 
    relocate(conteo, .after = taxa),
  "datos/rectangulares/caji_fito_rect.csv",
  row.names = F,
  na = "0"
)

caji_fito_rect <- read_csv("datos/rectangulares/caji_fito_rect.csv")

#### Análisis exploratorio ####
# Cuadros de resumen estadístico ----
# por parámetro, por año, en formato csv
cre <- function(x, n) {
  lapply(colnames(x)[-c((length(colnames(x)) - 3):length(colnames(x)))], function(i) {
    x %>%
      select(i, año) %>%
      group_by(año) %>%
      mutate(rn = row_number()) %>%
      pivot_wider(names_from = 2, values_from = 1) %>%
      select(-1) %>%
      st(
        title = paste("Cuadro de resumen estadístico:", i),
        summ = list(c("notNA(x)", "mean(x)", "sd(x)", "min(x)", "max(x)", "(sd(x, na.rm = T)/mean(x, na.rm = T) * 100)")),
        summ.names = list(c("N", "Media", "D.E.", "Min", "Max", "C.V.")),
        out = "csv",
        file = paste("figuras/cuadros/", n, "/resumen_", n, "_", i, ".csv", sep = "")
      )
  })
}

cre(caji_amb_tidy, "caji")
cre(zapo_amb_tidy, "zapo")
cre(verde_amb_tidy, "verde")
cre(lerma_amb_tidy, "lerma")
cre(santi_amb_tidy, "santi")

# Gráficos de series temporales ----
# Gráfico de series temporales, localidades completas, agrupados por parámetro
gst_wrap <- function(n) {
  ggplot(data = get(paste(n, "_amb_rect", sep = ""))) +
    geom_line(mapping = aes(
      x = fecha,
      y = valor,
      color = est
    )) +
    facet_wrap(~parámetro, scales = "free")
}

lapply(nom, gst_wrap)

# Matriz de correlaciones (Laguna de Cajititlán)----
# para identificar posibles variables redundantes

caji_amb_tidy_cor <- cor(caji_amb_tidy[, 1:45], use = "pairwise.complete.obs") # matriz de correlación

corrplot(caji_amb_tidy_cor,
  type = "upper",
  col = brewer.pal(n = 8, name = "RdYlBu")
)

# Aislar esas variables
caji_amb_tidy_cor_cured <- caji_amb_tidy %>%
  select(
    Conductividad, `Alcalinidad total`, `Cloruros totales`, `Nitrógeno total`,
    `Nitrógeno total Kjeldahl`, SST, Sodio, `Sólidos disueltos tot.`, `Sólidos totales`,
    `Fósforo total`
  ) %>%
  cor(use = "pairwise.complete.obs")

corrplot(caji_amb_tidy_cor_cured,
  type = "upper",
  col = brewer.pal(n = 8, name = "RdYlBu")
)

# Series temporales del coeficiente de variación (Laguna de Cajititlán) ----
# Coeficiente de variación por año
cdv <- function(x) {
  (sd(x, na.rm = T) / mean(x, na.rm = T) * 100)
}

caji_amb_tidy_cv_a <- caji_amb_tidy %>%
  group_by(año) %>%
  summarise(across(
    Temperatura:`Materia flotante`,
    cdv
  ))

for (i in colnames(caji_amb_tidy_cv_a[2:46])) {
  ggsave(paste("figuras/cov/año/", i, "_cov.png", sep = ""),
    plot =
      ggplot(data = caji_amb_tidy_cv_a) +
        geom_line(mapping = aes(
          x = año,
          y = .data[[i]],
          group = 1
        )) +
        labs(
          title = paste(i, "(coeficiente de variación)"),
          y = "COV"
        ) +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme_classic(),
    width = 1920, height = 1080, units = "px", pointsize = 12,
    bg = "white", dpi = 300
  )
}

# Coeficiente de variación por año y mes (fecha)
caji_amb_tidy_cv_am <- caji_amb_tidy %>%
  group_by(fecha) %>%
  summarise(across(
    Temperatura:`Materia flotante`,
    cdv
  ))

for (i in colnames(caji_amb_tidy_cv_am[2:46])) {
  ggsave(paste("figuras/cov/año+mes/", i, "_cov_am.png", sep = ""),
    plot =
      ggplot(data = caji_amb_tidy_cv_am) +
        geom_line(mapping = aes(x = fecha, y = .data[[i]], group = 1)) +
        labs(
          title = paste(i, "(coeficiente de variación)"),
          x = "fecha", y = "COV"
        ) +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme_classic(),
    width = 1920, height = 1080, units = "px", pointsize = 12,
    bg = "white", dpi = 300
  )
}

# SIMPROF y coherence plots (Laguna de Cajititlán) ----
# estandarizar media = 0, varianza = 1
caji_amb_tidy_stm <- amb.tidy %>%
  mutate(across(
    Temperatura:Clorofilas, # "across()" no permite evaluar argumentos ...
    ~ decostand(., method = "standardize", na.rm = T) # ...se debe usar función anónima "~"
  )) 

# Matriz de correlación (Pearson)
caji_amb_tidy_stm_cor <- as_tibble(cor(caji_amb_tidy_stm[1:44],
  method = "pearson",
  use = "pairwise.complete.obs"
))

# SIMPROF
caji_amb_tidy_simprof <- simprof(caji_amb_tidy_stm_cor,
  sample.orientation = "column",
  method.cluster = "average",
  num.expected = 1000,
  num.simulated = 999,
  alpha = 0.05
)

simprof_plot(amb_tidy_simprof)

# Coherence plots

coh_plot <- function(i) {
  col_vec <- scales::hue_pal()(sum(lengths(i)))
  names(col.vec) <- unlist(i)
  ggsave(paste("figuras/coherence/cohplot_", paste(i, collapse = "_"), ".png", sep = ""),
    plot =
      ggplot(data = caji_amb_tidy_stm, aes(x = fecha)) +
        lapply(i, function(x) {
          geom_line(aes(y = .data[[x]], color = x))
        }) +
        scale_color_manual(name = "Parámetro", values = col.vec) +
        labs(
          title = paste(i, collapse = ", "),
          x = "fecha", y = "valor"
        ) +
        theme_classic() +
        theme(plot.title = element_textbox_simple(halign = 0.5, margin = unit(c(5, 0, 5, 0), "pt"))),
    width = 1920, height = 1080, units = "px", pointsize = 12,
    bg = "white", dpi = 300
  )
}

lapply(caji_amb_tidy_simprof$significantclusters, coh_plot)

#### test de Mantel
# Matriz de correlación (Pearson)
caji_amb_tidy_stm_cor <- as_tibble(cor(caji_amb_tidy_stm[1:44], use = "complete.obs"))
noc <- caji_amb_tidy_stm_cor %>%
  select(-c(15, 22, 29, 33)) %>%
  slice(-c(15, 22, 29, 33))

# Distancia euclidiana
caji_amb_tidy_stm_euc <- as_tibble(as.matrix((dist(c, method = "euclidean"))))
noc2 <- caji_amb_tidy_stm_euc %>%
  select(-c(15, 22, 29, 33)) %>%
  slice(-c(15, 22, 29, 33))

# test de Mantel
mantel(xdis = as.dist(noc), ydis = as.dist(noc2), method = "spearman", permutations = 999)

# Análisis del fitoplancton (Laguna de Cajititlán) ----
# Gráfico de series temporales por especie

ggplot(data = caji_fito_rect) +
  geom_line(mapping = aes(
    x = año,
    y = conteo
  )) +
  facet_wrap(~taxa, scales = "free")

# Cuadro de resumen de taxa

treemap <- function(id) {
  ggplot(data = caji_fito_rect %>% 
           group_by({{id}}) %>% 
           summarise(conteo = sum(conteo)), 
         aes(area = conteo, fill = {{id}},
         label = paste({{id}}, conteo, sep = "\n"))) +
    geom_treemap() +
    geom_treemap_text(color = "white", place = "centre", size = 15)
}
treemap(Phylum)

# Abundancia
# absoluta
ab_n <- function(periodo) {
  ggplot(
    data = caji_fito_rect %>% 
      group_by({{periodo}}) %>% 
      summarise(conteo = sum(conteo)),
    aes(
      x = {{periodo}},
      y = conteo
      )
    ) +
    geom_line() +
    geom_point()
}

ab_n(año)

# relativa
ab_f <- function(id, periodo) {
  ggplot(
    data = caji_fito_rect %>% 
      group_by({{periodo}}) %>% 
      mutate(ni = (conteo/sum(conteo))*100),
    aes(
      x = {{periodo}},
      y = ni,
      fill = {{id}}
    )
  ) +
    geom_bar(stat = "identity")
}

ab_f(Phylum, año)

# Índices de Diversidad univariados por taxa, por fecha (Sobs, H' y D')
# Sobs
sobs_n <- function(periodo) {
  ggplot(
    data = caji_fito_rect %>% 
      group_by({{periodo}}) %>% 
      filter(conteo > 0) %>% 
      summarise(S = length(unique(taxa))),
    aes(
      x = {{periodo}},
      y = S
    )
  ) +
    geom_line() +
    geom_point()
}

sobs_n(fecha)
sobs_n(año)  

sobs_f <- function(periodo) {
  ggplot(
    data = caji_fito_rect %>% 
      group_by({{periodo}}) %>% 
      filter(conteo > 0) %>% 
      distinct(taxa, .keep_all = T) %>% 
      mutate(v_a = 1) %>% 
      group_by({{periodo}}, Phylum) %>% 
      summarise(v_a = sum(v_a)) %>% 
      group_by({{periodo}}) %>% 
      mutate(`%` = (v_a/sum(v_a))*100),
    aes(
      x = {{periodo}},
      y = `%`, 
      fill = Phylum
    )
  ) +
    geom_bar(stat = "identity")
}

sobs_f(año)
sobs_f(fecha)

# Shannon
shannon <- function(x) {
  f <- (x[x > 0]/sum(x))
  -sum(f * log(f, 2))
}

sh <- function(periodo) {
  ggplot(
    data = caji_fito_rect %>% 
      group_by({{periodo}}, taxa) %>% 
      summarise(conteo = sum(conteo)) %>% 
      group_by({{periodo}}) %>% 
      summarise(`H'` = shannon(conteo)),
    aes(
      x = {{periodo}},
      y = `H'`
    )
  ) +
    geom_line() +
    geom_point()
}

sh(año)
sh(fecha)
