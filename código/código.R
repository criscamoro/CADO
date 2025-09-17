# Programa de monitoreo de la limnología de Cuerpos de Agua Dulce de Oriente
# Instituto de Limnología, Centro Universitario de Ciencias Biológicas y Agropecuarias de la Universidad de Guadalajara
# Cristofer Camarena Orozco (camarenaocristofer@gmail.com)

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
#library(clustsig)
library(treemapify)
library(skimr)
library(tidymodels)
library(embed)
library(tidytext)

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
source("código/obtener_datos.R")

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

# Perfil de calidad de datos
caji_amb_tidy %>% skim()
zapo_amb_tidy %>% skim()
verde_amb_tidy %>% skim()
lerma_amb_tidy %>% skim()
santi_amb_tidy %>% skim()

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
# Cuadro de resumen de taxa
treemap <- function(nvl) {
  ggplot(data = caji_fito_rect %>% 
           group_by({{nvl}}) %>% 
           summarise(conteo = sum(conteo)), 
         aes(area = conteo, fill = {{nvl}},
             label = paste({{nvl}}, conteo, sep = "\n"))) +
    geom_treemap() +
    geom_treemap_text(color = "white", place = "centre", size = 15)
}
treemap(Phylum)

# Abundancia, Riqueza y Diversidad por taxa, por fecha (N, Ni, Sobs, H')
# Abundancia absoluta
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

# Abundancia relativa
ab_f <- function(nvl, periodo) {
  ggplot(
    data = caji_fito_rect %>% 
      group_by({{periodo}}) %>% 
      mutate(ni = (conteo/sum(conteo))*100),
    aes(
      x = {{periodo}},
      y = ni,
      fill = {{nvl}}
    )
  ) +
    geom_bar(stat = "identity")
}

ab_f(Phylum, año)

# Riqueza absoluta
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

# Composición de la riqueza
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

# Entropía de Shannon-Wiener (bits)
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

# nMDS
# Suma de conteo por fecha
caji_fito_mds_fecha <- metaMDS(caji_fito_tidy %>% 
                               group_by(fecha) %>% 
                               summarise(across(1:61, sum)) %>% 
                               select(!c(fecha)), trymax = 1000)

mds_fecha_plot <- ggplot(as.tibble(caji_fito_mds_fecha$points) %>% mutate(fecha = unique(caji_fito_tidy$fecha)), 
                         aes(x = MDS1, y = MDS2)) + 
  geom_point(aes(size = 6, color = fecha)) +
  labs(title = "nMDS del fitoplancton de la Laguna de Cajititlán de 2014 a 2019") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(),
    panel.background = element_blank()
    )

ggsave("figuras/ord_fito/nmds_caji_fito_fecha.png", mds_fecha_plot,
       width = 1920, height = 1080, units = "px", pointsize = 12,
       bg = "white", dpi = 300)

# Suma de conteo por año
caji_fito_mds_año <- metaMDS(caji_fito_tidy %>% 
                               group_by(año) %>% 
                               summarise(across(1:61, sum)) %>% 
                               select(!c(año)), trymax = 1000)

mds_año_plot <- ggplot(as.tibble(caji_fito_mds_año$points) %>% mutate(año = unique(caji_fito_tidy$año)), 
       aes(x = MDS1, y = MDS2, label = año)) + 
  geom_point(aes(size = 6, color = "green")) +
  geom_text(vjust = -0.5) +
  geom_line(arrow = arrow(length = unit(0.5, "cm"), type = "closed")) +
  labs(title = "nMDS (año)") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none", 
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(),
    panel.background = element_blank()
    ) 

ggsave("figuras/ord_fito/nmds_caji_fito_año.png", mds_año_plot,
       width = 1920, height = 1080, units = "px", pointsize = 12,
       bg = "white", dpi = 300)

# Suma de conteo por mes
caji_fito_mds_mes <- metaMDS(caji_fito_tidy %>% 
                                 group_by(mes) %>% 
                                 summarise(across(1:61, sum)) %>% 
                                 select(!c(mes)), trymax = 1000)

mds_mes_plot <- ggplot(as.tibble(caji_fito_mds_mes$points) %>% mutate(mes = 1:12), 
       aes(x = MDS1, y = MDS2, label = mes)) + 
  geom_point(aes(size = 6, color = "green")) +
  geom_text(vjust = -0.8) +
  labs(title = "nMDS del fitoplancton de la Laguna de Cajititlán, agrupado por mes") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none", axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(),
    panel.background = element_blank()
    )
ggsave("figuras/ord_fito/nmds_caji_fito_mes.png", mds_mes_plot,
       width = 1920, height = 1080, units = "px", pointsize = 12,
       bg = "white", dpi = 300)

# PCoA por año
# Matriz de asociación Bray-Curtis con transformación doble raíz cuadrada
caji_fito_sqrt2bray <- vegdist(caji_fito_tidy %>% 
                          group_by(año) %>% 
                          summarise(across(1:61, ~sqrt(sqrt(sum(.))))) %>% 
                          select(!c(año)), method = "bray")

caji_fito_pcoa <- cmdscale(caji_fito_sqrt2bray, eig = T)

pcoa_año_plot <- ggplot(as.tibble(caji_fito_pcoa$points) %>% mutate(año = unique(caji_fito_tidy$año)), 
       aes(x = V1, y = V2, label = año)) + 
  geom_point(aes(size = 6, color = "green")) +
  geom_text(vjust = -0.6) +
  geom_line(arrow = arrow(length = unit(0.5, "cm"), type = "closed")) +
  labs(title = "PCoA del fitoplancton de la Laguna de Cajititlán de 2014 a 2019", 
       x = paste("PCoA 1 (", round(caji_fito_pcoa$eig[1]/sum(caji_fito_pcoa$eig) * 100, digits = 2), "%)", sep = ""), 
       y = paste("PCoA 2 (", round(caji_fito_pcoa$eig[2]/sum(caji_fito_pcoa$eig) * 100, digits = 2), "%)", sep = "")) +
  scale_x_reverse() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none", 
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(),
    panel.background = element_blank()
  ) 

ggsave("figuras/ord_fito/pcoa_caji_fito_año.png", pcoa_año_plot,
       width = 1920, height = 1080, units = "px", pointsize = 12,
       bg = "white", dpi = 300)

#### Modelación exploratoria ####
# Modelo linear del fósforo total (Cajititlán) en función de la fecha ----
ml_P <- lm(`Fósforo total` ~ fecha, caji_amb_tidy)

# coeficientes 
coef(ml_P)

# R2 ajustado
summary(ml_P)$adj.r.squared

# Gráfico
ggplot(caji_amb_tidy, aes(fecha, `Fósforo total`)) +
  geom_point(aes(color = est)) +
  geom_abline(aes(
    intercept = coef(ml_P)[[1]], 
    slope = coef(ml_P)[[2]]
    ), color = "blue") +
  theme_classic()

# Reducción de dimensionalidad (unsupervised machine learning) ----
# PCA ----
# recipe y prep
rec_pca <- function(datos) {
    prep(
      recipe(~ ., data = datos) %>% # todos los datos (unsupervised)
        update_role(fecha, año, new_role = "tiempo") %>% 
        update_role(mes, new_role = "ciclo") %>% 
        update_role(est, new_role = "sitio") %>% 
        step_naomit(all_predictors()) %>% # omitir missing values
        step_normalize(all_predictors()) %>% # media = 0, D.E. = 1
        step_pca(all_predictors())) # PCA
}

# Resultados PCA
caji_amb_pca <- rec_pca(caji_amb_tidy)
zapo_amb_pca <- rec_pca(zapo_amb_tidy)
verde_amb_pca <- rec_pca(verde_amb_tidy)
lerma_amb_pca <- rec_pca(lerma_amb_tidy)
santi_amb_pca <- rec_pca(santi_amb_tidy)

# Barras horizontales de los componentes
pca_plot1 <- function(pca_res) {
  tidy(pca_res, 3) %>% 
    filter(component %in% paste0("PC", 1:5)) %>% 
    mutate(component = fct_inorder(component)) %>% 
    ggplot(aes(value, terms, fill = terms)) +
    geom_col(show.legend = F) +
    facet_wrap(~component, nrow = 1) +
    labs(y = NULL) +
    theme_classic()
}

pca_plot1(caji_amb_pca)
pca_plot1(zapo_amb_pca)
pca_plot1(verde_amb_pca)
pca_plot1(lerma_amb_pca)
pca_plot1(santi_amb_pca)

# Barras de componentes más importantes
pca_plot2 <- function(pca_res) {
  tidy(pca_res, 3) %>% 
    filter(component %in% paste0("PC", 1:4)) %>% 
    group_by(component) %>% 
    top_n(8, abs(value)) %>% 
    ungroup() %>% 
    mutate(terms = reorder_within(terms, abs(value), component)) %>% 
    ggplot(aes(abs(value), terms, fill = value > 0)) +
    geom_col() +
    scale_y_reordered() +
    facet_wrap(~component, scales = "free_y") +
    labs(y = NULL, fill = "¿Positivo?")
}

pca_plot2(caji_amb_pca)
pca_plot2(zapo_amb_pca)
pca_plot2(verde_amb_pca)
pca_plot2(lerma_amb_pca)
pca_plot2(santi_amb_pca)

# Ordenación
pca_plot3 <- function(pca_res, fac_col) {
  juice(pca_res) %>% 
    ggplot(aes(PC1, PC2)) +
    geom_point(aes(color = {{fac_col}}), alpha = 0.7, size = 2) + 
    theme_classic()
}

pca_plot3(caji_amb_pca, año)
pca_plot3(zapo_amb_pca, mes)
pca_plot3(verde_amb_pca, est)
pca_plot3(lerma_amb_pca, mes)
pca_plot3(santi_amb_pca, año)

# UMAP ----
rec_umap <- function(datos) {
  prep(
    recipe(~ ., data = datos) %>% 
      update_role(fecha, año, new_role = "tiempo") %>% 
      update_role(mes, new_role = "ciclo") %>% 
      update_role(est, new_role = "sitio") %>% 
      step_naomit(all_predictors()) %>% 
      step_normalize(all_predictors()) %>% 
      step_umap(all_predictors()))
}

caji_amb_umap <- rec_umap(caji_amb_tidy)
zapo_amb_umap <- rec_umap(zapo_amb_tidy)
verde_amb_umap <- rec_umap(verde_amb_tidy)
lerma_amb_umap <- rec_umap(lerma_amb_tidy)
santi_amb_umap <- rec_umap(santi_amb_tidy)

umap_plot <- function(umap_res, fac_col) {
  juice(umap_res) %>% 
    ggplot(aes(UMAP1, UMAP2)) +
    geom_point(aes(color = {{fac_col}}), alpha = 0.7, size = 2) + 
    theme_classic()
}

umap_plot(caji_amb_umap, año)
umap_plot(zapo_amb_umap, año)
umap_plot(verde_amb_umap, est)
umap_plot(lerma_amb_umap, mes)
umap_plot(santi_amb_umap, año)
