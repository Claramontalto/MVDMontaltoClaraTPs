# ------------------------------------------------------------
# ------------------------------------------------------------
# Voy a computar la DTM, filtrar 5 términos relevantes y generar
# un gráfico de barras con su frecuencia
# ------------------------------------------------------------
# ------------------------------------------------------------

# LIBRERÍAS --------------------------------------------------

library(tidyverse)
library(tm)
library(here)
library(tidytext) # Análisis de texto

# RUTAS ------------------------------------------------------ 

output_dir <- here("TP2/output")

# CARGAR DATOS -----------------------------------------------

# Cargamos el archivo procesado por processing.R
message("Cargando datos procesados")
comunicados_procesados <- readRDS(file.path(output_dir, "processed_text.rds"))
message("Datos cargados")

# CREAR LA DTM -----------------------------------------------

# La DTM es una matriz donde filas = documentos y columnas = términos
message("Creando la DTM")
# Primero contamos la frecuencia de cada lema por documento
frecuencia_tokens <- comunicados_procesados |>
  count(id, lemma, name = "n") |>
  arrange(id)
# Convertimos a DTM usando cast_dtm() de tidytext
matriz_dtm <- frecuencia_tokens |>
  cast_dtm(
    document = id, 
    term = lemma,
    value = n)
message("DTM creada")

# FILTRAR 5 TÉRMINOS DE INTERÉS ------------------------------

# Elegimos 5 términos relevantes para el contexto de la OEA
terminos_de_interes <- c("democracia", "elección", "misión", "país", "región")
matriz_filtrada <- matriz_dtm[, colnames(matriz_dtm) %in% terminos_de_interes]
message("Términos filtrados")

# CALCULAR FRECUENCIA TOTAL ----------------------------------

# Sumamos las frecuencias de cada término en todos los documentos
# as.matrix(), rownames_to_column(), pivot_longer() 
dtm_df <- as.data.frame(as.matrix(matriz_filtrada)) |>
  rownames_to_column(var = "id") |>
  pivot_longer(-id, names_to = "lemma", values_to = "n") |>
  group_by(lemma) |>
  summarise(frecuencia_total = sum(n))
message("Frecuencias calculadas")

# GRÁFICO DE BARRAS ------------------------------------------

# ggplot2 visto en clase
message("Generando gráfico")
ggplot(dtm_df, aes(x = lemma, y = frecuencia_total, fill = lemma)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Frecuencia de términos relevantes en comunicados de la OEA",
    subtitle = "Enero - Abril 2026",
    x = "Término",
    y = "Frecuencia total",
    caption = "Fuente: Comunicados de prensa OEA") +
  theme_minimal(base_size = 13)

# GUARDAR GRÁFICO --------------------------------------------

# ggsave() guarda el gráfico en /output.
ggsave(
  filename = file.path(output_dir, "frecuencia_terminos.png"),
  width  = 8,
  height = 5)
message("Gráfico guardado en: ", file.path(output_dir, "frecuencia_terminos.png"))

