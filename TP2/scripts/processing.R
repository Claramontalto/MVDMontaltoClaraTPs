# ------------------------------------------------------------
# ------------------------------------------------------------
# Voy a limpiar, lematizar y remover stopwords del texto de los 
# comunicados de prensa de la OEA
# ------------------------------------------------------------
# ------------------------------------------------------------

# LIBRERÍAS --------------------------------------------------
# tidyverse: para manipular datos
# udpipe: para lematizar en español 
# stopwords: para remover stopwords 
# here: para rutas relativas 
library(tidyverse)
library(udpipe)
library(stopwords)
library(here)

#RUTAS --------------------------------------------------------

data_dir   <- here("TP2/data")
output_dir <- here("TP2/output")

#CREAE CARPETA OUTPUT SI NO EXISTE ----------------------------

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  message("Carpeta creada: ", output_dir)} else {
  message("La carpeta output ya existe: ", output_dir)}

#CARGADO DE DATOS ----------------------------------------------

# readRDS() carga el archivo .rds generado por scraping_oea.R
message("Cargando datos")
comunicados <- readRDS(file.path(data_dir, "comunicados_oea.rds"))
message("Datos cargados. Total de comunicados: ", nrow(comunicados))

#LIMPIANDO TETXO -----------------------------------------------

#Usamos stringr (tidyverse) para limpiar el tetxo (str_to_lower(), str_remove_all(), str_squish())
message("limpiando texto")
comunicados_limpios <- comunicados |>
  mutate(
    cuerpo = cuerpo |>
      str_to_lower() |> # minúsculas
      str_replace_all("[\r\n\t]+" , " ") |> # eliminar saltos de línea
      str_replace_all("[\"'\u201c\u201d\u2018\u2019«»`´%()]", "") |>  # eliminar símbolos
      str_remove_all("[[:punct:]]") |> # eliminar puntuación
      str_remove_all("\\b\\d+(\\.\\d+)?%?\\b") |>  # eliminar números
      str_squish())  # eliminar espacios múltiples
message("Limpieza completada")

#LEMATIZACION --------------------------------------------------

# udpipe permite lematizar en español usando análisis morfológico
message("Descargando modelo de español para lematización")
m_es <- udpipe_download_model(language = "spanish", overwrite = FALSE)
modelo_es <- udpipe_load_model(m_es$file_model)
message("Modelo cargado")
# udpipe_annotate() lematiza el texto completo
message("Lematizando texto")
comunicados_lemas <- udpipe_annotate(
  modelo_es,
  x = comunicados_limpios$cuerpo,
  doc_id = comunicados_limpios$id) |>
  as.data.frame() |>
  mutate(id = as.integer(doc_id)) |>
  select(id, lemma, upos)
message("Lematización completada")

# FILTRAR SUSTANTIVOS, VERBOS Y ADJETIVOS ----------------------

# upos es la categoría gramatical de cada palabra
# NOUN = sustantivo, VERB = verbo, ADJ = adjetivo
message("Filtrando sustantivos, verbos y adjetivos")
comunicados_filtrados <- comunicados_lemas |>
  filter(upos %in% c("NOUN", "VERB", "ADJ")) |>
  mutate(lemma = str_to_lower(lemma))
message("Filtrado completado")

# REMOVER STOPWORDS -------------------------------------------- 

# stopwords::stopwords() devuelve lista de palabras vacías en español
message("Removiendo stopwords")
stop_es <- stopwords::stopwords("es")
stop_en <- stopwords::stopwords("en")
stop_words <- tibble(lemma = c(stop_es, stop_en))
comunicados_procesados <- comunicados_filtrados |>
  anti_join(stop_words, by = "lemma") |># elimina las stopwords
  filter(str_length(lemma) > 2) # elimina palabras muy cortas
message("Stopwords removidas")

# GUARDAR RESULTADO ---------------------------------------------  

saveRDS(comunicados_procesados, file.path(output_dir, "processed_text.rds"))
message("Archivo guardado en: ", file.path(output_dir, "processed_text.rds"))



