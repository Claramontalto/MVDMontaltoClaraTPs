# ===========
# Objetivo: scrapear los comunicados de prensa de la OEA para los meses de Enero, Febrero, Marzo y Abril de 2026
# ===========

# LIBRERÍAS -----------------------------------------------
# tidyverse: para manipular datos 
# rvest: para web scraping 
# here: para rutas relativas 
# xml2: para guardar el HTML
library(tidyverse)
library(rvest)
library(here)
library(xml2)

# CARPETA DATA --------------------------------------------

# here() busca la raíz del proyecto (.Rproj) y construye la ruta desde ahí. 
# dir.exists() chequea si la carpeta ya existe.
# dir.create() la crea si no existe. 
# message() imprime mensajes informativos. 
data_dir <- here("TP2/data")

if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
  message("Carpeta creada: ", data_dir)} else {
  message("La carpeta data ya existe: ", data_dir)}

#URLs A SCRAPEAR -----------------------------------------

# El URL de la OEA cambia según el mes y el año:
# https://www.oas.org/es/centro_noticias/comunicados_prensa.asp?nMes=1&nAnio=2026
# paste0() une strings. 
meses   <- 1:4
anio    <- 2026
base_url <- "https://www.oas.org/es/centro_noticias/comunicados_prensa.asp"

#FUNCIÓN PARA OBTENER TÍTULOS Y LINKS DE UN MES --------------

# Definimos una función que dado un mes y año:
# 1. Descarga la página de ese mes
# 2. Guarda el HTML con fecha (pedido por el TP)
# 3. Extrae los títulos y links de los comunicados
# El selector .itemmenulink fue identificado con SelectorGadget
obtener_links_mes <- function(mes, anio) {
  message("Obteniendo links del mes: ", mes, "/", anio)
  # Construimos el URL del mes. paste0() visto en todas las clases
  url <- paste0(base_url, "?nMes=", mes, "&nAnio=", anio)
  # Descargamos la página. read_html() de rvest
  pagina <- read_html(url)
  # Guardamos el HTML con la fecha de descarga en el nombre
  # Sys.Date() devuelve la fecha de hoy. xml2::write_html() guarda el HTML
  nombre_html <- paste0("comunicados_", anio, "_mes", mes, "_", Sys.Date(), ".html")
  xml2::write_html(pagina, file.path(data_dir, nombre_html))
  message("HTML guardado: ", nombre_html)
  # Extraemos títulos con el selector identificado con SelectorGadget
  # html_nodes() y html_text() de rvest
  titulos <- pagina |> html_nodes(".itemmenulink") |> html_text(trim = TRUE)
  # Extraemos los links (atributo href de cada título)
  # html_attr() de rvest
  links <- pagina |> html_nodes(".itemmenulink") |> html_attr("href")
  # Armamos una tabla con los resultados
  # tibble() 
  tibble(
    mes    = mes,
    titulo = titulos,
    link   = paste0("https://www.oas.org/es/centro_noticias/", links))}

#FUNCIÓN PARA OBTENER CUERPO DE UN COMUNICADO -----------------

# Esta función recibe el link de un comunicado y devuelve el cuerpo
# El selector #rightmaincol p fue verificado con SelectorGadget
obtener_cuerpo <- function(link) {
  message("Obteniendo cuerpo de: ", link)
  # Esperamos 3 segundos entre requests por el Crawl-delay
  # del robots.txt de la OEA (Crawl-delay: 3)
  Sys.sleep(3)
  # Descargamos la página del comunicado
  pagina <- read_html(link)
  # Extraemos los párrafos del cuerpo con el selector
  # #rightmaincol p verificado con SelectorGadget
  cuerpo <- pagina |> 
    html_nodes("#rightmaincol p") |> 
    html_text(trim = TRUE) |> 
    paste(collapse = " ")  # unimos todos los párrafos en un solo texto
  return(cuerpo)}

#EJECUTAR EL SCRAPING -------------------------------------
# map_dfr() aplica la función a cada mes y une los resultados en una sola tabla.
message("Iniciando scraping de todos los meses")
tabla_links <- map_dfr(meses, ~ obtener_links_mes(.x, anio))
message("Links obtenidos. Total de comunicados: ", nrow(tabla_links))
# Agregamos el cuerpo de cada comunicado usando un loop for
# Creamos primero un vector vacío para guardar los cuerpos
cuerpos <- c()
for (link in tabla_links$link) {
  cuerpo <- obtener_cuerpo(link)
  cuerpos <- c(cuerpos, cuerpo)}
# Armamos la tabla final con id, titulo y cuerpo
tabla_completa <- tabla_links |>
  mutate(id = row_number(),
    cuerpo = cuerpos) |>
  select(id, titulo, cuerpo)
message("Cuerpos obtenidos")

# GUARDAR RESULTADO ----------------------------------------
# saveRDS() guarda el objeto en formato .rds. 
saveRDS(tabla_completa, file.path(data_dir, "comunicados_oea.rds"))
message("Tabla guardada en: ", file.path(data_dir, "comunicados_oea.rds"))
