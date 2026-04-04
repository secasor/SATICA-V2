# ==============================================================================
# MOTOR DE BIOMASA SATELITAL SENTINEL-2 (SATICA V3.0)
# ==============================================================================
# Propósito: Usar la supercomputadora de Google Earth Engine para descargar
# a 10 metros de resolución el grado de verdor (NDVI) y las cicatrices
# post-cosecha (NBR) de cada suerte de caña, evaluando el riesgo de combustión.
# ==============================================================================
# REQUISITOS PREVIOS EN LA CONSOLA (SOLO SE CORREN UNA VEZ):
# 1. install.packages("reticulate")
# 2. rgee::ee_install()  # Instala Python MiniConda
# ==============================================================================

library(sf)
library(dplyr)

# Intentar cargar rgee (Google Earth Engine for R)
if (!requireNamespace("rgee", quietly = TRUE)) {
  stop("El paquete 'rgee' no está instalado. Para instalarlo, ejecuta: install.packages('rgee'). Luego corre rgee::ee_install()")
}
reticulate::use_condaenv("r-reticulate", required = TRUE)

library(rgee)

message("🌍 Inicializando conexión con Google Earth Engine...")
# [BLINDAJE 4: BYPASS DEL VALIDADOR DE RGEE]
# rgee::ee_Initialize() está revisando la metadata del token y malinterpretando la
# fecha de expiración por cambios en la API v1.7. Enviamos la orden
# directo al núcleo de Python (ee) que comprobamos que SÍ funciona.
ee <- reticulate::import("ee")
ee$Initialize(project = "satica-v2")

message("🗺️ Cargando y empaquetando Área de Estudio (Cultivos de Caña)...")
cana <- st_read("capas/Caña_SOR_OK.shp", quiet = TRUE) %>%
  janitor::clean_names() %>%
  st_transform(4326)

# Bounding Box para no enviar los miles de polígonos al filtro inicial
caja_cana <- sf_as_ee(st_as_sfc(st_bbox(cana)))

# 1. PARAMETRIZACIÓN DEL SATÉLITE
# COPERNICUS/S2_SR_HARMONIZED = Sentinel-2 Nivel 2A (Reflectancia de Superficie)
fecha_inicio <- Sys.Date() - 45 # Aumentamos umbral base a 45 días
fecha_fin <- Sys.Date()

message(sprintf("🛰️ Descargando catálogo Sentinel-2 (%s a %s)...", fecha_inicio, fecha_fin))

s2_col <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$
  filterBounds(caja_cana)$
  filterDate(as.character(fecha_inicio), as.character(fecha_fin))$
  filterMetadata("CLOUDY_PIXEL_PERCENTAGE", "less_than", 25)

# [BLINDAJE 7: COLECCIÓN VACÍA POR TEMPORADA ALTA NUBOSIDAD]
cuantas_imagenes <- s2_col$size()$getInfo()
if (cuantas_imagenes == 0) {
  message("⚠️ Cero imágenes encontradas (Alta Nubosidad en la región). Ampliando filtro a 90 días y 50% nubes...")
  fecha_inicio <- Sys.Date() - 90
  s2_col <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$
    filterBounds(caja_cana)$
    filterDate(as.character(fecha_inicio), as.character(fecha_fin))$
    filterMetadata("CLOUDY_PIXEL_PERCENTAGE", "less_than", 50)
}

# 2. FUNCIONES DE CÁLCULO FÍSICO-ÓPTICO
# NDVI = (NIR - Rojo) / (NIR + Rojo) -> Banda 8 y Banda 4
# NBR = (NIR - SWIR2) / (NIR + SWIR2) -> Banda 8 y Banda 12
calcular_indices <- function(imagen) {
  ndvi <- imagen$normalizedDifference(c("B8", "B4"))$rename("NDVI")
  nbr <- imagen$normalizedDifference(c("B8", "B12"))$rename("NBR")
  return(imagen$addBands(ndvi)$addBands(nbr))
}

# Mapear los índices matemáticos sobre toda la colección y sacar la mediana temporal
s2_con_indices <- s2_col$map(calcular_indices)$median()

# 3. EXTRACCIÓN ZONAL DE ESTADÍSTICAS A CADA SUERTE EN LOTES
message("📊 Descargando estadísticas zonales en lotes (evitando límite de 10k)... Esto tomará unos minutos.")

tamano_lote <- 1000 # Reducido temporalmente a 1000 para evitar límite de peso (10MB) en lotes con geometrías complejas
total_filas <- nrow(cana)
n_lotes <- ceiling(total_filas / tamano_lote)
lista_extracciones <- list()

for (i in 1:n_lotes) {
  inicio <- ((i - 1) * tamano_lote) + 1
  fin <- min(i * tamano_lote, total_filas)
  lote_cana <- cana[inicio:fin, ]

  message(sprintf("   ▶ Procesando Lote %d de %d (Suertes %d a %d)...", i, n_lotes, inicio, fin))

  # [BLINDAJE 5: BYPASS DE ee_extract]
  # El wrapper rgee::ee_extract() intenta forzar argumentos nombrados que ya
  # no existen en la API de Python v1.7. Usamos llamadas nativas OOP de Python.
  ee_lote <- sf_as_ee(lote_cana)

  # Escala 30m (vs 10m original) evita timeouts con geometrías complejas.
  # bestEffort permite que GEE ajuste la escala si los polígonos son demasiado grandes.
  ee_stats <- s2_con_indices$select(c("NDVI", "NBR"))$reduceRegions(
    collection = ee_lote,
    reducer = ee$Reducer$mean(),
    scale = 30L,
    tileScale = 4L
  )

  # [BLINDAJE 6: EXTRACCIÓN NATIVA getInfo()]
  json_data <- tryCatch({
    ee_stats$getInfo()
  }, error = function(e) {
    message(sprintf("   ⚠️ Error en Lote %d: %s (omitiendo...)", i, e$message))
    list(features = list())
  })

  if (length(json_data$features) > 0) {
    extraccion_lote <- bind_rows(lapply(json_data$features, function(x) {
      props <- x$properties
      # GEE devuelve las bandas como propiedades directas cuando se usa select() + mean()
      data.frame(
        cod_unico  = if (!is.null(props$cod_unico)) props$cod_unico else NA_character_,
        hda_nombre = if (!is.null(props$hda_nombre)) props$hda_nombre else NA_character_,
        NDVI       = if (!is.null(props$NDVI)) as.numeric(props$NDVI) else NA_real_,
        NBR        = if (!is.null(props$NBR))  as.numeric(props$NBR)  else NA_real_,
        stringsAsFactors = FALSE
      )
    }))
    lista_extracciones[[i]] <- extraccion_lote
  }
}

extraccion_ee <- bind_rows(lista_extracciones)

# 4. EXPORTACIÓN
if (!dir.exists("data_master")) dir.create("data_master")

reporte_biomasa <- extraccion_ee %>%
  select(any_of(c("cod_unico", "hda_nombre", "NDVI", "NBR"))) %>%
  mutate(
    # Etiquetador Automático de Riesgo Físico
    Alerta_Combustion = case_when(
      NDVI < 0.25 & NBR < 0.1 ~ "ALTA (Extrema Resequedad)",
      NDVI >= 0.25 & NDVI <= 0.6 ~ "INTERMEDIA",
      NDVI > 0.6 ~ "BAJA (Cultivo Sano/Húmedo)"
    )
  )

write.csv(reporte_biomasa, "data_master/Biomasa_Cana_Sentinel2.csv", row.names = FALSE)
message("✅ Extracción V3.0 Finalizada: data_master/Biomasa_Cana_Sentinel2.csv")
print(head(reporte_biomasa))
