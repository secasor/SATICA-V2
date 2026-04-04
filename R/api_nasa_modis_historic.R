# ==============================================================================
# RASTREADOR HISTÓRICO NASA FIRMS MODIS/VIIRS (SATICA V3.0)
# ==============================================================================
# Propósito: Descargar el banco de datos históricos de anomalías térmicas
# para entrenar modelos XGBoost futuros con el récord de fuegos reales del satélite.
# ==============================================================================
# REQUISITO: Se necesita una API Key gratuita de la NASA.
# Consíguela en: https://firms.modaps.eosdis.nasa.gov/api/map_key/
# ==============================================================================

library(httr)
library(readr)
library(dplyr)

message("🛰️ INICIANDO DESCARGA HISTÓRICA NASA FIRMS (VALLE DEL CAUCA)...")

# ==============================================================================
# ⚠️ INGRESA TU NASA MAP KEY AQUÍ (Generada en la página de FIRMS) ⚠️
# ==============================================================================
NASA_MAP_KEY <- "67796e55c1024f4beac823ff61ffa800" # MAP KEY oficial CVC/SATICA

# Parámetros de la API Histórica (Area Request)
sensor <- "MODIS_NRT" # Nombre correcto del producto en la API de área de NASA FIRMS
# Bounding Box para el Valle del Cauca (Colombia): [Oeste, Sur, Este, Norte]
bbox <- "-78.5,-3.0,-73.0,5.5"
ventana_dias <- "5" # Días retrospectivos (API gratuita permite hasta 5 por Request)
fecha_limite <- Sys.Date()

# Construcción Mágica de la URL
url_historica <- sprintf(
  "https://firms.modaps.eosdis.nasa.gov/api/area/csv/%s/%s/%s/%s/%s",
  NASA_MAP_KEY, sensor, bbox, ventana_dias, fecha_limite
)

message(sprintf("🔗 Contactando base de datos NASA: %s a %s...", Sys.Date() - 5, Sys.Date()))

# 1. Petición HTTP al Servidor
res <- GET(url_historica)

if (status_code(res) == 200) {
  historial_csv <- content(res, "text", encoding = "UTF-8")
  # I() fuerza a readr a interpretar el string como datos CSV literales,
  # no como una ruta de archivo (bug en readr >= 2.0 con respuestas de 0 filas)
  df_fuegos_historicos <- read_csv(I(historial_csv), show_col_types = FALSE)

  if (nrow(df_fuegos_historicos) > 0) {
    if (!dir.exists("data_master")) dir.create("data_master")

    # 2. Exportación a Archivo
    # MODIS usa confianza numérica (0-100), no letras como VIIRS
    df_clean <- df_fuegos_historicos %>% filter(as.numeric(confidence) >= 80)

    archivo_salida <- sprintf("data_master/NASA_Historico_%s_a_%s.csv", Sys.Date() - 5, Sys.Date())
    write.csv(df_clean, archivo_salida, row.names = FALSE)

    message(sprintf("✅ ÉXITO: %d focos de calor históricos guardados.", nrow(df_clean)))
    message(sprintf("📁 Archivo: %s", archivo_salida))
  } else {
    message("✅ SATICA: No hubo incendios reportados en esta ventana de 5 días.")
  }
} else {
  message(sprintf("❌ FIRMS API ERROR: Código %s. Verifica tu MAP_KEY y límites de velocidad de descargas.", status_code(res)))
}
