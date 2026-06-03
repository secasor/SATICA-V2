# ==============================================================================
# RADAR SATELITAL NASA FIRMS (SATICA V3.0)
# ==============================================================================
# Propósito: Descargar telemetría térmica en vivo de los satélites Suomi-NPP 
# y NOAA-20 (Sensor VIIRS 375m) de las últimas 24 horas y cruzar por 
# intersección pura contra el mapa maestro de caña.
# ==============================================================================

library(sf)
library(dplyr)
library(httr)
library(readr)

sf_use_s2(FALSE)

message("🛰️ INICIANDO RASTREO SATELITAL NASA FIRMS (24 HORAS)...")

# MAP KEY de NASA FIRMS (requierida para el acceso autenticado)
# Regístrate en: https://firms.modaps.eosdis.nasa.gov/api/map_key/
NASA_MAP_KEY <- "67796e55c1024f4beac823ff61ffa800"

# BBOX Valle del Cauca: [Oeste, Sur, Este, Norte]
bbox <- "-77.5,3.5,-75.5,4.5"
dias <- "1"

# 1. URLs Autenticadas NASA FIRMS - Area API (24h sobre Valle del Cauca)
url_snpp  <- sprintf("https://firms.modaps.eosdis.nasa.gov/api/area/csv/%s/VIIRS_SNPP_NRT/%s/%s",  NASA_MAP_KEY, bbox, dias)
url_j1    <- sprintf("https://firms.modaps.eosdis.nasa.gov/api/area/csv/%s/VIIRS_NOAA20_NRT/%s/%s", NASA_MAP_KEY, bbox, dias)
url_modis <- sprintf("https://firms.modaps.eosdis.nasa.gov/api/area/csv/%s/MODIS_NRT/%s/%s",        NASA_MAP_KEY, bbox, dias)

descargar_csv_seguro <- function(url) {
  tryCatch({
    res <- GET(url)
    if (status_code(res) == 200) {
      return(read_csv(content(res, "text", encoding = "UTF-8"), show_col_types = FALSE))
    } else {
      warning(paste("NASA API Error en:", url))
      return(NULL)
    }
  }, error = function(e) {
    warning("No se pudo conectar al satélite. Revise su conexión a Internet.")
    return(NULL)
  })
}

message("📡 Descargando Órbita Suomi-NPP (VIIRS)...")
fuegos_snpp <- descargar_csv_seguro(url_snpp)
message("📡 Descargando Órbita NOAA-20 (VIIRS)...")
fuegos_j1 <- descargar_csv_seguro(url_j1)
message("📡 Descargando Órbita Terra/Aqua (MODIS)...")
fuegos_modis <- descargar_csv_seguro(url_modis)

# 2. Consolidación
lista_fuegos <- list(fuegos_snpp, fuegos_j1, fuegos_modis)
lista_fuegos <- lapply(lista_fuegos, function(df) {
  if (!is.null(df) && "confidence" %in% names(df)) {
    df$confidence <- as.character(df$confidence)
  }
  return(df)
})
fuegos_master <- bind_rows(lista_fuegos[!sapply(lista_fuegos, is.null)])

if (nrow(fuegos_master) == 0) {
  message("✅ No hay alertas térmicas reportadas por NASA en el Valle del Cauca hoy.")
  # Crear archivo vacío para no romper el dashboard
  if(!dir.exists("data_master")) dir.create("data_master")
  write.csv(data.frame(cod_unico = NA_character_, Mensaje = "Sin incendios en 24H"),
            "data_master/Incendios_Satelite_24H.csv", row.names = FALSE)
} else {
  # VIIRS define la confianza como: "l" (low), "n" (nominal), "h" (high)
  fuegos_alta_confianza <- fuegos_master %>%
    filter(confidence %in% c("n", "h"))
  
  message(sprintf("🔍 Escaneando %d anomalías térmicas confirmadas continentales...", nrow(fuegos_alta_confianza)))
  
  # 3. Conversión Espacial (EPSG 4326 WGS84 - Nativo del Satélite)
  fuegos_sf <- st_as_sf(fuegos_alta_confianza, coords = c("longitude", "latitude"), crs = 4326)
  
  # 4. Ingesta Mapa Maestro SATICA
  message("🗺️ Cruzando contra Geometría de Cultivos (Caña_SOR_OK)...")
  cana_path <- normalizePath(list.files("capas", pattern = "SOR_OK\\.shp$", full.names = TRUE)[1])
  if (is.na(cana_path)) stop("No se encontró el archivo de geometría Caña_SOR_OK.shp en /capas")
  mapa_cana <- st_read(cana_path, quiet = TRUE) %>% 
    st_make_valid() %>%
    janitor::clean_names() %>%
    st_transform(4326) # Sincronizar espectro de proyección con NASA
  
  # 5. Raycasting Intersección Exacta
  # left = FALSE asegura que solo se guardan los Fuegos que caen DENTRO de una suerte
  incendios_detectados <- st_join(fuegos_sf, mapa_cana, join = st_intersects, left = FALSE)
  
  # 6. Guardado del Reporte Definitivo
  if(!dir.exists("data_master")) dir.create("data_master")
  
  if (nrow(incendios_detectados) > 0) {
    reporte_final <- suppressWarnings(incendios_detectados %>% 
      st_drop_geometry() %>%
      select(cod_unico, hda_nombre, municipio = nom_munici,
             satelite = satellite, fecha_satelite = acq_date, 
             hora_satelite = acq_time, brillo_termico = bright_ti4) %>%
      arrange(desc(fecha_satelite), desc(hora_satelite)))
    
    write.csv(reporte_final, "data_master/Incendios_Satelite_24H.csv", row.names = FALSE)
    message(sprintf("🚨 ¡ALERTA! El satélite confirmó %d incendios activos sobre los cultivos.", nrow(reporte_final)))
    message("➡️ Reporte oficial guardado en: data_master/Incendios_Satelite_24H.csv")
    print(head(reporte_final))
  } else {
    message("✅ SATICA: El cielo está despejado. Cero (0) cruces satelitales térmicos sobre Caña hoy.")
    # Crear archivo vacío con estructura para no romper el dashboard
    write.csv(data.frame(cod_unico = NA_character_, Mensaje = "Sin incendios en 24H"), 
              "data_master/Incendios_Satelite_24H.csv", row.names = FALSE)
  }
}
