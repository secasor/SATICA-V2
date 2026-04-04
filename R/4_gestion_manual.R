# Archivo: R/4_gestion_manual.R
# Objetivo: Guardar y cargar haciendas manuales en CSV.

library(readr); library(dplyr); library(sf); library(stringr)

archivo_csv <- "haciendas_manuales.csv"

guardar_hacienda_manual <- function(ingenio, hacienda, suerte, muni, correg, lat, long) {
  
  # Normalización idéntica al sistema oficial
  h_norm <- str_pad(gsub("[^0-9]", "", as.character(hacienda)), 6, "left", "0")
  s_norm <- str_pad(gsub("[^0-9]", "", as.character(suerte)), 6, "left", "0")
  
  dicc <- c("MANUELITA"="MN", "INCAUCA"="IN", "PROVIDENCIA"="PR", "MAYAGUEZ"="MY", 
            "PICHICHI"="PI", "RISARALDA"="RI", "CARMELITA"="CM", "MARIA LUISA"="ML", 
            "SANCARLOS"="SC", "TUMACO"="TU", "OCCIDENTE"="OC", "LA CABAÑA"="CB")
  
  ing_upper <- toupper(str_trim(ingenio))
  abr <- ifelse(ing_upper %in% names(dicc), dicc[ing_upper], substr(ing_upper, 1, 2))
  
  # Generamos COD_UNICO compatible
  COD_GENERADO <- toupper(gsub("[^A-Z0-9]", "", paste0(abr, h_norm, s_norm)))
  
  nuevo <- data.frame(
    UID_GEO = paste0("MAN_", as.integer(Sys.time())),
    COD_UNICO = COD_GENERADO,
    INGENIO = ingenio,
    HACIENDA = hacienda,
    SUERTE = suerte,
    AREA_HA = 0, 
    NOMBRE_MUNICIPIO = muni,
    NOMBRE_CORREGIMIENTO = correg,
    LAT = as.numeric(lat),
    LONG = as.numeric(long),
    stringsAsFactors = FALSE
  )
  
  if (file.exists(archivo_csv)) write_csv(nuevo, archivo_csv, append = TRUE)
  else write_csv(nuevo, archivo_csv)
}

cargar_manuales_sf <- function() {
  if (!file.exists(archivo_csv)) return(NULL)
  datos <- read_csv(archivo_csv, show_col_types = FALSE)
  if (nrow(datos) == 0) return(NULL)
  
  st_as_sf(datos, coords = c("LONG", "LAT"), crs = 4326, remove = FALSE) %>%
    select(UID_GEO, COD_UNICO, INGENIO, HACIENDA, SUERTE, AREA_HA, NOMBRE_MUNICIPIO, NOMBRE_CORREGIMIENTO) %>%
    mutate(ORIGEN = "MANUAL")
}