# ==============================================================================
# ESCANEO DINÁMICO DE HUMO SATÉLITE GOES-16 (SATICA V3.0)
# ==============================================================================
# Propósito: Utilizar la colección GOES-16 MCMIPF (Banda 7 InfraRojo Cercano) 
# en Google Earth Engine para escanear en tiempo casi real (cada 15 min) 
# anomalías termales dinámicas sobre los ingenios.
# ==============================================================================

library(sf)
library(dplyr)

# Intentar cargar rgee (Google Earth Engine for R)
if (!requireNamespace("rgee", quietly = TRUE)) stop("El paquete 'rgee' no está instalado. Ejecute rgee::ee_install()")
reticulate::use_condaenv("r-reticulate", required = TRUE)

library(rgee)

message("🌍 Inicializando conexión con Google Earth Engine para GOES-16...")
# Bypass del validador de expiración (igual que en Sentinel-2)
ee <- reticulate::import("ee")
ee$Initialize(project = "satica-v2")

message("🧭 Cargando zona de escrutinio para GOES-16...")
cana_path <- normalizePath(list.files("capas", pattern = "SOR_OK\\.shp$", full.names = TRUE)[1])
if (is.na(cana_path)) stop("No capa de caña disponible.")

cana <- st_read(cana_path, quiet = TRUE) %>%
  janitor::clean_names() %>%
  st_transform(4326) %>%
  mutate(
    hda_raw = toupper(stringr::str_replace_all(as.character(dplyr::coalesce(!!!dplyr::select(st_drop_geometry(.), dplyr::matches("^hda$|^cod$|^codigo$")))), "[^0-9A-Za-z_]", "")),
    ing_raw = toupper(as.character(dplyr::coalesce(!!!dplyr::select(st_drop_geometry(.), dplyr::matches("^ing$|^ingenio$"))))),
    ing_clean = dplyr::case_when(
      grepl("INCAUCA", ing_raw) ~ "CA",
      grepl("MAYAGUEZ", ing_raw) ~ "MY",
      grepl("MARIA LUISA", ing_raw) ~ "ML",
      grepl("CASTILLA", ing_raw) ~ "CC",
      grepl("PROVIDENCIA", ing_raw) ~ "PR",
      grepl("MANUELITA", ing_raw) ~ "MN",
      grepl("PICHICHI", ing_raw) ~ "PC",
      grepl("CABA", ing_raw) ~ "CB",
      grepl("RIOPAILA", ing_raw) ~ "RP",
      TRUE ~ ing_raw
    ),
    cod_unico = paste(ing_clean, stringr::str_pad(hda_raw, width = 6, side = "left", pad = "0"), sep = "_"),
    hda_nombre = toupper(as.character(dplyr::coalesce(!!!dplyr::select(st_drop_geometry(.), dplyr::matches("^nombre_hda$|^nombre$")))))
  )


caja_cana <- sf_as_ee(st_as_sfc(st_bbox(cana)))

# 1. PARAMETRIZACIÓN GOES-16
# COPERNICUS es Sentinel. Para GOES-16: NOAA/GOES/16/MCMIPF
# Analizamos los últimos 60 minutos (resolución temporal altísima)
ahora <- Sys.time()
hora_inicio <- ahora - (60 * 60) # 1 hora atrás
hora_fin    <- ahora

fecha_str_ini <- format(hora_inicio, "%Y-%m-%dT%H:%M:%S", tz = "UTC")
fecha_str_fin <- format(hora_fin, "%Y-%m-%dT%H:%M:%S", tz = "UTC")

message(sprintf("🛰️ Sondando Órbita GOES-16 (Última hora UTC: %s a %s)...", fecha_str_ini, fecha_str_fin))

# NOAA/GOES/16/FDCF (Fire Detection and Characterization)
# El producto FDC (Fire Detection) es mejor para fuego que MCMIPF crudo
goes_col <- ee$ImageCollection("NOAA/GOES/16/FDCF")$
  filterBounds(caja_cana)$
  filterDate(fecha_str_ini, fecha_str_fin)

cuantas_imagenes <- goes_col$size()$getInfo()
if (cuantas_imagenes == 0) {
  message("✅ GOES-16: No hay paso satelital en la última hora. Abortando rastreo secundario.")
  if (!dir.exists("data_master")) dir.create("data_master")
  write.csv(data.frame(cod_unico = character(), GOES_Fuego = logical(), Estado_GOES = character()), 
            "data_master/GOES16_Alertas.csv", row.names = FALSE)
  return(invisible(NULL))
}

# La colección FDC tiene la banda "Mask"
# Valores típicos de Mask: 10,11,12,13,14,15,30,31,32,33,34,35 (Fire detected)
# Reducimos a Máximo temporal de la última hora
goes_fuego_max <- goes_col$select("Mask")$max()

# Extraemos valores sobre los polígonos
message("🔥 Cruzando anomalías termomecánicas dinámicas contra lotes...")
ee_cana <- sf_as_ee(cana)

estadisticas_goes <- goes_fuego_max$reduceRegions(
  collection = ee_cana,
  reducer    = ee$Reducer$max(), # Buscamos si ALGÚN pixel toca la suerte
  scale      = 2000              # Resolución espacial nativa de GOES (~2km)
)

json_goes <- tryCatch({
  estadisticas_goes$getInfo()
}, error = function(e) {
  message("   ⚠️ Error en extracción zonal GEE GOES: ", e$message)
  list(features = list())
})

# Extraer y filtrar
if (length(json_goes$features) > 0) {
  extraccion_alertas <- bind_rows(lapply(json_goes$features, function(x) {
    props <- x$properties
    
    # max = Valor de la máscara FDC
    val_max <- if (!is.null(props$max)) as.numeric(props$max) else NA_real_
    
    # 10=Processed, 11=Saturated, 12=Cloud, 13=HighProb, 14=MediumProb, 15=LowProb
    # Nos centramos en códigos 10 al 35 como "Posible Fuego" según la tabla NOAA
    alarma <- !is.na(val_max) && (val_max >= 10 && val_max <= 35)
    
    data.frame(
      cod_unico   = if (!is.null(props$cod_unico)) props$cod_unico else NA_character_,
      hda_nombre  = if (!is.null(props$hda_nombre)) props$hda_nombre else NA_character_,
      Mask_GOES   = val_max,
      GOES_Fuego  = alarma,
      Estado_GOES = ifelse(alarma, "Fuego Confirmado (Dinámico)", "Normal"),
      stringsAsFactors = FALSE
    )
  }))
  
  # Filtramos solo las suertes que activaron GOES para el CSV final ligero
  solo_alertas <- extraccion_alertas %>% filter(GOES_Fuego == TRUE)
  
  if (!dir.exists("data_master")) dir.create("data_master")
  write.csv(solo_alertas, "data_master/GOES16_Alertas.csv", row.names = FALSE)
  
  if (nrow(solo_alertas) > 0) {
    message(sprintf("🚨 ¡ALERTA MAYOR! GOES-16 detectó focos dinámicos en %d suertes.", nrow(solo_alertas)))
    print(head(solo_alertas))
  } else {
    message("✅ SATICA GOES-16: Cielo limpio (FDC Mask OK).")
  }
} else {
  message("✅ Error o lista vacía desde GEE.")
  write.csv(data.frame(cod_unico = character(), GOES_Fuego = logical(), Estado_GOES = character()), 
            "data_master/GOES16_Alertas.csv", row.names = FALSE)
}

message("✅ Monitoreo de Alta Frecuencia (GOES-16) Finalizado.")
return(invisible(NULL))
