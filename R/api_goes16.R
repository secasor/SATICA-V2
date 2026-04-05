# ==============================================================================
# ESCANEO DINÁMICO VÍA NASA FIRMS (REEMPLAZO DE GEE GOES-16)
# SATICA V2.0 — GitHub Actions Edition
# ==============================================================================
# Propósito: Descargar telemetría térmica en tiempo casi real de los satélites
# VIIRS (Suomi-NPP, NOAA-20) y MODIS (Terra/Aqua) vía NASA FIRMS Area API.
# Cruza los focos detectados contra el shapefile maestro de caña (SOR_OK).
# Genera: data_master/GOES16_Alertas.csv (compatible con centinela_satelital.R)
# Sin dependencia de GEE, rgee ni Python. Solo HTTP + sf.
# ==============================================================================

library(sf)
library(dplyr)
library(httr)
library(readr)

sf_use_s2(FALSE)
options(warn = -1)

message("🛰️  Iniciando escaneo FIRMS (reemplazo GOES-16)...")

# --- Credenciales -------------------------------------------------------
# La llave se toma del secreto GitHub Actions (o del archivo si se corre local)
NASA_MAP_KEY <- Sys.getenv("NASA_FIRMS_KEY")
if (NASA_MAP_KEY == "") {
  # Fallback: llave incluida en el script (actualizar si expira)
  NASA_MAP_KEY <- "67796e55c1024f4beac823ff61ffa800"
  message("  ℹ️  Usando llave NASA FIRMS embebida (fallback local).")
}

# --- Área de interés — Valle del Cauca [W, S, E, N] ---------------------
bbox <- "-77.5,3.5,-75.5,4.5"
dias <- "1"  # Últimas 24 horas (máx resolución temporal disponible)

# --- URLs autenticadas NASA FIRMS Area API -----------------------------------
mk <- NASA_MAP_KEY
url_snpp  <- sprintf("https://firms.modaps.eosdis.nasa.gov/api/area/csv/%s/VIIRS_SNPP_NRT/%s/%s",  mk, bbox, dias)
url_noaa20 <- sprintf("https://firms.modaps.eosdis.nasa.gov/api/area/csv/%s/VIIRS_NOAA20_NRT/%s/%s", mk, bbox, dias)
url_modis <- sprintf("https://firms.modaps.eosdis.nasa.gov/api/area/csv/%s/MODIS_NRT/%s/%s",        mk, bbox, dias)

descargar_seguro <- function(url, fuente) {
  tryCatch({
    res <- GET(url, timeout(30))
    if (status_code(res) == 200) {
      txt <- content(res, "text", encoding = "UTF-8")
      # Verificar que no sea un mensaje de error de la API
      if (nchar(trimws(txt)) < 30) {
        message(sprintf("  ⚠️  %s: Respuesta vacía o muy corta.", fuente))
        return(NULL)
      }
      # I() fuerza a readr a tratar el string como datos, no como ruta de archivo
      df <- suppressMessages(read_csv(I(txt), show_col_types = FALSE))
      if (nrow(df) == 0) {
        message(sprintf("  ℹ️  %s: Sin focos en el área.", fuente))
        return(NULL)
      }
      message(sprintf("  ✅ %s: %d focos descargados.", fuente, nrow(df)))
      return(df)
    } else {
      message(sprintf("  ⚠️  %s: HTTP %d", fuente, status_code(res)))
      return(NULL)
    }
  }, error = function(e) {
    message(sprintf("  ⚠️  %s: Error — %s", fuente, e$message))
    return(NULL)
  })
}

message("📡 Consultando órbitas satelitales...")
fuegos_snpp   <- descargar_seguro(url_snpp,   "Suomi-NPP (VIIRS)")
fuegos_noaa20 <- descargar_seguro(url_noaa20, "NOAA-20  (VIIRS)")
fuegos_modis  <- descargar_seguro(url_modis,  "Terra/Aqua (MODIS)")

# --- Consolidación ---------------------------------------------------------
lista <- list(fuegos_snpp, fuegos_noaa20, fuegos_modis)
lista <- lapply(lista, function(df) {
  if (!is.null(df) && "confidence" %in% names(df)) {
    df$confidence <- as.character(df$confidence)
  }
  return(df)
})
fuegos_master <- bind_rows(lista[!sapply(lista, is.null)])

if (!dir.exists("data_master")) dir.create("data_master", recursive = TRUE)

alertas_vacio <- data.frame(
  cod_unico   = character(),
  hda_nombre  = character(),
  Mask_GOES   = numeric(),
  GOES_Fuego  = logical(),
  Estado_GOES = character(),
  stringsAsFactors = FALSE
)

if (nrow(fuegos_master) == 0) {
  message("✅ FIRMS: Sin anomalías térmicas reportadas en Valle del Cauca (últimas 24h).")
  write.csv(alertas_vacio, "data_master/GOES16_Alertas.csv", row.names = FALSE)
  return(invisible(NULL))
}

# --- Filtrado por confianza ------------------------------------------------
# VIIRS: "l"=baja, "n"=nominal, "h"=alta | MODIS: 0-100 (>=60 = confiable)
fuegos_conf <- fuegos_master %>%
  filter(
    (confidence %in% c("n", "h")) |           # VIIRS nominal/alta
    suppressWarnings(!is.na(as.numeric(confidence)) & as.numeric(confidence) >= 60)  # MODIS >=60%
  )

message(sprintf("🔍 Filtrando: %d de %d focos con confianza aceptable.",
                nrow(fuegos_conf), nrow(fuegos_master)))

if (nrow(fuegos_conf) == 0) {
  message("✅ FIRMS: Todos los focos son de baja confianza. Sin alerta.")
  write.csv(alertas_vacio, "data_master/GOES16_Alertas.csv", row.names = FALSE)
  return(invisible(NULL))
}

# --- Cruce espacial --------------------------------------------------------
message("🗺️  Cruzando focos contra geometría de cultivos (SOR_OK.shp)...")

cana_path <- tryCatch(
  normalizePath(list.files("capas", pattern = "SOR_OK\\.shp$",
                            full.names = TRUE, recursive = TRUE)[1]),
  error = function(e) NA
)

if (is.na(cana_path) || !file.exists(cana_path)) {
  message("  ⚠️  No se encontró SOR_OK.shp. Generando alerta sin cruce espacial.")
  # Generar alerta genérica sin cruce para no perder la detección
  alertas_gen <- fuegos_conf %>%
    transmute(
      cod_unico   = paste0("FUEGO_", row_number()),
      hda_nombre  = "Valle del Cauca (Sin cruce catastral)",
      Mask_GOES   = if ("bright_ti4" %in% names(.)) bright_ti4 else NA_real_,
      GOES_Fuego  = TRUE,
      Estado_GOES = "Fuego Confirmado (FIRMS)"
    )
  write.csv(alertas_gen, "data_master/GOES16_Alertas.csv", row.names = FALSE)
  message(sprintf("🚨 %d focos guardados (sin cruce catastral).", nrow(alertas_gen)))
  return(invisible(NULL))
}

fuegos_sf <- st_as_sf(fuegos_conf,
                       coords = c("longitude", "latitude"),
                       crs = 4326)

mapa_cana <- st_read(cana_path, quiet = TRUE) %>%
  st_make_valid() %>%
  janitor::clean_names() %>%
  st_transform(4326) %>%
  mutate(
    hda_raw  = toupper(stringr::str_replace_all(
      as.character(dplyr::coalesce(!!!dplyr::select(st_drop_geometry(.), dplyr::matches("^hda$|^cod$|^codigo$")))),
      "[^0-9A-Za-z_]", "")),
    ing_raw  = toupper(as.character(dplyr::coalesce(
      !!!dplyr::select(st_drop_geometry(.), dplyr::matches("^ing$|^ingenio$"))))),
    ing_clean = dplyr::case_when(
      grepl("INCAUCA",     ing_raw) ~ "CA",
      grepl("MAYAGUEZ",    ing_raw) ~ "MY",
      grepl("MARIA LUISA", ing_raw) ~ "ML",
      grepl("CASTILLA",    ing_raw) ~ "CC",
      grepl("PROVIDENCIA", ing_raw) ~ "PR",
      grepl("MANUELITA",   ing_raw) ~ "MN",
      grepl("PICHICHI",    ing_raw) ~ "PC",
      grepl("CABA",        ing_raw) ~ "CB",
      grepl("RIOPAILA",    ing_raw) ~ "RP",
      TRUE ~ ing_raw
    ),
    cod_unico  = paste(ing_clean,
                       stringr::str_pad(hda_raw, width = 6, side = "left", pad = "0"),
                       sep = "_"),
    hda_nombre = toupper(as.character(dplyr::coalesce(
      !!!dplyr::select(st_drop_geometry(.), dplyr::matches("^nombre_hda$|^nombre$")))))
  )

incendios <- suppressWarnings(
  st_join(fuegos_sf, mapa_cana, join = st_intersects, left = FALSE)
)

if (nrow(incendios) == 0) {
  message("✅ FIRMS: Focos detectados pero fuera de los polígonos catastrales. Sin alerta.")
  write.csv(alertas_vacio, "data_master/GOES16_Alertas.csv", row.names = FALSE)
  return(invisible(NULL))
}

# --- Construir CSV compatible con centinela_satelital.R --------------------
bright_col <- intersect(c("bright_ti4", "bright_t31", "brightness"), names(incendios))
brillo_vals <- if (length(bright_col) > 0) {
  as.numeric(st_drop_geometry(incendios)[[bright_col[1]]])
} else {
  rep(NA_real_, nrow(incendios))
}

alertas_final <- st_drop_geometry(incendios) %>%
  transmute(
    cod_unico   = cod_unico,
    hda_nombre  = hda_nombre,
    Mask_GOES   = brillo_vals,
    GOES_Fuego  = TRUE,
    Estado_GOES = "Fuego Confirmado (FIRMS NRT)"
  ) %>%
  distinct(cod_unico, .keep_all = TRUE)

write.csv(alertas_final, "data_master/GOES16_Alertas.csv", row.names = FALSE)

message(sprintf("🚨 ¡ALERTA! FIRMS confirmó focos en %d suertes de caña.", nrow(alertas_final)))
print(alertas_final)

message("✅ Escaneo FIRMS finalizado.")
return(invisible(NULL))
