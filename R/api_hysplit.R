# ==============================================================================
# MODELACIÓN PROSPECTIVA DE PLUMAS DE HUMO — MODO DUAL (SATICA V2.4)
# ==============================================================================
# MODO A (Preferido): splitr + HYSPLIT binario local (si está disponible)
# MODO B (Fallback):  Vientos reales de Open-Meteo → trayectoria vectorial simple
# Resultado: data_master/HYSPLIT_plumas.rds (sf LINESTRING o NULL)
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(lubridate)
  library(purrr)
})

message("🌬️ INICIANDO MODELADOR PREDICTIVO DE HUMO — MODO DUAL (6 HORAS)...")

# ─── 1. BASE MAESTRA ─────────────────────────────────────────────────────────
master_rds_path <- "data_master/SATICA_MASTER_v2.2.rds"

if (!file.exists(master_rds_path)) {
  message("❌ ERROR: No se encontró SATICA_MASTER_v2.2.rds. Abortando modelo.")
  saveRDS(NULL, "data_master/HYSPLIT_plumas.rds")
  return(invisible(NULL))
}

master_db <- tryCatch(readRDS(master_rds_path), error = function(e) NULL)
if (is.null(master_db)) {
  saveRDS(NULL, "data_master/HYSPLIT_plumas.rds")
  return(invisible(NULL))
}

# Si el RDS tiene geometría sf, quitarla
if (inherits(master_db, "sf")) master_db <- sf::st_drop_geometry(master_db)

# Top 5 haciendas CRÍTICAS con coordenadas válidas
top5 <- master_db %>%
  filter(riesgo == "CRITICO", !is.na(lat), !is.na(lon)) %>%
  distinct(cod_hda_key, .keep_all = TRUE) %>%
  arrange(DIFF_MESES) %>%
  head(5)

if (nrow(top5) == 0) {
  message("✅ Sin haciendas CRÍTICAS con coordenadas. No se requieren trayectorias.")
  saveRDS(NULL, "data_master/HYSPLIT_plumas.rds")
  return(invisible(NULL))
}

message(sprintf("🌪️ Preparando trayectorias para %d haciendas críticas...", nrow(top5)))

# ─── 2. FUNCIÓN FALLBACK: VIENTO VECTORIAL (Open-Meteo, sin clave API) ───────
# Descarga viento u10/v10 del punto de la hacienda y proyecta
# una trayectoria lineal simple de 6 pasos horarios.

generar_trayectoria_fallback <- function(lat0, lon0, cod_hda_key, hda_nombre) {
  
  # Obtener viento prom hoy desde Open-Meteo (gratis, sin key)
  url <- sprintf(
    paste0("https://api.open-meteo.com/v1/forecast?",
           "latitude=%.4f&longitude=%.4f",
           "&hourly=wind_speed_10m,wind_direction_10m",
           "&forecast_days=1&timezone=auto"),
    lat0, lon0
  )
  
  # Valores de viento por defecto (viento alisio Valle del Cauca ~NE→SW 3 m/s)
  u_mean <- -2.1  # componente Este (negativo = hacia W)
  v_mean <-  1.4  # componente Norte (positivo = hacia N)
  
  tryCatch({
    if (requireNamespace("httr", quietly = TRUE)) {
      resp <- httr::GET(url, httr::timeout(8))
      if (httr::status_code(resp) == 200) {
        datos <- httr::content(resp, as = "parsed", type = "application/json")
        vel   <- as.numeric(datos$hourly$wind_speed_10m[1:6])
        dir   <- as.numeric(datos$hourly$wind_direction_10m[1:6])
        vel[is.na(vel)] <- 3.0; dir[is.na(dir)] <- 225
        # Convertir dirección meteorológica (desde dónde viene) a vectores
        # dirección 225° = desde SW → sopla hacia NE
        rad   <- (270 - dir) * pi / 180
        u_mean <- mean(vel * cos(rad), na.rm = TRUE)
        v_mean <- mean(vel * sin(rad), na.rm = TRUE)
      }
    }
  }, error = function(e) {
    message(sprintf("   ℹ️  Sin datos Open-Meteo para %s. Usando viento climatológico.", hda_nombre))
  })
  
  # Proyección: 1 grado lat ≈ 111 km, 1 grado lon ≈ 111*cos(lat) km
  # Velocidad en m/s * 3600 s/h / 111000 m/° = delta en grados por hora
  km_por_grado_lat <- 111.0
  km_por_grado_lon <- 111.0 * cos(lat0 * pi / 180)
  
  horas <- 1:6
  delta_lat <- (v_mean * 3600 / 1000) / km_por_grado_lat
  delta_lon <- (u_mean * 3600 / 1000) / km_por_grado_lon
  
  pts <- data.frame(
    lon       = lon0 + cumsum(rep(delta_lon, 6)),
    lat       = lat0 + cumsum(rep(delta_lat, 6)),
    hour_along = horas,
    cod_hda_key = cod_hda_key,
    hda_nombre  = hda_nombre
  )
  return(pts)
}

# ─── 3. MODO A: HYSPLIT REAL (si splitr + binario disponibles) ────────────────
usar_hysplit_real <- FALSE

if (requireNamespace("splitr", quietly = TRUE)) {
  # Verificar existencia del ejecutable HYSPLIT
  exec_posibles <- c(
    "hysplit_exec/hyts_std",
    "hysplit_exec/hyts_std.exe",
    Sys.getenv("HYSPLIT_EXEC")
  )
  exec_existe <- any(file.exists(exec_posibles))
  
  if (exec_existe) {
    usar_hysplit_real <- TRUE
    message("   ✅ Binario HYSPLIT detectado. Usando simulación real (Modo A).")
    # Crear directorios necesarios
    if (!dir.exists("met_data"))    dir.create("met_data",    recursive = TRUE)
    if (!dir.exists("hysplit_exec")) dir.create("hysplit_exec", recursive = TRUE)
  } else {
    message("   ℹ️  Binario HYSPLIT no encontrado. Activando Modo B (Vectorial Open-Meteo).")
  }
} else {
  message("   ℹ️  Paquete 'splitr' no instalado. Activando Modo B (Vectorial Open-Meteo).")
}

# ─── 4. EJECUTAR SIMULACIÓN ───────────────────────────────────────────────────
hora_sim <- round_date(Sys.time(), unit = "day") + hours(12)

lista_trayectorias <- map(1:nrow(top5), function(i) {
  predio <- top5[i, ]
  nom    <- coalesce(predio$hda_nombre, predio$cod_hda_key)
  message(sprintf("   ▶ Simulando dispersión: %s", nom))
  
  if (usar_hysplit_real) {
    # ── MODO A: splitr ──
    tryCatch({
      tray <- splitr::hysplit_trajectory(
        lat         = predio$lat,
        lon         = predio$lon,
        height      = 50,
        duration    = 6,
        days        = as.character(as.Date(hora_sim)),
        daily_hours = hour(hora_sim),
        direction   = "forward",
        met_type    = "gdas1",
        extended_met = TRUE,
        met_dir     = "met_data",
        exec_dir    = "hysplit_exec"
      )
      tray %>% as.data.frame() %>%
        mutate(cod_hda_key = predio$cod_hda_key, hda_nombre = nom)
    }, error = function(e) {
      message(sprintf("   ⚠️  splitr falló (%s). Cambiando a Modo B.", e$message))
      generar_trayectoria_fallback(predio$lat, predio$lon, predio$cod_hda_key, nom)
    })
  } else {
    # ── MODO B: Vectorial Open-Meteo ──
    generar_trayectoria_fallback(predio$lat, predio$lon, predio$cod_hda_key, nom)
  }
})

# ─── 5. ENSAMBLAR Y EXPORTAR ──────────────────────────────────────────────────
trayectorias_prev <- bind_rows(lista_trayectorias[!sapply(lista_trayectorias, is.null)])

if (nrow(trayectorias_prev) > 0) {
  
  # Convertir puntos a LINESTRING por hacienda
  trayectorias_sf <- trayectorias_prev %>%
    arrange(cod_hda_key, hour_along) %>%
    group_by(cod_hda_key, hda_nombre) %>%
    filter(n() > 1) %>%
    summarise(
      geometry = st_cast(
        st_combine(st_as_sfc(lapply(seq_len(n()), function(k) {
          st_point(c(lon[k], lat[k]))
        }), crs = 4326)),
        "LINESTRING"
      ),
      .groups = "drop"
    ) %>%
    st_as_sf(crs = 4326) %>%
    mutate(height = 50)
  
  if (nrow(trayectorias_sf) > 0 && all(st_is_valid(trayectorias_sf))) {
    saveRDS(trayectorias_sf, "data_master/HYSPLIT_plumas.rds")
    modo_msg <- if (usar_hysplit_real) "HYSPLIT real" else "vectorial Open-Meteo"
    message(sprintf("✅ Modelación de humo completada (%s): %d trayectorias generadas.",
                    modo_msg, nrow(trayectorias_sf)))
  } else {
    message("⚠️  Geometrías inválidas. Guardando NULL como fallback seguro.")
    saveRDS(NULL, "data_master/HYSPLIT_plumas.rds")
  }
  
} else {
  message("⚠️  Sin trayectorias generadas. Guardando NULL.")
  saveRDS(NULL, "data_master/HYSPLIT_plumas.rds")
}

return(invisible(NULL))
