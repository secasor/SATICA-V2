library(shiny)
library(bs4Dash)
library(dplyr)
library(tidyr)
library(leaflet)
library(sf)
library(readxl)
library(lubridate)
library(DT)
library(shinyWidgets)
library(stringr)
library(janitor)
library(stringi)
if (file.exists("instalar_dependencias.R")) {
  source("instalar_dependencias.R")
}
library(lwgeom)
library(readr)
library(rmarkdown)
library(openxlsx)
library(plotly)

# =========================================================================
# 🔄 DETECCIÓN DE ENTORNO Y SMART SYNC — ACTUALIZACIÓN AUTOMÁTICA
# =========================================================================
# Detectamos si se ejecuta en la nube (ShinyApps, Hugging Face Spaces o Docker)
.ON_CLOUD <- Sys.getenv('ON_CLOUD') == "true" || Sys.getenv('SHINYAPPS_ACCOUNT') != "" || Sys.getenv('SHINY_PORT') != ""

.SMART_SYNC_UMBRAL_MIN <- 30  
.RDS_PATH <- "data_master/SATICA_MASTER_v2.2.rds"

.datos_desactualizados <- function() {
  if (!file.exists(.RDS_PATH)) {
    message("📂 Smart Sync: No existe el archivo maestro. Iniciando creación...")
    return(TRUE)
  }
  mins_transcurridos <- as.numeric(difftime(Sys.time(),
                                             file.mtime(.RDS_PATH),
                                             units = "mins"))
  message(sprintf("⏱️  Smart Sync: Datos tienen %.0f minutos de antigüedad.", mins_transcurridos))
  return(mins_transcurridos > .SMART_SYNC_UMBRAL_MIN)
}

.hay_internet <- function() {
  tryCatch({
    con <- url("https://firms.modaps.eosdis.nasa.gov", open = "r")
    close(con)
    return(TRUE)
  }, error = function(e) FALSE)
}

if (.ON_CLOUD) {
  message("🌐 [ENTORNO NUBE] Detectado. Se omiten descargas y cruces espaciales pesados al inicio.")
  message("📡 SATICA consumirá la base maestra de datos y alertas en vivo pre-calculadas en GitHub Pages.")
} else {
  # --- SMART SYNC LOCAL (PARA DESARROLLADOR) ---
  if (!file.exists(.RDS_PATH)) {
    if (.hay_internet()) {
      message("🔄 Smart Sync: Datos desactualizados. Iniciando sincronización satelital...")
      message("  🛰️  [1/2] Consultando NASA FIRMS (VIIRS 375m + MODIS)...")
      tryCatch({
        source("R/api_nasa_firms.R")
        message("  ✅ NASA FIRMS: Telemetría actualizada.")
      }, error = function(e) {
        message("  ⚠️  NASA FIRMS sin datos (puede ser que el área esté limpia): ", e$message)
      })
      
      message("  ⚙️  [2/2] Ejecutando Motor de Consolidación y Predicción...")
      tryCatch({
        source("satica_engine.R")
        message("  ✅ Motor SATICA: Base maestra actualizada.")
      }, error = function(e) {
        message("  ❌ Error en Motor SATICA: ", e$message)
        message("  ℹ️  Se continuará con los datos del ciclo anterior.")
      })
      
      message("✅ Smart Sync completado. Cargando Dashboard...")
    } else {
      message("📡 Smart Sync: Sin conexión a internet detectada.")
      if (file.exists(.RDS_PATH)) {
        message("⚠️  MODO OFFLINE: Cargando últimos datos disponibles.")
      } else {
        stop("❌ Sin internet y sin datos en caché. Conecte y reinicie la App.")
      }
    }
  } else {
    message("✅ Smart Sync: Base maestra local disponible. Cargando Dashboard...")
  }
}

# -------------------------------------------------------------------------
# CONFIGURACIÓN DEL MOTOR
# -------------------------------------------------------------------------
suppressMessages(sf_use_s2(FALSE))

mapear_ingenio <- function(codigo) {
  if (is.null(codigo)) return("OTRO")
  codigo_clean <- toupper(str_trim(as.character(codigo)))
  case_when(
    is.na(codigo) | codigo_clean %in% c("NA", "", "NULL") ~ "OTRO",
    codigo_clean == "MN" ~ "MANUELITA",
    codigo_clean == "CB" ~ "LA CABAÑA",
    codigo_clean == "PR" ~ "PROVIDENCIA",
    codigo_clean == "PC" ~ "PICHICHI",
    codigo_clean == "CA" ~ "INCAUCA",
    codigo_clean == "CC" ~ "CENTRAL CASTILLA",
    codigo_clean == "ML" ~ "MARIA LUISA",
    codigo_clean == "MY" ~ "MAYAGÜEZ",
    TRUE ~ paste("ING.", codigo_clean)
  )
}

# --- FUNCIONES DE CARGA HÍBRIDA PARA OPTIMIZACIÓN EN LA NUBE ---
cargar_rds_hibrido <- function(local_path, url_path) {
  if (file.exists(local_path)) {
    message("⚡ Carga local rápida (Caché): ", local_path)
    res <- tryCatch(readRDS(local_path), error = function(e) NULL)
    if (!is.null(res)) return(res)
  }
  message("🌐 Descargando desde internet: ", url_path)
  temp_file <- tempfile(fileext = ".rds")
  tryCatch({
    download.file(url_path, temp_file, mode = "wb", quiet = TRUE)
    res <- readRDS(temp_file)
    unlink(temp_file)
    return(res)
  }, error = function(e) {
    if (file.exists(temp_file)) unlink(temp_file)
    stop("Error descargando RDS: ", e$message)
  })
}

# --- 2. CARGA PRINCIPAL: GEOMETRÍA + DATOS MAESTROS ---
tryCatch({
  
  master_rds <- cargar_rds_hibrido(
    "data_master/SATICA_MASTER_v2.2.rds",
    "https://secasor.github.io/SATICA-V2/data_master/SATICA_MASTER_v2.2.rds"
  )
  
  formatear_tiempo_g <- function(dias) {
    if (is.na(dias) || dias == 0) return("Sin Historial")
    if (dias < 60) return(paste(round(dias), "días"))
    meses <- floor(dias / 30.44); sobra <- round(dias %% 30.44)
    txt_m <- ifelse(meses == 1, "1 mes", paste(meses, "meses"))
    txt_d <- ifelse(sobra > 0, paste(sobra, "días"), "")
    return(trimws(paste(txt_m, txt_d)))
  }
  
  DB_RIESGO <- master_rds %>%
    group_by(cod_hda_key) %>%
    summarise(
      TOTAL_HISTORICO = max(N_EVENTOS_HDA,      na.rm = TRUE),
      N_EVENTOS       = max(N_EVENTOS_HDA,      na.rm = TRUE),
      FECHA_ULT_I     = max(FECHA_ULT_I_HDA,    na.rm = TRUE),
      CICLO_DIAS      = mean(FRECUENCIA_HDA_DIAS,na.rm = TRUE),
      TXT_CICLO       = sapply(mean(FRECUENCIA_HDA_DIAS, na.rm = TRUE), formatear_tiempo_g),
      DIFF_MESES      = min(DIFF_HDA_MESES,      na.rm = TRUE),
      RIESGO = { idx <- which.min(DIFF_HDA_MESES); if (length(idx) > 0) riesgo[idx] else NA_character_ },
      COL    = { idx <- which.min(DIFF_HDA_MESES); if (length(idx) > 0) col[idx]    else NA_character_ },
      FECHA_ESTIMADA  = as.Date(max(FECHA_ULT_I_HDA, na.rm = TRUE) +
                                  mean(FRECUENCIA_HDA_DIAS, na.rm = TRUE)),
      SAT_FUEGO       = if("Satelite_Fuego" %in% names(.)) any(Satelite_Fuego == TRUE, na.rm = TRUE) else FALSE,
      GOES_FUEGO      = if("GOES_Fuego" %in% names(.)) any(GOES_Fuego == TRUE, na.rm = TRUE) else FALSE,
      ESTADO_GOES     = if("Estado_GOES" %in% names(.)) first(na.omit(Estado_GOES)) else "Normal",
      MAX_NDVI        = if("NDVI" %in% names(.)) max(NDVI, na.rm = TRUE) else 0.5,
      ESTADO_BIOMASA  = if("Alerta_Combustion" %in% names(.)) first(na.omit(Alerta_Combustion)) else "Sin Datos",
      .groups = "drop"
    ) %>%
    mutate(
      FECHA_ULT_I = if_else(is.infinite(FECHA_ULT_I), as.Date(NA), as.Date(FECHA_ULT_I)),
      RIESGO      = coalesce(RIESGO, "BAJO"),
      COL         = coalesce(COL,    "#7f8c8d"),
      COD_HDA_KEY = cod_hda_key
    )

  # --- 3. CACHÉ GEOESPACIAL: EVITAR REPROCESAMIENTO ---
  cache_geo_path <- "data_estatica/GEO_CACHE_SATICA.rds"
  cache_rest_path <- "data_estatica/REST_CACHE_SATICA.RData"
  
  if (!dir.exists("data_estatica")) dir.create("data_estatica")
  
  if (file.exists(cache_geo_path) && file.exists(cache_rest_path)) {
    message("⚡ CACHE LOCAL ENCONTRADO: Cargando Base Geoespacial Estática...")
    DATOS_ESPACIALES_BASE <- readRDS(cache_geo_path)
    load(cache_rest_path, envir = .GlobalEnv)
  } else if (.ON_CLOUD) {
    message("🌐 CLOUD CACHE: Descargando Base Geoespacial Estática desde Portal de GitHub Pages...")
    temp_geo <- tempfile(fileext = ".rds")
    tryCatch({
      download.file("https://secasor.github.io/SATICA-V2/data_estatica/GEO_CACHE_SATICA.rds", temp_geo, mode = "wb", quiet = TRUE)
      DATOS_ESPACIALES_BASE <- readRDS(temp_geo)
      unlink(temp_geo)
    }, error = function(e) {
      if (file.exists(temp_geo)) unlink(temp_geo)
      stop("Error descargando geo cache: ", e$message)
    })
    
    # Cargar archivo .RData desde la URL descargándolo a un archivo temporal
    temp_rest <- tempfile(fileext = ".RData")
    tryCatch({
      download.file("https://secasor.github.io/SATICA-V2/data_estatica/REST_CACHE_SATICA.RData", temp_rest, mode = "wb", quiet = TRUE)
      load(temp_rest, envir = .GlobalEnv)
    }, error = function(e) {
      message("⚠️ Error al descargar REST_CACHE_SATICA.RData: ", e$message)
    }, finally = {
      if (file.exists(temp_rest)) unlink(temp_rest)
    })
  } else {
    message("⏳ CACHE NO ENCONTRADO: Generando Base Geoespacial (Esto tomará 1-2 minutos)...")
    
    corr_path <- list.files("capas", pattern = "Corregimientos.*\\.shp$", full.names = TRUE)[1]
    SHP_CORR <- st_read(corr_path, quiet = TRUE) %>% 
      st_transform(4326) %>% 
      st_make_valid() %>% 
      select(NOM_MUNICI, NOM_DIV_PO)
    
    cana_path <- list.files("capas", pattern = "SOR_OK\\.shp$", full.names = TRUE)[1]
    temp_shp <- st_read(cana_path, quiet = TRUE) %>% 
      st_transform(4326) %>% 
      st_zm(drop = TRUE) %>% 
      clean_names() %>%
      st_make_valid() 
    
    DATOS_ESPACIALES_BASE <- temp_shp %>%
      st_transform(3857) %>%
      mutate(
        NOM_RAW = toupper(as.character(coalesce(!!!select(st_drop_geometry(.), matches("^nombre_hda$|^nombre$"))))),
        COD_ING_TXT = toupper(as.character(coalesce(!!!select(st_drop_geometry(.), matches("^ing$|^ingenio$"))))),
        hda_limpia = toupper(str_replace_all(as.character(coalesce(!!!select(st_drop_geometry(.), matches("^hda$|^cod$|^codigo$")))), "[^0-9A-Za-z_]", "")),
        COD_HDA_PAD = str_pad(hda_limpia, width = 6, side = "left", pad = "0"),
        COD_ING = case_when(
          grepl("INCAUCA", COD_ING_TXT) ~ "CA",
          grepl("MAYAGUEZ", COD_ING_TXT) ~ "MY",
          grepl("MARIA LUISA", COD_ING_TXT) ~ "ML",
          grepl("CASTILLA", COD_ING_TXT) ~ "CC",
          grepl("PROVIDENCIA", COD_ING_TXT) ~ "PR",
          grepl("MANUELITA", COD_ING_TXT) ~ "MN",
          grepl("PICHICHI", COD_ING_TXT) ~ "PC",
          grepl("CABA", COD_ING_TXT) ~ "CB",
          grepl("RIOPAILA", COD_ING_TXT) ~ "RP",
          TRUE ~ COD_ING_TXT
        ),
        COD_HDA_KEY = paste(COD_ING, COD_HDA_PAD, sep = "_"),
        HDA_LABEL = str_trim(NOM_RAW)
      ) %>%
      mutate(geometry = st_buffer(geometry, 15)) %>%
      group_by(COD_HDA_KEY, COD_ING_TXT) %>% 
      summarise(
        HDA_LABEL = first(HDA_LABEL),
        CANTIDAD_SUERTES = n(), 
        geometry = st_union(geometry), 
        .groups = "drop"
      ) %>%
      mutate(geometry = st_buffer(geometry, -15)) %>%
      st_simplify(preserveTopology = TRUE, dTolerance = 1) %>%
      st_make_valid() %>% 
      st_collection_extract("POLYGON") %>% 
      st_cast("MULTIPOLYGON") %>%          
      st_transform(4326) %>%
      st_join(SHP_CORR, join = st_intersects, left = TRUE, largest = TRUE) %>%
      mutate(
        MUNICIPIO = toupper(coalesce(NOM_MUNICI, "SIN MUNICIPIO")),
        CORREGIMIENTO = toupper(coalesce(NOM_DIV_PO, "SIN DATO")),
        INGENIO_FULL = mapear_ingenio(COD_ING_TXT)
      )
      
    # CACHE DE RESTRICCIONES
    message("  -> Calculando Restricciones Ambientales Estáticas...")
    DATOS_ESPACIALES_BASE <- DATOS_ESPACIALES_BASE %>% mutate(RESTRICCIONES_CVC = "Ninguna")
    tryCatch({
      map_ramsar   <- st_read("capas/restricciones/Ramsar.shp", quiet = TRUE) %>% st_zm() %>% st_transform(4326) %>% st_make_valid()
      map_ap       <- st_read("capas/restricciones/Áreas_Protegidas.shp", quiet = TRUE) %>% st_zm() %>% st_transform(4326) %>% st_make_valid()
      map_recarga  <- st_read("capas/restricciones/Zona_de_recarga.shp", quiet = TRUE) %>% st_zm() %>% st_transform(4326) %>% st_make_valid()
      map_forest   <- st_read("capas/restricciones/Área Forestal Protectora_ (R_Q_Ar)_disolve.shp", quiet = TRUE) %>% st_zm() %>% st_transform(4326) %>% st_make_valid()
      map_cauca    <- st_read("capas/restricciones/Buffer_50_m_Río_Cauca.shp", quiet = TRUE) %>% st_zm() %>% st_transform(4326) %>% st_make_valid()
      map_pob      <- st_read("capas/restricciones/Buffer_Centros_Poblados_V2.shp", quiet = TRUE) %>% st_zm() %>% st_transform(4326) %>% st_make_valid()
      map_pozos    <- st_read("capas/restricciones/Buffer_pozos_ab_pub_RH.shp", quiet = TRUE) %>% st_zm() %>% st_transform(4326) %>% st_make_valid()
      
      shp_dar_path <- "capas/Dirección_Ambiental_Regional.shp"
      limite_regional <- NULL
      if (file.exists(shp_dar_path)) {
        shp_dar <- st_read(shp_dar_path, quiet = TRUE) %>% st_transform(4326) %>% st_make_valid()
        limite_regional <- shp_dar[grepl("SURORIENTE", toupper(shp_dar[[which(sapply(shp_dar, is.character))[1]]]), ignore.case = TRUE), ]
        if (nrow(limite_regional) == 0) limite_regional <- shp_dar 
        limite_regional <- st_union(limite_regional)
      } else {
        limite_regional <- st_as_sfc(st_bbox(DATOS_ESPACIALES_BASE))
      }
      
      safe_crop <- function(shp) {
        if(nrow(shp) == 0) return(shp)
        shp_bbox <- st_bbox(limite_regional)
        shp_pre_crop <- tryCatch({ suppressWarnings(st_crop(shp, shp_bbox)) }, error = function(e) shp)
        if (nrow(shp_pre_crop) == 0) return(shp_pre_crop)
        tryCatch({
          suppressWarnings(st_intersection(shp_pre_crop, limite_regional) %>% st_simplify(dTolerance = 0.001))
        }, error = function(e) {
          suppressWarnings(st_crop(shp_pre_crop, st_bbox(DATOS_ESPACIALES_BASE)) %>% st_simplify(dTolerance = 0.001))
        })
      }
      
      SHP_REST_RAMSAR   <<- safe_crop(map_ramsar)
      SHP_REST_AP       <<- safe_crop(map_ap)
      SHP_REST_RECARGA  <<- safe_crop(map_recarga)
      SHP_REST_FORESTAL <<- safe_crop(map_forest)
      SHP_REST_CAUCA    <<- safe_crop(map_cauca)
      SHP_REST_POBLADOS <<- safe_crop(map_pob)
      SHP_REST_POZOS    <<- safe_crop(map_pozos)
      
      idx_ramsar   <- lengths(st_intersects(DATOS_ESPACIALES_BASE, SHP_REST_RAMSAR)) > 0
      idx_ap       <- lengths(st_intersects(DATOS_ESPACIALES_BASE, SHP_REST_AP)) > 0
      idx_recarga  <- lengths(st_intersects(DATOS_ESPACIALES_BASE, SHP_REST_RECARGA)) > 0
      idx_forestal <- lengths(st_intersects(DATOS_ESPACIALES_BASE, SHP_REST_FORESTAL)) > 0
      idx_cauca    <- lengths(st_intersects(DATOS_ESPACIALES_BASE, SHP_REST_CAUCA)) > 0
      idx_pob      <- lengths(st_intersects(DATOS_ESPACIALES_BASE, SHP_REST_POBLADOS)) > 0
      idx_pozos    <- lengths(st_intersects(DATOS_ESPACIALES_BASE, SHP_REST_POZOS)) > 0
      
      DATOS_ESPACIALES_BASE <- DATOS_ESPACIALES_BASE %>%
        mutate(REST_TXT = case_when(
          idx_ramsar | idx_ap | idx_recarga | idx_forestal | idx_cauca | idx_pob | idx_pozos ~ paste0(
            ifelse(idx_ramsar, "Sitio Ramsar | ", ""),
            ifelse(idx_ap, "Área Protegida | ", ""),
            ifelse(idx_recarga, "Zona de Recarga | ", ""),
            ifelse(idx_forestal, "Reserva Forestal | ", ""),
            ifelse(idx_cauca, "Buffer Río Cauca | ", ""),
            ifelse(idx_pob, "Centro Poblado | ", ""),
            ifelse(idx_pozos, "Pozo Subterráneo | ", "")
          ),
          TRUE ~ "Ninguna"
        )) %>%
        mutate(RESTRICCIONES_CVC = gsub(" \\| $", "", REST_TXT)) %>%
        select(-REST_TXT)
        
    }, error = function(e) {
      message("⚠️ Error procesando restricciones: ", e$message)
    })
    
    saveRDS(DATOS_ESPACIALES_BASE, cache_geo_path)
    save(SHP_REST_RAMSAR, SHP_REST_AP, SHP_REST_RECARGA, SHP_REST_FORESTAL, SHP_REST_CAUCA, SHP_REST_POBLADOS, SHP_REST_POZOS, file = cache_rest_path)
    message("✅ CACHE CREADO Y GUARDADO.")
  }

  # --- CRUCE EN VIVO: DATOS_OFICIALES = GEOMETRÍA ESTÁTICA + MASTER_RDS SATELITAL ---
  DATOS_OFICIALES <- DATOS_ESPACIALES_BASE %>%
    left_join(DB_RIESGO, by = "COD_HDA_KEY") %>%
    mutate(
      TOTAL_HISTORICO = coalesce(TOTAL_HISTORICO, 0),
      N_EVENTOS = coalesce(N_EVENTOS, 0),
      FECHA_ULT_I = as.Date(FECHA_ULT_I, origin = "1970-01-01"),
      SAT_FUEGO   = coalesce(SAT_FUEGO, FALSE),
      GOES_FUEGO  = coalesce(GOES_FUEGO, FALSE),
      ESTADO_GOES = coalesce(ESTADO_GOES, "Normal"),
      ESTADO_BIOMASA = coalesce(ESTADO_BIOMASA, "Sin Datos"),
      RIESGO      = coalesce(RIESGO, "BAJO"),
      COL         = coalesce(COL,    "#27ae60")
    ) %>%
    filter(INGENIO_FULL != "OTRO")

  message("✅ PROCESO GLOBAL LISTO: Haciendas + Sensores Satelitales Acoplados.")
  message("🔢 Total Haciendas Cargadas: ", nrow(DATOS_OFICIALES))
  
  # --- CARGA DE HACIENDAS SIN GEORREF ---
  RUTA_PUNTO_CIEGO <- "resultados_diagnostico/punto_ciego_suroriente.csv"
  RUTA_COORDS      <- "sin_georref_coords.csv"
  
  # Carga híbrida inteligente para Punto Ciego
  if (file.exists(RUTA_PUNTO_CIEGO)) {
    message("⚡ Carga local rápida: ", RUTA_PUNTO_CIEGO)
    sin_georref_base <- read.csv(RUTA_PUNTO_CIEGO, stringsAsFactors = FALSE, encoding = "UTF-8") %>%
      mutate(
        COD_HDA_8       = toupper(trimws(COD_HDA_8)),
        Municipio_Excel = toupper(trimws(coalesce(Municipio_Excel, "SIN DATO"))),
        Correg_Excel    = toupper(trimws(coalesce(Correg_Excel,    "SIN DATO")))
      )
  } else if (.ON_CLOUD) {
    message("🌐 Descargando Punto Ciego desde GitHub Pages...")
    sin_georref_base <- tryCatch({
      read.csv(url("https://secasor.github.io/SATICA-V2/resultados_diagnostico/punto_ciego_suroriente.csv"),
               stringsAsFactors = FALSE, encoding = "UTF-8") %>%
        mutate(
          COD_HDA_8       = toupper(trimws(COD_HDA_8)),
          Municipio_Excel = toupper(trimws(coalesce(Municipio_Excel, "SIN DATO"))),
          Correg_Excel    = toupper(trimws(coalesce(Correg_Excel,    "SIN DATO")))
        )
    }, error = function(e) {
      data.frame(
        COD_HDA_8 = character(), Nombre_Reporte = character(),
        Ingenio = character(), Municipio_Excel = character(),
        Correg_Excel = character(), n_registros = integer(),
        n_suertes = integer(), Prioridad = character(),
        stringsAsFactors = FALSE
      )
    })
  } else {
    sin_georref_base <- data.frame(
      COD_HDA_8 = character(), Nombre_Reporte = character(),
      Ingenio = character(), Municipio_Excel = character(),
      Correg_Excel = character(), n_registros = integer(),
      n_suertes = integer(), Prioridad = character(),
      stringsAsFactors = FALSE
    )
  }
  
  # Carga híbrida inteligente para Coordenadas Manuales
  if (file.exists(RUTA_COORDS)) {
    message("⚡ Carga local rápida: ", RUTA_COORDS)
    coords_manual <- read.csv(RUTA_COORDS, stringsAsFactors = FALSE, encoding = "UTF-8") %>%
      mutate(COD_HDA_8 = toupper(trimws(COD_HDA_8)))
  } else if (.ON_CLOUD) {
    message("🌐 Descargando Coordenadas Manuales desde GitHub Pages...")
    coords_manual <- tryCatch({
      read.csv(url("https://secasor.github.io/SATICA-V2/sin_georref_coords.csv"),
               stringsAsFactors = FALSE, encoding = "UTF-8") %>%
        mutate(COD_HDA_8 = toupper(trimws(COD_HDA_8)))
    }, error = function(e) {
      data.frame(
        COD_HDA_8     = sin_georref_base$COD_HDA_8,
        LAT           = NA_real_,
        LON           = NA_real_,
        Notas         = NA_character_,
        Fecha_Georref = NA_character_,
        stringsAsFactors = FALSE
      )
    })
  } else {
    coords_manual <- data.frame(
      COD_HDA_8     = sin_georref_base$COD_HDA_8,
      LAT           = NA_real_,
      LON           = NA_real_,
      Notas         = NA_character_,
      Fecha_Georref = NA_character_,
      stringsAsFactors = FALSE
    )
    write.csv(coords_manual, RUTA_COORDS, row.names = FALSE, fileEncoding = "UTF-8")
  }
  
  # --- DETECCIÓN DINÁMICA DE PREDIOS HUÉRFANOS SIN GEOMETRÍA ---
  orphans_keys <- setdiff(unique(master_rds$cod_hda_key), unique(DATOS_ESPACIALES_BASE$COD_HDA_KEY))
  valid_prefixes <- c("CA", "CB", "CC", "ML", "MN", "MY", "PC", "PR", "RP")
  orphans_keys <- orphans_keys[substr(orphans_keys, 1, 2) %in% valid_prefixes]
  
  if (length(orphans_keys) > 0) {
    new_orphans <- master_rds %>%
      filter(cod_hda_key %in% orphans_keys) %>%
      group_by(cod_hda_key) %>%
      summarise(
        Nombre_Reporte = coalesce(first(na.omit(hda_nombre)), paste("Hda", first(cod_hda_key))),
        Ingenio = mapear_ingenio(substr(first(cod_hda_key), 1, 2)),
        Municipio_Excel = coalesce(first(na.omit(municipio)), "SIN MUNICIPIO"),
        Correg_Excel = coalesce(first(na.omit(corregimiento)), "SIN DATO"),
        n_registros = max(N_EVENTOS_HDA, na.rm = TRUE),
        n_suertes = n_distinct(cod_unico),
        .groups = "drop"
      ) %>%
      mutate(
        COD_HDA_8 = gsub("_", "", cod_hda_key)
      ) %>%
      filter(!COD_HDA_8 %in% sin_georref_base$COD_HDA_8)
    
    if (nrow(new_orphans) > 0) {
      new_orphans_df <- new_orphans %>%
        mutate(
          NOMBRE_HDA = NA_character_,
          ING_HDA = NA_character_,
          Tipo_Match = "Sin match — no encontrada en capa",
          Municipio_Limpio = Municipio_Excel,
          Clasificacion_Final = "Punto ciego — Suroriente sin geometría",
          Prioridad = case_when(
            n_registros >= 10 ~ "ALTA — 10+ eventos",
            n_registros >= 5  ~ "MEDIA — 5-9 eventos",
            TRUE              ~ "BAJA — 1-2 eventos"
          )
        ) %>%
        select(names(sin_georref_base))
      
      sin_georref_base <- bind_rows(sin_georref_base, new_orphans_df)
    }
  }
  
  # Asegurar que todas las haciendas sin georreferenciación tengan su plantilla en coords_manual
  missing_coords <- setdiff(sin_georref_base$COD_HDA_8, coords_manual$COD_HDA_8)
  if (length(missing_coords) > 0) {
    new_coords_df <- data.frame(
      COD_HDA_8 = missing_coords,
      LAT = NA_real_,
      LON = NA_real_,
      Notas = "Detectado en reportes sin geometría catastral",
      Fecha_Georref = as.character(Sys.Date()),
      stringsAsFactors = FALSE
    )
    coords_manual <- bind_rows(coords_manual, new_coords_df)
    if (!.ON_CLOUD) {
      write.csv(coords_manual, RUTA_COORDS, row.names = FALSE, fileEncoding = "UTF-8")
    }
  }
  
  SIN_GEORREF <- sin_georref_base %>%
    left_join(coords_manual %>% select(COD_HDA_8, LAT, LON, Notas, Fecha_Georref),
              by = "COD_HDA_8") %>%
    mutate(
      Tiene_Coords   = !is.na(LAT) & !is.na(LON),
      CICLO_DIAS     = NA_real_,
      FECHA_ULT_I    = as.Date(NA),
      FECHA_ESTIMADA = as.Date(NA),
      DIFF_MESES     = NA_real_,
      RIESGO         = "SIN GEORREF",
      COL            = "#7f8c8d"
    )
  
  ciclos_sg <- master_rds %>%
    group_by(ing_hda) %>%
    summarise(
      FECHA_ULT_I    = if_else(is.infinite(max(FECHA_ULT_I_HDA, na.rm=TRUE)),
                               as.Date(NA), as.Date(max(FECHA_ULT_I_HDA, na.rm=TRUE))),
      CICLO_DIAS     = mean(FRECUENCIA_HDA_DIAS, na.rm = TRUE),
      DIFF_MESES     = min(DIFF_HDA_MESES,       na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      FECHA_ESTIMADA = as.Date(FECHA_ULT_I + CICLO_DIAS),
      RIESGO = case_when(
        is.na(DIFF_MESES)                  ~ "BAJO",
        DIFF_MESES >= -1 & DIFF_MESES <= 1 ~ "CRITICO",
        DIFF_MESES > 1  & DIFF_MESES <= 2  ~ "ALTO",
        DIFF_MESES < -1 & DIFF_MESES >= -3 ~ "OBSERVACION",
        TRUE                               ~ "BAJO"
      ),
      COL = case_when(
        RIESGO == "CRITICO"     ~ "#c0392b",
        RIESGO == "ALTO"        ~ "#e67e22",
        RIESGO == "OBSERVACION" ~ "#f1c40f",
        TRUE                    ~ "#7f8c8d"
      )
    )
  
  SIN_GEORREF <- SIN_GEORREF %>%
    left_join(ciclos_sg, by = c("COD_HDA_8" = "ing_hda"), suffix = c("", "_rds")) %>%
    mutate(
      FECHA_ULT_I    = coalesce(FECHA_ULT_I_rds,    FECHA_ULT_I),
      CICLO_DIAS     = coalesce(CICLO_DIAS_rds,      CICLO_DIAS),
      DIFF_MESES     = coalesce(DIFF_MESES_rds,      DIFF_MESES),
      FECHA_ESTIMADA = coalesce(FECHA_ESTIMADA_rds,  FECHA_ESTIMADA),
      RIESGO         = coalesce(RIESGO_rds,           RIESGO),
      COL            = coalesce(COL_rds,              COL)
    ) %>%
    select(-ends_with("_rds"))
  
  # --- ANCLA OPERATIVA: Integrar haciendas con coordenadas GPS al sistema ---
  DATOS_OFICIALES$ES_ANCLA <- FALSE
  
  sg_con_coords <- SIN_GEORREF %>% filter(Tiene_Coords == TRUE)
  
  if (nrow(sg_con_coords) > 0) {
    DATOS_ANCLA <- sg_con_coords %>%
      mutate(
        COD_HDA_KEY      = paste0(substr(COD_HDA_8, 1, 2), "_", substr(COD_HDA_8, 3, 8)),
        HDA_LABEL        = toupper(trimws(Nombre_Reporte)),
        COD_ING_TXT      = substr(COD_HDA_8, 1, 2),
        INGENIO_FULL     = mapear_ingenio(COD_ING_TXT),
        MUNICIPIO        = toupper(coalesce(Municipio_Excel, "SIN MUNICIPIO")),
        CORREGIMIENTO    = toupper(coalesce(Correg_Excel, "SIN DATO")),
        CANTIDAD_SUERTES = 0L,
        TOTAL_HISTORICO  = coalesce(as.integer(n_registros), 0L),
        N_EVENTOS        = coalesce(as.integer(n_registros), 0L),
        TXT_CICLO        = sapply(CICLO_DIAS, formatear_tiempo_g),
        SAT_FUEGO        = FALSE,
        GOES_FUEGO       = FALSE,
        ESTADO_GOES      = "Normal",
        MAX_NDVI         = 0.5,
        ESTADO_BIOMASA   = "Sin Datos",
        ES_ANCLA         = TRUE,
        RESTRICCIONES_CVC= "Ninguna"
      ) %>%
      st_as_sf(coords = c("LON", "LAT"), crs = 4326)
    
    # Alinear columnas
    for (col in setdiff(names(DATOS_OFICIALES), c(names(DATOS_ANCLA), "geometry"))) {
      DATOS_ANCLA[[col]] <- NA
    }
    
    DATOS_OFICIALES <- bind_rows(DATOS_OFICIALES, DATOS_ANCLA) %>%
      filter(INGENIO_FULL != "OTRO")
    SIN_GEORREF <- SIN_GEORREF %>% filter(Tiene_Coords == FALSE)
    
    message(paste("\U0001f4cd Ancla Operativa:", nrow(DATOS_ANCLA),
                  "haciendas integradas como puntos GPS al Radar Preventivo"))
  }
  
  message(paste("\U0001f50d Sin Georref:", nrow(SIN_GEORREF),
                "| Con coords:", sum(SIN_GEORREF$Tiene_Coords, na.rm = TRUE)))

}, error = function(e) { stop(paste("Error Fatal en Carga Global:", e)) })
