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
if (!requireNamespace("lwgeom", quietly = TRUE)) install.packages("lwgeom", repos = "https://cran.rstudio.com/")
library(lwgeom)
library(readr)
library(rmarkdown)
library(openxlsx)

# =========================================================================
# 🔄 SMART SYNC — ACTUALIZACIÓN AUTOMÁTICA AL INICIO
# =========================================================================
# Elimina la necesidad de ejecutar actualizar_y_ejecutar.R manualmente.
# Cada vez que se abre la App, verifica si los datos están desactualizados.
# Si tienen más de 30 minutos, actualiza automáticamente vía NASA FIRMS.
# Modo Offline: si no hay internet, usa los últimos datos disponibles.
# =========================================================================

.SMART_SYNC_UMBRAL_MIN <- 30  # Minutos de tolerancia antes de re-sincronizar
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

if (.datos_desactualizados()) {
  if (.hay_internet()) {
    message("🔄 Smart Sync: Datos desactualizados. Iniciando sincronización satelital...")
    
    # Paso 1: Actualizar telemetría NASA FIRMS (sin GEE, sin Python)
    message("  🛰️  [1/2] Consultando NASA FIRMS (VIIRS 375m + MODIS)...")
    tryCatch({
      source("R/api_nasa_firms.R")
      message("  ✅ NASA FIRMS: Telemetría actualizada.")
    }, error = function(e) {
      message("  ⚠️  NASA FIRMS sin datos (puede ser que el área esté limpia): ", e$message)
    })
    
    # Paso 2: Reconstruir el Motor de Predicción SATICA
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
    # MODO OFFLINE: sin internet, usa caché disponible
    message("📡 Smart Sync: Sin conexión a internet detectada.")
    if (file.exists(.RDS_PATH)) {
      message("⚠️  MODO OFFLINE: Cargando últimos datos satelitales disponibles.")
      message("   Los datos pueden tener hasta ",
              round(as.numeric(difftime(Sys.time(), file.mtime(.RDS_PATH), units = "hours")), 1),
              " horas de antigüedad.")
    } else {
      stop("❌ Sin internet y sin datos en caché. Conecte a internet y reinicie la App.")
    }
  }
} else {
  message("✅ Smart Sync: Datos actualizados (< 30 min). Cargando Dashboard directamente.")
}

# -------------------------------------------------------------------------
# CONFIGURACIÓN DEL MOTOR
# -------------------------------------------------------------------------
suppressMessages(sf_use_s2(FALSE))

# [BLINDAJE 2: SINCRONIZACIÓN ÚNICA APLICADA]
# Se eliminó por completo la función cargar_historial_incendios()
# Todo surge ahora desde el SATICA_MASTER_v2.2.rds

mapear_ingenio <- function(codigo) {
  codigo <- toupper(str_trim(as.character(codigo)))
  case_when(
    codigo == "MN" ~ "MANUELITA",
    codigo == "CB" ~ "LA CABAÑA",
    codigo == "PR" ~ "PROVIDENCIA",
    codigo == "PC" ~ "PICHICHI",
    codigo == "CA" ~ "INCAUCA",
    codigo == "CC" ~ "CENTRAL CASTILLA",
    codigo == "ML" ~ "MARIA LUISA",
    codigo == "MY" ~ "MAYAGÜEZ",
    TRUE ~ paste("ING.", codigo)
  )
}

# --- 2. CARGA PRINCIPAL: GEOMETRÍA + DATOS MAESTROS ---
tryCatch({
  
  # Cargar la única fuente de verdad: el Master generado por satica_engine
  master_rds <- readRDS("data_master/SATICA_MASTER_v2.2.rds")
  
  # Función de apoyo temporal obligatoria para la fusión
  formatear_tiempo_g <- function(dias) {
    if (is.na(dias) || dias == 0) return("Sin Historial")
    if (dias < 60) return(paste(round(dias), "días"))
    meses <- floor(dias / 30.44); sobra <- round(dias %% 30.44)
    txt_m <- ifelse(meses == 1, "1 mes", paste(meses, "meses"))
    txt_d <- ifelse(sobra > 0, paste(sobra, "días"), "")
    return(trimws(paste(txt_m, txt_d)))
  }
  
  # Agregamos el RDS a nivel de Hacienda para coincidir con la Fusión Nuclear de shapefiles
  DB_RIESGO <- master_rds %>%
    group_by(cod_hda_key) %>%
    summarise(
      TOTAL_HISTORICO = max(N_EVENTOS_HDA,      na.rm = TRUE),
      N_EVENTOS       = max(N_EVENTOS_HDA,      na.rm = TRUE),
      FECHA_ULT_I     = max(FECHA_ULT_I_HDA,    na.rm = TRUE),
      CICLO_DIAS      = mean(FRECUENCIA_HDA_DIAS,na.rm = TRUE),
      TXT_CICLO       = sapply(mean(FRECUENCIA_HDA_DIAS, na.rm = TRUE), formatear_tiempo_g),
      DIFF_MESES      = min(DIFF_HDA_MESES,      na.rm = TRUE),
      RIESGO          = riesgo[which.min(DIFF_HDA_MESES)],
      COL             = col[which.min(DIFF_HDA_MESES)],
      FECHA_ESTIMADA  = as.Date(max(FECHA_ULT_I_HDA, na.rm = TRUE) +
                                  mean(FRECUENCIA_HDA_DIAS, na.rm = TRUE)),
      # NUEVOS: Atributos Satelitales (Blindaje contra columnas faltantes)
      SAT_FUEGO       = if("Satelite_Fuego" %in% names(.)) any(Satelite_Fuego == TRUE, na.rm = TRUE) else FALSE,
      GOES_FUEGO      = if("GOES_Fuego" %in% names(.)) any(GOES_Fuego == TRUE, na.rm = TRUE) else FALSE,
      ESTADO_GOES     = if("Estado_GOES" %in% names(.)) first(na.omit(Estado_GOES)) else "Normal",
      MAX_NDVI        = if("NDVI" %in% names(.)) max(NDVI, na.rm = TRUE) else 0.5,
      ESTADO_BIOMASA  = if("Alerta_Combustion" %in% names(.)) first(na.omit(Alerta_Combustion)) else "Sin Datos",
      .groups = "drop"
    ) %>%
    mutate(
      FECHA_ULT_I = if_else(is.infinite(FECHA_ULT_I), as.Date(NA), as.Date(FECHA_ULT_I)),
      # cod_hda_key ya tiene guion bajo (ej: CA_010685) — igual que COD_HDA_KEY en DATOS_OFICIALES
      COD_HDA_KEY = cod_hda_key
    )
  
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
  
  DATOS_OFICIALES <- temp_shp %>%
    st_transform(3857) %>% # Proyección a Metros para la "Fusión Nuclear"
    
    mutate(
      NOM_RAW = toupper(as.character(coalesce(!!!select(st_drop_geometry(.), matches("^nombre_hda$|^nombre$"))))),
      COD_ING_TXT = toupper(as.character(coalesce(!!!select(st_drop_geometry(.), matches("^ing$|^ingenio$"))))),
      
      # [BLINDAJE 1: ANTI-TRITURACIÓN APLICADO A LA GEOMETRÍA]
      # Extraemos limpiamente el código respetando letras (4A) y ceros (_008) (con Case-Insensitive toupper)
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
    
    # --- FASE DE FUSIÓN Y MÉTRICAS ESTRUCTURALES ---
    mutate(geometry = st_buffer(geometry, 15)) %>% # Expandir para cerrar caminos
    
    group_by(COD_HDA_KEY, COD_ING_TXT) %>% 
    summarise(
      HDA_LABEL = first(HDA_LABEL),
      # CANTIDAD_SUERTES: Cuenta cuántas suertes componen la hacienda
      CANTIDAD_SUERTES = n(), 
      geometry = st_union(geometry), 
      .groups = "drop"
    ) %>%
    
    # --- FASE DE LIMPIEZA GEOMÉTRICA ---
    mutate(geometry = st_buffer(geometry, -15)) %>% # Contraer al tamaño original
    st_simplify(preserveTopology = TRUE, dTolerance = 1) %>% # Eliminar arrugas
    st_make_valid() %>% 
    st_collection_extract("POLYGON") %>% 
    st_cast("MULTIPOLYGON") %>%          
    
    st_transform(4326) %>% # Volver a Lat/Lon
    
    # Cruce Geográfico
    st_join(SHP_CORR, join = st_intersects, left = TRUE, largest = TRUE) %>%
    mutate(
      MUNICIPIO = toupper(coalesce(NOM_MUNICI, "SIN MUNICIPIO")),
      CORREGIMIENTO = toupper(coalesce(NOM_DIV_PO, "SIN DATO")),
      INGENIO_FULL = mapear_ingenio(COD_ING_TXT)
    ) %>%
    
    # --- CRUCE DE DATOS CRÍTICOS DEL MASTER RDS ---
    left_join(DB_RIESGO, by = "COD_HDA_KEY") %>%
    mutate(
      TOTAL_HISTORICO = coalesce(TOTAL_HISTORICO, 0),
      N_EVENTOS = coalesce(N_EVENTOS, 0),
      FECHA_ULT_I = as.Date(FECHA_ULT_I, origin = "1970-01-01"),
      SAT_FUEGO   = coalesce(SAT_FUEGO, FALSE),
      GOES_FUEGO  = coalesce(GOES_FUEGO, FALSE),
      ESTADO_GOES = coalesce(ESTADO_GOES, "Normal"),
      ESTADO_BIOMASA = coalesce(ESTADO_BIOMASA, "Sin Datos")
      # Nota: RIESGO, COL, DIFF_MESES ya vienen cargados y blindados del join si existen en esa Hacienda.
    )
  
  message(paste("✅ PROCESO GLOBAL COMPLETADO BAJO REGLAS DE BLINDAJE."))
  message(paste("📊 Datos Estructurales (Suertes) e Historial RDS integrados."))
  message(paste("🔢 Total Haciendas Fusionadas:", nrow(DATOS_OFICIALES)))
  
  # --- CARGA DE HACIENDAS SIN GEORREF ---
  RUTA_PUNTO_CIEGO <- "resultados_diagnostico/punto_ciego_suroriente.csv"
  RUTA_COORDS      <- "sin_georref_coords.csv"
  
  if (file.exists(RUTA_PUNTO_CIEGO)) {
    sin_georref_base <- read.csv(RUTA_PUNTO_CIEGO,
                                 stringsAsFactors = FALSE, encoding = "UTF-8") %>%
      mutate(
        COD_HDA_8       = toupper(trimws(COD_HDA_8)),
        Municipio_Excel = toupper(trimws(coalesce(Municipio_Excel, "SIN DATO"))),
        Correg_Excel    = toupper(trimws(coalesce(Correg_Excel,    "SIN DATO")))
      )
  } else {
    sin_georref_base <- data.frame(
      COD_HDA_8 = character(), Nombre_Reporte = character(),
      Ingenio = character(), Municipio_Excel = character(),
      Correg_Excel = character(), n_registros = integer(),
      n_suertes = integer(), Prioridad = character(),
      stringsAsFactors = FALSE
    )
  }
  
  if (file.exists(RUTA_COORDS)) {
    coords_manual <- read.csv(RUTA_COORDS,
                              stringsAsFactors = FALSE, encoding = "UTF-8") %>%
      mutate(COD_HDA_8 = toupper(trimws(COD_HDA_8)))
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
  
  # Cruzar con master_rds para obtener ciclo y semáforo
  # ing_hda en el RDS es sin guion bajo (ej: CA010685)
  # COD_HDA_8 en SIN_GEORREF también es sin guion bajo
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
        ES_ANCLA         = TRUE
      ) %>%
      st_as_sf(coords = c("LON", "LAT"), crs = 4326)
    
    # Alinear columnas faltantes antes del bind
    for (col in setdiff(names(DATOS_OFICIALES), c(names(DATOS_ANCLA), "geometry"))) {
      DATOS_ANCLA[[col]] <- NA
    }
    
    DATOS_OFICIALES <- bind_rows(DATOS_OFICIALES, DATOS_ANCLA)
    
    # Sacar del punto ciego las haciendas ya georreferenciadas
    SIN_GEORREF <- SIN_GEORREF %>% filter(Tiene_Coords == FALSE)
    
    message(paste("\U0001f4cd Ancla Operativa:", nrow(DATOS_ANCLA),
                  "haciendas integradas como puntos GPS al Radar Preventivo"))
  }
  
  message(paste("\U0001f50d Sin Georref:", nrow(SIN_GEORREF),
                "| Con coords:", sum(SIN_GEORREF$Tiene_Coords, na.rm = TRUE)))
  
}, error = function(e) { stop(paste("Error Fatal en Carga Global:", e)) })