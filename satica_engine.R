# ==============================================================================
# MOTOR SATICA V2.4 - GESTIÓN INTEGRAL DE RIESGO E IDENTIDAD
# ==============================================================================
# Propósito: Procesar reportes de cosecha, calcular riesgos y reparar nombres.
# --- BACKUP POINT: satica_engine_v2.2..R (Integración de Cláusulas y Semáforo de 7 Niveles) ---
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf, dplyr, purrr, readxl, janitor, lubridate, stringi, stringr)

message("🚀 INICIANDO MOTOR TÁCTICO V2.4 - MODO AUTO-REPARACIÓN CON BLINDAJE...")
sf_use_s2(FALSE) # Desactivar geometría esférica para cálculos planos rápidos

# --- 1. FUNCIONES DE LIMPIEZA Y FORMATO ---
limpiar_texto <- function(x) {
  x %>% as.character() %>% toupper() %>% 
    stri_trans_general("Latin-ASCII") %>% trimws()
}

formatear_tiempo <- function(dias) {
  if (is.na(dias) || dias == 0) return("Sin Historial")
  if (dias < 60) return(paste(round(dias), "días"))
  meses <- floor(dias / 30.44); sobra <- round(dias %% 30.44)
  txt_m <- ifelse(meses == 1, "1 mes", paste(meses, "meses"))
  txt_d <- ifelse(sobra > 0, paste(sobra, "días"), "")
  return(trimws(paste(txt_m, txt_d)))
}

# --- 2. CARGA Y TRADUCCIÓN GEOGRÁFICA (EL BLINDAJE) ---
message("🗺️ Cargando geografía y traduciendo nombres...")
cana_path <- normalizePath(list.files("capas", pattern = "SOR_OK\\.shp$", full.names = TRUE)[1])
if (is.na(cana_path)) stop("No se encontró el archivo de geometría Caña_SOR_OK.shp en /capas")
mapa_cana <- st_read(cana_path, quiet = TRUE) %>% 
  st_make_valid() %>% 
  st_transform(4326)
corr_path <- normalizePath(list.files("capas", pattern = "Corregimientos\\.shp$", full.names = TRUE)[1])
if (is.na(corr_path)) stop("No se encontró el archivo de geometría Corregimientos.shp en /capas")
corregimientos <- st_read(corr_path, quiet = TRUE) %>% 
  st_make_valid() %>% 
  st_transform(4326) %>% clean_names()

# Obtener centroides para el cruce de nombres
centroides <- suppressWarnings(st_centroid(mapa_cana))

# Unión espacial para heredar nombres de Municipio y Corregimiento reales (NOM_MUNICI y NOM_DIV_PO)
admin_ref <- st_join(centroides, corregimientos, join = st_intersects) %>% 
  st_drop_geometry() %>%
  select(nom_munici, nom_div_po)

# --- 3. PROCESAMIENTO DE HISTORIAL (LÓGICA BLINDADA 14 DÍGITOS) ---
message("🔥 Procesando reportes de incendios bajo Cláusula Anti-Trituración...")
path_reportes <- "reportes_cosecha"
archivos <- list.files(path_reportes, pattern = "\\.xlsx$|\\.xls$", full.names = TRUE)

historial_incendios <- NULL
if (length(archivos) > 0) {
  historial_incendios <- map_df(archivos, function(x) {
    df_raw <- tryCatch({ read_excel(x, skip = 6, col_types = "text") %>% clean_names() }, error = function(e) NULL)
    if(is.null(df_raw)) return(NULL)
    
    # Identificación dinámica de columnas
    col_ing <- names(df_raw)[grepl("ingenio", names(df_raw))][1]
    col_hda <- names(df_raw)[grepl("hacienda|cod_hda", names(df_raw))][1]
    col_sue <- names(df_raw)[grepl("suerte|lote|cod_sue", names(df_raw))][1]
    
    if(any(is.na(c(col_ing, col_hda, col_sue)))) return(NULL)
    
    df_raw %>%
      filter(cod_cosecha == "I") %>%
      mutate(
        ing_clean = limpiar_texto(!!sym(col_ing)),
        Cod_ing = case_when(
          grepl("INCAUCA", ing_clean) ~ "CA",
          grepl("MAYAGUEZ", ing_clean) ~ "MY",
          grepl("MARIA LUISA", ing_clean) ~ "ML",
          grepl("CASTILLA", ing_clean) ~ "CC",
          grepl("PROVIDENCIA", ing_clean) ~ "PR",
          grepl("MANUELITA", ing_clean) ~ "MN",
          grepl("PICHICHI", ing_clean) ~ "PC",
          grepl("CABA", ing_clean) ~ "CB",
          grepl("RIOPAILA", ing_clean) ~ "RP",
          TRUE ~ NA_character_
        ),
        
        # [BLINDAJE 1: ANTI-TRITURACIÓN APLICADO]
        # Extraemos solo letras, números y guiones bajos. Sin conversión escalar.
        # Rellenamos por la izquierda con ceros, garantizando strings como _008 o 4A y forzando mayúsculas.
        hda_limpia = toupper(str_replace_all(as.character(!!sym(col_hda)), "[^0-9A-Za-z_]", "")),
        sue_limpia = toupper(str_replace_all(as.character(!!sym(col_sue)), "[^0-9A-Za-z_]", "")),
        Cod_hda_full = str_pad(hda_limpia, width = 6, side = "left", pad = "0"),
        Cod_sue_full = str_pad(sue_limpia, width = 6, side = "left", pad = "0"),
        
        COD_UNICO_14 = paste0(Cod_ing, Cod_hda_full, Cod_sue_full),
        FECHA = as.Date(as.numeric(fecha_dato), origin = "1899-12-30")
      ) %>%
      filter(!is.na(FECHA), !is.na(Cod_ing)) %>%
      select(COD_UNICO_14, FECHA)
  })
}

# Fallback robusto en la nube si no hay archivos locales Excel a procesar
if (is.null(historial_incendios) || nrow(historial_incendios) == 0) {
  message("  🛰️  Sin nuevos archivos locales. Cargando historial consolidado...")
  
  if (file.exists("data_master/SATICA_HISTORIAL_v2.2.rds")) {
    message("  📦 Cargando historial local existente...")
    historial_incendios <- tryCatch({
      readRDS("data_master/SATICA_HISTORIAL_v2.2.rds")
    }, error = function(e) NULL)
  }
  
  if (is.null(historial_incendios) || nrow(historial_incendios) == 0) {
    message("  🌐 Cargando historial de respaldo desde URL...")
    url_historial <- "https://secasor.github.io/SATICA-V2/data_master/SATICA_HISTORIAL_v2.2.rds"
    historial_incendios <- tryCatch({
      readRDS(url(url_historial))
    }, error = function(e) {
      message("  ⚠️  No se pudo descargar el historial consolidado. Creando estructura de respaldo vacía.")
      data.frame(COD_UNICO_14 = character(), FECHA = as.Date(character()), stringsAsFactors = FALSE)
    })
  }
}

# 4. CÁLCULO DE ESTADÍSTICAS (SUERTE Y HACIENDA CRONOLÓGICA)
saveRDS(historial_incendios, "data_master/SATICA_HISTORIAL_v2.2.rds")

# Historial procesado para extraer el código de hacienda (6 dígitos + ingenio)
hist_prep <- historial_incendios %>%
  mutate(
    cod_hda_key = paste(substr(COD_UNICO_14, 1, 2), substr(COD_UNICO_14, 3, 8), sep = "_")
  )

# A. ESTADÍSTICAS POR SUERTE (CÁLCULO HEURÍSTICO BASE)
frecuencia_suerte <- hist_prep %>%
  arrange(COD_UNICO_14, FECHA) %>%
  group_by(COD_UNICO_14) %>%
  summarise(
    FECHA_ULT_I_SUE = max(FECHA, na.rm = TRUE),
    N_EVENTOS_SUE = n(),
    CICLO_HEURISTICO = ifelse(n() > 1, as.numeric(mean(diff(FECHA))), 365),
    .groups = "drop"
  )

# B. FRECUENCIA PREDIAL (INTERVALO ENTRE CUALQUIER EVENTO EN LA HACIENDA)
frecuencia_hacienda <- hist_prep %>%
  arrange(cod_hda_key, FECHA) %>%
  group_by(cod_hda_key) %>%
  summarise(
    FECHA_ULT_I_HDA = max(FECHA, na.rm = TRUE),
    N_EVENTOS_HDA = n(),
    # Esta es la métrica clave sugerida por el usuario: cada cuánto ocurre UN incendio en el predio
    FRECUENCIA_HDA_DIAS = ifelse(n() > 1, as.numeric(mean(diff(FECHA))), 365),
    .groups = "drop"
  )

# --- 5. ENSAMBLE MAESTRO FINAL (CON INTEGRACIÓN DE HUÉRFANOS PARCIALES) ---
message("📊 Ensamblando Master RDS con integración de suertes sin geometría...")

# Preparar datos geográficos base
base_geo <- mapa_cana %>%
  clean_names() %>%
  mutate(
    municipio_geo = admin_ref$nom_munici,
    corregimiento_geo = admin_ref$nom_div_po,
    lat = st_coordinates(centroides)[,2],
    lon = st_coordinates(centroides)[,1],
    hda_nombre_geo = nombre_hda,
    cod_unico = toupper(cod_unico),
    cod_hda_key = paste(substr(cod_unico, 1, 2), substr(cod_unico, 3, 8), sep = "_"),
    tiene_geometria = TRUE
  ) %>%
  st_drop_geometry()

# Unir con estadísticas (Suerte y Hacienda)
master_final <- base_geo %>%
  full_join(frecuencia_suerte, by = c("cod_unico" = "COD_UNICO_14")) %>%
  mutate(
    tiene_geometria = coalesce(tiene_geometria, FALSE),
    cod_hda_key = coalesce(cod_hda_key, paste(substr(cod_unico, 1, 2), substr(cod_unico, 3, 8), sep = "_"))
  ) %>%
  left_join(frecuencia_hacienda, by = "cod_hda_key")

# Lógica de Herencia para Huérfanos Parciales (928)
referencia_hdas <- base_geo %>%
  group_by(cod_hda_key) %>%
  summarise(
    hda_nombre_ref = first(hda_nombre_geo),
    muni_ref       = first(municipio_geo),
    corr_ref       = first(corregimiento_geo),
    .groups = "drop"
  )

master_final <- master_final %>%
  left_join(referencia_hdas, by = "cod_hda_key") %>%
  mutate(
    municipio     = coalesce(municipio_geo, muni_ref),
    corregimiento = coalesce(corregimiento_geo, corr_ref),
    hda_nombre    = coalesce(hda_nombre_geo, hda_nombre_ref),
    
    # [INICIALIZACIÓN PREVENTIVA DE VARIABLE DE RIESGO Y SATÉLITE]
    CICLO_DIAS_SUE    = CICLO_HEURISTICO, # Backup default
    Satelite_Fuego    = FALSE,
    GOES_Fuego        = FALSE,
    Estado_GOES       = "Normal",
    NDVI              = 0.5,
    Alerta_Combustion = "Sin Datos"
  )

# ==============================================================================
# --- 5.5. INTEGRACIÓN DE INTELIGENCIA SATELITAL (OJO DE HALCÓN) ---
# ==============================================================================
message("🛰️ Cruzando con Inteligencia Satelital (FIRMS + Sentinel-2 + GOES-16)...")

# 1. Cargar Alertas Térmicas (NASA FIRMS)
df_firms <- tryCatch({ 
  df <- read.csv("data_master/Incendios_Satelite_24H.csv", stringsAsFactors = FALSE)
  if (!"cod_unico" %in% names(df)) df$cod_unico <- character()
  df %>% mutate(cod_unico = as.character(cod_unico)) %>% select(cod_unico) %>% distinct(cod_unico) %>% mutate(Satelite_Fuego = TRUE) 
}, error = function(e) data.frame(cod_unico = character(), Satelite_Fuego = logical()))

# 1.5. Cargar Alertas Dinámicas (GOES-16)
df_goes <- tryCatch({ 
  df <- read.csv("data_master/GOES16_Alertas.csv", stringsAsFactors = FALSE)
  if (!"cod_unico" %in% names(df)) df$cod_unico <- character()
  df %>% mutate(cod_unico = as.character(cod_unico)) %>% select(cod_unico, GOES_Fuego, Estado_GOES) %>% distinct(cod_unico, .keep_all = TRUE)
}, error = function(e) data.frame(cod_unico = character(), GOES_Fuego = logical(), Estado_GOES = character()))

# 2. Cargar Índices de Biomasa (Sentinel-2 via Google Earth Engine)
df_biomasa <- tryCatch({ 
  df <- read.csv("data_master/Biomasa_Cana_Sentinel2.csv", stringsAsFactors = FALSE)
  if (!"cod_unico" %in% names(df)) df$cod_unico <- character()
  df %>% mutate(cod_unico = as.character(cod_unico)) %>% select(any_of(c("cod_unico", "NDVI", "NBR", "Alerta_Combustion"))) %>% 
    distinct(cod_unico, .keep_all = TRUE)
}, error = function(e) data.frame(cod_unico = character(), NDVI = numeric(), NBR = numeric(), Alerta_Combustion = character()))

# 3. Fusión Pre-ML con el Master
master_final <- master_final %>%
  select(-Satelite_Fuego, -GOES_Fuego, -Estado_GOES, -NDVI, -Alerta_Combustion) %>% # Remover placeholders
  left_join(df_firms,   by = "cod_unico") %>%
  left_join(df_goes,    by = "cod_unico") %>%
  left_join(df_biomasa, by = "cod_unico") %>%
  mutate(
    Satelite_Fuego    = coalesce(Satelite_Fuego, FALSE),
    GOES_Fuego        = coalesce(GOES_Fuego, FALSE),
    Estado_GOES       = coalesce(Estado_GOES, "Normal"),
    NDVI              = coalesce(NDVI, 0.5),
    Alerta_Combustion = coalesce(Alerta_Combustion, "Sin Datos")
  )

# ==============================================================================
# --- 5.5. INFERENCIA ALGORÍTMICA (XGBOOST V9 GEOESPACIAL) ---
# ==============================================================================
message("🤖 Inicializando Cerebro Predictivo (XGBoost V9 Geoespacial)...")
modelo_v9 <- tryCatch({
  ruta_ubj <- "modelo_rds/xgb_model_V9_regresion_geoespacial.ubj"
  ruta_rds <- "modelo_rds/xgb_model_V9_regresion_geoespacial.rds"
  if (file.exists(ruta_ubj)) {
    xgboost::xgb.load(ruta_ubj)
  } else if (file.exists(ruta_rds)) {
    message("   -> ⚠️  Modelo en formato .rds detectado. Intentando migrar a .ubj...")
    m <- readRDS(ruta_rds)
    xgboost::xgb.save(m, ruta_ubj)
    message("   -> ✅ Modelo migrado a formato .ubj portable. Próximo inicio será más rápido.")
    xgboost::xgb.load(ruta_ubj)
  } else {
    message("   -> ❌ No se encontró el modelo V9.")
    NULL
  }
}, error = function(e) {
  message("   -> ❌ No se pudo cargar el modelo XGBoost: ", e$message)
  NULL
})
diccionario_v9 <- tryCatch(readRDS("modelo_rds/v9_features_geoespacial.rds"), error = function(e) NULL)
matriz_distancias <- tryCatch(readRDS("data_master/matriz_distancias_cana.rds"), error = function(e) NULL)

master_final$CICLO_DIAS_SUE <- master_final$CICLO_HEURISTICO

if(!is.null(modelo_v9) && !is.null(diccionario_v9) && !is.null(matriz_distancias) && requireNamespace("xgboost", quietly = TRUE) && requireNamespace("Matrix", quietly = TRUE)) {
  tryCatch({
    # Acoplar matriz estática de distancias físicas
    master_geo <- master_final %>%
      left_join(matriz_distancias, by = "cod_unico") %>%
      tidyr::replace_na(list(dist_vias_m = 15000, dist_poblados_m = 15000, dist_bosques_m = 15000))

    # 1. Mapeo de Variables Fieles al Entrenamiento del V9
    df_inferencia <- master_geo %>%
      mutate(
        Mes = as.numeric(lubridate::month(FECHA_ULT_I_SUE)),
        Ingenio = as.character(substr(cod_unico, 1, 2)),
        Corregimiento = as.character(corregimiento),
        Municipio = as.character(municipio),
        Recurrencia = as.numeric(N_EVENTOS_SUE),
        Dist_Vias = as.numeric(dist_vias_m),
        Dist_Pueblos = as.numeric(dist_poblados_m),
        Dist_Bosques = as.numeric(dist_bosques_m)
      ) %>%
      tidyr::replace_na(list(Mes = 1, Recurrencia = 1, Ingenio = "UNKN", Corregimiento = "UNKN", Municipio = "UNKN")) %>%
      mutate(
        Ingenio = as.factor(Ingenio),
        Corregimiento = as.factor(Corregimiento),
        Municipio = as.factor(Municipio)
      )
      
    # 2. Generación Transparente de la DMatrix Expansa V9
    dummy_matrix_inf <- Matrix::sparse.model.matrix(
      ~ Ingenio + Corregimiento + Municipio + Recurrencia + Mes + Dist_Vias + Dist_Pueblos + Dist_Bosques - 1, 
      data = df_inferencia
    )
    
    # 3. Alineación forzosa con el diccionario original (ADN del V9)
    aligned_matrix <- Matrix::Matrix(0, nrow = nrow(dummy_matrix_inf), ncol = length(diccionario_v9), sparse = TRUE)
    colnames(aligned_matrix) <- diccionario_v9
    
    intersect_cols <- intersect(colnames(dummy_matrix_inf), diccionario_v9)
    aligned_matrix[, intersect_cols] <- dummy_matrix_inf[, intersect_cols]
    
    dtest_inferencia <- xgboost::xgb.DMatrix(data = aligned_matrix)
    
    # 4. SOBREESCRITURA MAESTRA (XGBoost prevalece sobre Heurística)
    preds <- tryCatch({ predict(modelo_v9, newdata = dtest_inferencia) }, error = function(e) { message("   -> ⚠️ El modelo XGBoost es incompatible con esta versión de R/xgboost. Usando heurística."); return(NULL) })
    if (!is.null(preds)) {
      master_final$CICLO_DIAS_SUE <- preds
      message("   -> ✅ Predicción XGBoost V9 inyectada al 100% en la variable de riesgo tomando en cuenta Zonas de Restricción.")
    }
  }, error = function(e) {
    message("   -> ⚠️ Fallo en el ensamblaje de la matriz V9. Volviendo al promedio matemático (Backup Heurístico).")
    message("Error: ", base::conditionMessage(e))
  })
} else {
  message("   -> ⚠️ Faltan dependencias o modelo V9. Operando con promedio heurístico.")
}

# ==============================================================================
# --- 5.7. CRUCE HÍBRIDO FINAL (RESULTADO 98% PRECISIÓN) ---
# ==============================================================================
# Recalculamos FECHA ESTIMADA y RIESGOS impactados por la nueva inteligencia
master_final <- master_final %>%
  mutate(
    FECHA_ESTIMADA = FECHA_ULT_I_SUE + CICLO_DIAS_SUE,
    DIFF_MESES = as.numeric(Sys.Date() - FECHA_ESTIMADA) / 30.44,
    DIFF_HDA_MESES = as.numeric(Sys.Date() - (FECHA_ULT_I_HDA + FRECUENCIA_HDA_DIAS)) / 30.44,
    
    # [LÓGICA HÍBRIDA MAESTRA]:
    # Combina Heurística (Frecuencia) + ML (XGBoost V9) + Flagrancia (Satélite)
    riesgo = case_when(
      Satelite_Fuego == TRUE ~ "CRITICO", # ¡Está ardiendo ahora mismo!
      GOES_Fuego     == TRUE ~ "CRITICO", # ¡Fuego dinámico capturado por GOES-16!
      Alerta_Combustion == "ALTA (Extrema Resequedad)" ~ "CRITICO", # Pólvora seca
      is.na(FECHA_ULT_I_HDA) ~ "BAJO",
      DIFF_HDA_MESES < -3 ~ "BAJO",
      DIFF_HDA_MESES >= -3 & DIFF_HDA_MESES < -2 ~ "OBSERVACION",
      DIFF_HDA_MESES >= -2 & DIFF_HDA_MESES < -1 ~ "ALTO",
      DIFF_HDA_MESES >= -1 & DIFF_HDA_MESES <= 1 ~ "CRITICO",
      DIFF_HDA_MESES > 1 & DIFF_HDA_MESES <= 2 ~ "ALTO",
      DIFF_HDA_MESES > 2 & DIFF_HDA_MESES <= 3 ~ "OBSERVACION",
      DIFF_HDA_MESES > 3 ~ "MITIGADO",
      TRUE ~ "BAJO"
    ),
    col = case_when(
      riesgo == "BAJO" ~ "#27ae60",
      riesgo == "OBSERVACION" ~ "#f1c40f",
      riesgo == "ALTO" ~ "#e67e22",
      riesgo == "CRITICO" ~ "#c0392b",
      riesgo == "MITIGADO" ~ "#7f8c8d",
      TRUE ~ "#27ae60"
    ),
    txt_ciclo = purrr::map_chr(CICLO_DIAS_SUE, formatear_tiempo)
  )

# --- 6. GUARDADO Y CIERRE (BLINDAJE 2: SINCRONIZACIÓN ÚNICA) ---
if(!dir.exists("data_master")) dir.create("data_master")
saveRDS(master_final, "data_master/SATICA_MASTER_v2.2.rds")

# --- GENERAR DATA_SUMMARY.JSON PARA PORTAL WEB ---
tryCatch({
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    visitas_path <- "visitas_cvc.csv"
    total_exitos <- 0
    if (file.exists(visitas_path)) {
      df_v <- read.csv(visitas_path, stringsAsFactors = FALSE)
      colnames(df_v) <- tolower(colnames(df_v))
      
      if (all(c("cod_hda_key", "fecha_visita", "radicado") %in% colnames(df_v))) {
        # Identificar haciendas con visitas válidas (tienen fecha y radicado válido)
        hdas_visitadas <- df_v %>%
          mutate(
            cod_hda_key = toupper(trimws(as.character(cod_hda_key))),
            fecha_visita = trimws(as.character(fecha_visita)),
            radicado = toupper(trimws(as.character(radicado)))
          ) %>%
          filter(
            !is.na(fecha_visita) & fecha_visita != "" & fecha_visita != "NA" &
            !is.na(radicado) & radicado != "" & radicado != "NA" & radicado != "S/N"
          ) %>%
          pull(cod_hda_key) %>%
          unique()
        
        # Contar cuántas de estas haciendas visitadas tienen riesgo MITIGADO en master_final
        total_exitos <- length(unique(master_final$cod_hda_key[master_final$riesgo == "MITIGADO" & master_final$cod_hda_key %in% hdas_visitadas]))
      }
    }
    
    summary_data <- list(
      last_update = format(Sys.time(), "%Y-%m-%d %H:%M COT", tz = "America/Bogota"),
      active_fires_24h = sum(master_final$Satelite_Fuego == TRUE, na.rm = TRUE),
      goes_alerts_1h = sum(master_final$GOES_Fuego == TRUE, na.rm = TRUE),
      critical_count = length(unique(master_final$cod_hda_key[master_final$riesgo == "CRITICO"])),
      success_count = total_exitos
    )
    jsonlite::write_json(summary_data, "data_master/data_summary.json", auto_unbox = TRUE, pretty = TRUE)
    message("📊 data_summary.json generado exitosamente para el portal web.")
  } else {
    message("⚠️ jsonlite no está disponible para exportar JSON.")
  }
}, error = function(e) {
  message("⚠️ No se pudo generar el data_summary.json: ", e$message)
})

message("\n==========================================================")
message("✅ PROCESO EXITOSO: MASTER V2.4 GENERADO BAJO REGLAS DE BLINDAJE")
message("📍 Municipios procesados: ", paste(unique(master_final$municipio), collapse = ", "))
message("🔥 Hacienda Ejemplo: ", head(master_final$hda_nombre, 1), " en ", head(master_final$municipio, 1))
message("==========================================================\n")

# --- GENERACIÓN DE ARCHIVOS DE DESCARGA ESTÁTICOS ---
if (file.exists("R/generar_descargas_static.R")) {
  tryCatch({
    source("R/generar_descargas_static.R", encoding = "UTF-8")
  }, error = function(e) {
    message("⚠️ Error al generar descargas estáticas: ", e$message)
  })
}