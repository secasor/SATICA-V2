# ==============================================================================
# SCRIPT: R/generar_descargas_static.R
# Propósito: Pre-generar los archivos de descarga estáticos para el portal web:
#            1. Plantilla de Visitas (CSV)
#            2. Excel de Seguimiento (XLSX)
#            3. Capas GIS de Riesgo (KML con colores)
# ==============================================================================
library(dplyr)
library(sf)
library(openxlsx)

message("📥 Iniciando generación de descargas estáticas para el portal...")

# 1. Cargar base maestra y datos de visitas
if (!file.exists("data_master/SATICA_MASTER_v2.2.rds")) {
  stop("❌ No se encontró SATICA_MASTER_v2.2.rds. Ejecute primero el motor.")
}

master_rds <- readRDS("data_master/SATICA_MASTER_v2.2.rds")

# Helper de formato de tiempo
formatear_tiempo_g <- function(dias) {
  if (is.na(dias) || dias == 0) return("Sin Historial")
  if (dias < 60) return(paste(round(dias), "días"))
  meses <- floor(dias / 30.44); sobra <- round(dias %% 30.44)
  txt_m <- ifelse(meses == 1, "1 mes", paste(meses, "meses"))
  txt_d <- ifelse(sobra > 0, paste(sobra, "días"), "")
  return(trimws(paste(txt_m, txt_d)))
}

mapear_ingenio <- function(codigo) {
  if (is.null(codigo)) return("OTRO")
  codigo_clean <- toupper(trimws(as.character(codigo)))
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

# Cargar visitas
df_v <- NULL
if (file.exists("visitas_cvc.csv")) {
  df_v <- tryCatch({ read.csv("visitas_cvc.csv", stringsAsFactors = FALSE) %>% janitor::clean_names() }, error = function(e) NULL)
}

if(!is.null(df_v) && all(c("cod_hda_key", "fecha_visita") %in% names(df_v))) {
  datos_visitas <- df_v %>%
    mutate(
      COD_HDA_KEY = toupper(trimws(as.character(cod_hda_key))),
      FECHA_VISITA = as.Date(fecha_visita)
    ) %>%
    group_by(COD_HDA_KEY) %>%
    summarise(
      HISTORIAL_VISITAS = paste(sort(format(unique(FECHA_VISITA), "%Y-%m-%d")), collapse = " | "),
      FECHA_VISITA = max(FECHA_VISITA, na.rm = TRUE),
      RADICADO = if("radicado" %in% names(df_v)) { val <- trimws(as.character(last(radicado))); ifelse(is.na(val) | val == "", "S/N", val) } else "S/N",
      .groups = "drop"
    )
} else {
  datos_visitas <- data.frame(COD_HDA_KEY = character(), FECHA_VISITA = as.Date(character()), RADICADO = character(), HISTORIAL_VISITAS = character(), stringsAsFactors = FALSE)
}

# 2. Resumir a nivel de hacienda
DB_RIESGO <- master_rds %>%
  group_by(cod_hda_key) %>%
  summarise(
    TOTAL_HISTORICO = max(N_EVENTOS_HDA,      na.rm = TRUE),
    N_EVENTOS       = max(N_EVENTOS_HDA,      na.rm = TRUE),
    FECHA_ULT_I     = max(FECHA_ULT_I_HDA,    na.rm = TRUE),
    CICLO_DIAS      = mean(FRECUENCIA_HDA_DIAS,na.rm = TRUE),
    TXT_CICLO       = sapply(mean(FRECUENCIA_HDA_DIAS, na.rm = TRUE), formatear_tiempo_g),
    DIFF_MESES_VAL  = min(DIFF_HDA_MESES,      na.rm = TRUE),
    HDA_NOM         = first(na.omit(hda_nombre)),
    MUN_VAL         = first(na.omit(municipio)),
    CORREG_VAL      = first(na.omit(corregimiento)),
    INGENIO_VAL     = first(na.omit(mapear_ingenio(substr(cod_hda_key, 1, 2)))),
    LAT_VAL         = mean(as.numeric(lat), na.rm = TRUE),
    LON_VAL         = mean(as.numeric(lon), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    FECHA_ULT_I = if_else(is.infinite(FECHA_ULT_I), as.Date(NA), as.Date(FECHA_ULT_I))
  )

# Aplicar lógica de riesgos idéntica a server.R
fecha_hoy <- Sys.Date()
df_base <- DB_RIESGO %>%
  left_join(datos_visitas, by = c("cod_hda_key" = "COD_HDA_KEY")) %>%
  mutate(
    DIAS_DESDE_ULT = as.numeric(fecha_hoy - FECHA_ULT_I),
    ESTADO_RADAR = case_when(
      is.na(N_EVENTOS) | N_EVENTOS == 0 ~ "SIN_HISTORIAL",
      N_EVENTOS == 1 ~ "OCASIONAL",
      TRUE ~ "RECURRENTE"
    ),
    DIFF_MESES = if_else(ESTADO_RADAR == "RECURRENTE", (DIAS_DESDE_ULT - CICLO_DIAS) / 30.44, NA_real_),
    RIESGO = case_when(
      ESTADO_RADAR != "RECURRENTE" ~ "BAJO",
      DIFF_MESES < -3 ~ "BAJO",
      DIFF_MESES >= -3 & DIFF_MESES < -2 ~ "OBSERVACION",
      DIFF_MESES >= -2 & DIFF_MESES < -1 ~ "ALTO",
      DIFF_MESES >= -1 & DIFF_MESES <= 1 ~ "CRITICO",
      DIFF_MESES > 1 & DIFF_MESES <= 2 ~ "ALTO",
      DIFF_MESES > 2 & DIFF_MESES <= 3 ~ "OBSERVACION",
      DIFF_MESES > 3 ~ "MITIGADO",
      TRUE ~ "BAJO"
    ),
    COL = case_when(
      RIESGO == "BAJO" ~ "#27ae60",
      RIESGO == "OBSERVACION" ~ "#f1c40f",
      RIESGO == "ALTO" ~ "#e67e22",
      RIESGO == "CRITICO" ~ "#c0392b",
      RIESGO == "MITIGADO" ~ "#7f8c8d",
      TRUE ~ "#27ae60"
    ),
    TXT_ULTIMO_INCENDIO = ifelse(!is.na(FECHA_ULT_I), as.character(FECHA_ULT_I), "Sin Eventos"),
    VISITA_VALIDA = case_when(
      is.na(FECHA_VISITA) ~ FALSE,
      is.na(FECHA_ULT_I) ~ TRUE, 
      FECHA_VISITA >= FECHA_ULT_I ~ TRUE,
      TRUE ~ FALSE
    ),
    ESTADO_CONTROL = case_when(
      !VISITA_VALIDA ~ "Sin Intervencion",
      VISITA_VALIDA & RIESGO %in% c("ALTO", "CRITICO") ~ "Visitado",
      VISITA_VALIDA & (RIESGO == "BAJO" | RIESGO == "MITIGADO") & DIFF_MESES > 3 ~ "Incendio Evitado (Exito)",
      TRUE ~ "Visita Preventiva"
    ),
    TXT_AUDITORIA = case_when(
      ESTADO_CONTROL == "Visitado" & !is.na(RADICADO) & RADICADO != "S/N" ~ paste("Visitado (Rad:", RADICADO, ")"),
      TRUE ~ ESTADO_CONTROL
    ),
    PESO_RIESGO = case_when(RIESGO == "CRITICO" ~ 1, RIESGO == "ALTO" ~ 2, RIESGO == "OBSERVACION" ~ 3, TRUE ~ 4)
  ) %>%
  filter(INGENIO_VAL != "OTRO" & !is.na(LAT_VAL) & !is.na(LON_VAL))

# ==============================================================================
# A. PLANTILLA DE VISITAS (CSV)
# ==============================================================================
message("📊 Generando Plantilla de Visitas CSV...")
df_inminentes <- df_base %>% 
  filter(RIESGO %in% c("CRITICO", "ALTO")) %>%
  mutate(es_inminente = TRUE) %>% select(cod_hda_key, es_inminente)

df_foc_mun <- df_base %>%
  group_by(MUN_VAL) %>% arrange(PESO_RIESGO, desc(DIFF_MESES)) %>% slice_head(n = 10) %>% ungroup() %>%
  mutate(es_foc_mun = TRUE) %>% select(cod_hda_key, es_foc_mun)

df_top_reg <- df_base %>%
  arrange(PESO_RIESGO, desc(DIFF_MESES)) %>% slice_head(n = 10) %>%
  mutate(es_top_reg = TRUE) %>% select(cod_hda_key, es_top_reg)

df_keys <- bind_rows(df_inminentes %>% select(cod_hda_key), df_foc_mun %>% select(cod_hda_key), df_top_reg %>% select(cod_hda_key)) %>% distinct()

df_plantilla <- df_base %>%
  inner_join(df_keys, by = "cod_hda_key") %>%
  left_join(df_inminentes, by = "cod_hda_key") %>%
  left_join(df_foc_mun, by = "cod_hda_key") %>%
  left_join(df_top_reg, by = "cod_hda_key") %>%
  mutate(
    es_inminente = ifelse(is.na(es_inminente), FALSE, es_inminente),
    es_foc_mun = ifelse(is.na(es_foc_mun), FALSE, es_foc_mun),
    es_top_reg = ifelse(is.na(es_top_reg), FALSE, es_top_reg),
    cat_1 = ifelse(es_inminente, "Alerta Inminente", NA),
    cat_2 = ifelse(es_foc_mun, "Focalización Operativa", NA),
    cat_3 = ifelse(es_top_reg, "Top 10 Regional", NA)
  ) %>%
  rowwise() %>%
  mutate(CATEGORIA_ALERTA = paste(na.omit(c(cat_1, cat_2, cat_3)), collapse = " + ")) %>%
  ungroup() %>%
  select(COD_HDA_KEY = cod_hda_key, HDA_LABEL = HDA_NOM, INGENIO_FULL = INGENIO_VAL, MUNICIPIO = MUN_VAL, CORREGIMIENTO = CORREG_VAL, CATEGORIA_ALERTA) %>%
  mutate(FECHA_VISITA = "", RADICADO = "")

write.csv(df_plantilla, "data_master/Plantilla_Visitas_SATICA_latest.csv", row.names = FALSE, fileEncoding = "UTF-8")
message("✅ Plantilla de Visitas generada.")

# ==============================================================================
# A2. ACTUALIZAR DOCUMENTO MAESTRO ACUMULADO (visitas_cvc.csv)
# ==============================================================================
message("📊 Actualizando visitas_cvc.csv como documento maestro...")

# Determinar el número de boletín actual
fecha_hoy <- Sys.Date()
num_boletin_val <- (as.numeric(format(fecha_hoy, "%Y")) - 2026) * 12 + as.numeric(format(fecha_hoy, "%m")) + 1
num_boletin_str <- as.character(num_boletin_val)

# Cargar visitas existentes o inicializar
ruta_visitas_master <- "visitas_cvc.csv"
if (file.exists(ruta_visitas_master)) {
  df_visitas_old <- tryCatch({
    read.csv(ruta_visitas_master, stringsAsFactors = FALSE)
  }, error = function(e) {
    NULL
  })
  
  if (!is.null(df_visitas_old)) {
    colnames(df_visitas_old) <- tolower(colnames(df_visitas_old))
    required_cols <- c("cod_hda_key", "hda_label", "ingenio_full", "municipio", "corregimiento", "coordenadas", "categoria_alerta", "fecha_visita", "radicado", "boletin_n")
    for (col in required_cols) {
      if (!col %in% colnames(df_visitas_old)) {
        df_visitas_old[[col]] <- ""
      }
      df_visitas_old[[col]] <- as.character(df_visitas_old[[col]])
    }
    # Filtrar registros históricos "OTRO" o sin georreferenciación
    df_visitas_old <- df_visitas_old %>%
      filter(
        !is.na(coordenadas) & trimws(coordenadas) != "" & tolower(trimws(coordenadas)) != "sin coordenadas",
        !is.na(ingenio_full) & tolower(trimws(ingenio_full)) != "otro"
      )
  } else {
    df_visitas_old <- data.frame(
      cod_hda_key = character(), hda_label = character(), ingenio_full = character(),
      municipio = character(), corregimiento = character(), coordenadas = character(),
      categoria_alerta = character(), fecha_visita = character(), radicado = character(),
      boletin_n = character(), stringsAsFactors = FALSE
    )
  }
} else {
  df_visitas_old <- data.frame(
    cod_hda_key = character(), hda_label = character(), ingenio_full = character(),
    municipio = character(), corregimiento = character(), coordenadas = character(),
    categoria_alerta = character(), fecha_visita = character(), radicado = character(),
    boletin_n = character(), stringsAsFactors = FALSE
  )
}

df_visitas_old$cod_hda_key <- toupper(trimws(as.character(df_visitas_old$cod_hda_key)))

# Construir la lista de recomendadas en el boletín actual con coordenadas y metadatos
df_recs_actual <- df_base %>%
  inner_join(df_keys, by = "cod_hda_key") %>%
  left_join(df_inminentes, by = "cod_hda_key") %>%
  left_join(df_foc_mun, by = "cod_hda_key") %>%
  left_join(df_top_reg, by = "cod_hda_key") %>%
  mutate(
    es_inminente = ifelse(is.na(es_inminente), FALSE, es_inminente),
    es_foc_mun = ifelse(is.na(es_foc_mun), FALSE, es_foc_mun),
    es_top_reg = ifelse(is.na(es_top_reg), FALSE, es_top_reg),
    cat_1 = ifelse(es_inminente, "Alerta Inminente", NA),
    cat_2 = ifelse(es_foc_mun, "Focalización Operativa", NA),
    cat_3 = ifelse(es_top_reg, "Top 10 Regional", NA)
  ) %>%
  rowwise() %>%
  mutate(CATEGORIA_ALERTA = paste(na.omit(c(cat_1, cat_2, cat_3)), collapse = " + ")) %>%
  ungroup() %>%
  mutate(
    coordenadas = ifelse(!is.na(LAT_VAL) & !is.na(LON_VAL), paste(round(LAT_VAL, 4), round(LON_VAL, 4), sep = ", "), "Sin Coordenadas")
  ) %>%
  transmute(
    cod_hda_key = toupper(trimws(as.character(cod_hda_key))),
    hda_label = HDA_NOM,
    ingenio_full = INGENIO_VAL,
    municipio = MUN_VAL,
    corregimiento = CORREG_VAL,
    coordenadas = coordenadas,
    categoria_alerta = CATEGORIA_ALERTA,
    fecha_visita = "",
    radicado = "",
    boletin_n = num_boletin_str
  ) %>%
  filter(
    coordenadas != "Sin Coordenadas",
    ingenio_full != "OTRO"
  )

# Mezclar inteligentemente preservando datos del usuario
keys_old <- df_visitas_old$cod_hda_key
df_visitas_updated <- df_visitas_old

for (i in seq_len(nrow(df_recs_actual))) {
  new_row <- df_recs_actual[i, ]
  key <- new_row$cod_hda_key
  
  if (key %in% keys_old) {
    idx <- which(keys_old == key)
    # Actualizar metadatos del predio
    df_visitas_updated$hda_label[idx] <- new_row$hda_label
    df_visitas_updated$ingenio_full[idx] <- new_row$ingenio_full
    df_visitas_updated$municipio[idx] <- new_row$municipio
    df_visitas_updated$corregimiento[idx] <- new_row$corregimiento
    df_visitas_updated$coordenadas[idx] <- new_row$coordenadas
    df_visitas_updated$categoria_alerta[idx] <- new_row$categoria_alerta
    
    # Adicionar el boletín actual a la lista
    old_b <- trimws(strsplit(as.character(df_visitas_updated$boletin_n[idx]), ",")[[1]])
    old_b <- old_b[old_b != "" & !is.na(old_b)]
    if (!num_boletin_str %in% old_b) {
      new_b <- c(old_b, num_boletin_str)
      df_visitas_updated$boletin_n[idx] <- paste(new_b, collapse = ", ")
    }
  } else {
    # Agregar como nueva fila
    df_visitas_updated <- bind_rows(df_visitas_updated, new_row)
  }
}

# Guardar el archivo visitas_cvc.csv con UTF-8
write.csv(df_visitas_updated, ruta_visitas_master, row.names = FALSE, fileEncoding = "UTF-8")
message("✅ visitas_cvc.csv (documento maestro) actualizado con éxito.")

# ==============================================================================
# B. EXCEL DE SEGUIMIENTO (XLSX)
# ==============================================================================
message("📊 Generando Excel de Seguimiento...")
datos_quincena <- df_base %>% 
  filter(RIESGO %in% c("CRITICO", "ALTO")) %>% 
  arrange(PESO_RIESGO, desc(DIFF_MESES)) %>%
  mutate(
    FECH_EST = as.Date(FECHA_ULT_I) + CICLO_DIAS,
    DIAS_FALTANTES = as.numeric(FECH_EST - Sys.Date()),
    TXT_ESTIMADO = ifelse(!is.na(FECH_EST), as.character(FECH_EST), "N/A")
  ) %>%
  filter(DIAS_FALTANTES >= -15 & DIAS_FALTANTES <= 15) %>%
  arrange(MUN_VAL) %>%
  select(Municipio = MUN_VAL, Hacienda = HDA_NOM, Corregimiento = CORREG_VAL, Ingenio = INGENIO_VAL, `Ultimo Evento` = TXT_ULTIMO_INCENDIO, `Ciclo Estimado` = TXT_ESTIMADO, `Historico` = TOTAL_HISTORICO, `Gestion CVC` = TXT_AUDITORIA) %>%
  mutate(`Funcionario Responsable` = "", Radicado = "")

top_municipios <- df_base %>%
  group_by(MUN_VAL) %>%
  slice_max(order_by = TOTAL_HISTORICO, n = 10, with_ties = FALSE) %>%
  arrange(MUN_VAL, desc(TOTAL_HISTORICO)) %>%
  ungroup() %>%
  select(Municipio = MUN_VAL, Hacienda = HDA_NOM, Corregimiento = CORREG_VAL, Ingenio = INGENIO_VAL, Riesgo = RIESGO, `Ultimo Evento` = TXT_ULTIMO_INCENDIO, Historico = TOTAL_HISTORICO, `Gestion CVC` = TXT_AUDITORIA) %>%
  mutate(`Funcionario Responsable` = "", Radicado = "")

ficha_maestra <- df_base %>%
  slice_max(order_by = TOTAL_HISTORICO, n = 10, with_ties = FALSE) %>%
  arrange(desc(TOTAL_HISTORICO)) %>%
  select(Municipio = MUN_VAL, Hacienda = HDA_NOM, Corregimiento = CORREG_VAL, Ingenio = INGENIO_VAL, Nivel = RIESGO, `Ultimo Evento` = TXT_ULTIMO_INCENDIO, `Historico` = TOTAL_HISTORICO, `Gestion CVC` = TXT_AUDITORIA) %>%
  mutate(`Funcionario Responsable` = "", Radicado = "")

openxlsx::write.xlsx(list(
  "Cronograma Inminentes" = datos_quincena,
  "Top 10 Municipio" = top_municipios,
  "Ficha Tecnica DAR" = ficha_maestra
), "data_master/SATICA_Seguimiento_latest.xlsx")
message("✅ Excel de Seguimiento generado.")

# ==============================================================================
# C. CAPA DE RIESGO GIS (KML)
# ==============================================================================
message("🗺️ Generando Capa KML...")
if (file.exists("data_estatica/GEO_CACHE_SATICA.rds")) {
  DATOS_ESPACIALES_BASE <- readRDS("data_estatica/GEO_CACHE_SATICA.rds")
  
  poligonos_gis <- DATOS_ESPACIALES_BASE %>%
    left_join(df_base, by = c("COD_HDA_KEY" = "cod_hda_key")) %>%
    filter(RIESGO %in% c("CRITICO", "ALTO")) %>%
    mutate(
      RIESGO_GIS = as.character(RIESGO),
      NOM_PRED   = substr(HDA_LABEL, 1, 100),
      INGENIO    = INGENIO_VAL,
      MUN        = MUN_VAL,
      TIPO       = ifelse(RIESGO == "CRITICO", "INMINENTE", "ALTO"),
      FECHA_EST  = as.character(suppressWarnings(as.Date(FECHA_ULT_I + CICLO_DIAS)))
    ) %>%
    select(NOM_PRED, INGENIO, MUN, RIESGO_GIS, TIPO, FECHA_EST)
  
  kml_tmp <- tempfile(fileext = ".kml")
  sf::st_write(poligonos_gis, dsn = kml_tmp, driver = "KML", delete_dsn = TRUE, quiet = TRUE)
  
  kml_lines <- readLines(kml_tmp, warn = FALSE, encoding = "UTF-8")
  
  style_block <- c(
    '  <Style id="CRITICO">',
    '    <LineStyle><color>ff0a143c</color><width>1.5</width></LineStyle>',
    '    <PolyStyle><color>d92b39c0</color><fill>1</fill><outline>1</outline></PolyStyle>',
    '  </Style>',
    '  <Style id="ALTO">',
    '    <LineStyle><color>ff0032c8</color><width>1.5</width></LineStyle>',
    '    <PolyStyle><color>d9227ee6</color><fill>1</fill><outline>1</outline></PolyStyle>',
    '  </Style>'
  )
  
  new_lines   <- character(0)
  cur_riesgo  <- NA_character_
  styles_done <- FALSE
  
  for (ln in kml_lines) {
    if (!styles_done && grepl("<Folder>", ln, fixed = TRUE)) {
      new_lines   <- c(new_lines, style_block, ln)
      styles_done <- TRUE
      next
    }
    if (grepl("<Placemark>", ln, fixed = TRUE)) cur_riesgo <- NA_character_
    if (grepl('name="RIESGO_GIS"', ln, fixed = TRUE)) {
      m <- regmatches(ln, regexpr('(?<=<SimpleData name="RIESGO_GIS">)[^<]+', ln, perl = TRUE))
      if (length(m) > 0) cur_riesgo <- trimws(m[1])
    }
    if (!is.na(cur_riesgo) && grepl("<Polygon>|<MultiGeometry>", ln)) {
      sid       <- if (cur_riesgo == "CRITICO") "CRITICO" else "ALTO"
      new_lines <- c(new_lines, paste0("    <styleUrl>#", sid, "</styleUrl>"))
      cur_riesgo <- NA_character_
    }
    new_lines <- c(new_lines, ln)
  }
  
  writeLines(new_lines, "data_master/SATICA_GIS_latest.kml", useBytes = FALSE)
  message("✅ Capa KML generada.")
} else {
  message("⚠️ No se encontró data_estatica/GEO_CACHE_SATICA.rds. Saltando generación de KML.")
}

message("🏁 Generación de archivos estáticos finalizada con éxito.")
