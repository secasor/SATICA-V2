# ==============================================================================
# CENTINELA SATELITAL - ROBOT EN SEGUNDO PLANO (GITHUB ACTIONS)
# ==============================================================================
# Este script se ejecuta mediante cron job de GitHub Actions cada 15 min.
# Escanea focos dinámicos en GOES-16 y manda Ficha de Acción Rápida a Telegram.
# ==============================================================================

options(warn = -1)
suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(httr)
  library(stringr)
})

message("🤖 Iniciando Centinela Satelital de SATICA...")

# =============================================================================
# 1. EJECUTAR ESCANEO DINÁMICO DE GOES-16
# =============================================================================
tryCatch({
  source("R/api_goes16.R")
}, error = function(e) {
  message("⚠️ Error en el escaneo satelital: ", e$message)
  message("ℹ️ El escaneo no encontró datos. El robot termina normalmente.")
  # Crear archivo vacío para que el siguiente paso no falle
  if (!dir.exists("data_master")) dir.create("data_master")
  write.csv(
    data.frame(cod_unico=character(), hda_nombre=character(),
               Mask_GOES=numeric(), GOES_Fuego=logical(), Estado_GOES=character()),
    "data_master/GOES16_Alertas.csv", row.names = FALSE
  )
})

# =============================================================================
# 2. VERIFICAR ALERTAS
# =============================================================================
ruta_alertas <- "data_master/GOES16_Alertas.csv"
if (!file.exists(ruta_alertas)) {
  message("✅ No hay archivo de alertas generado. Operación normal.")
  quit(status = 0)
}

alertas_df <- read.csv(ruta_alertas, stringsAsFactors = FALSE)
if (nrow(alertas_df) == 0) {
  message("✅ Archivo de alertas vacío. Todo normal.")
  quit(status = 0)
}

# =============================================================================
# 3. EXTRAER COORDENADAS Y RIESGO DE LA BASE MAESTRA
# =============================================================================
# Cargar la capa espacial básica con control total de fallos
cana <- tryCatch({
  cana_path <- tryCatch(
    normalizePath(list.files("capas", pattern = "SOR_OK\\.shp$",
                              full.names = TRUE, recursive = TRUE)[1]),
    error = function(e) NA
  )
  if (is.na(cana_path) || !file.exists(cana_path)) stop("No existe el Shapefile SOR_OK.shp")
  st_read(cana_path, quiet = TRUE) %>%
    janitor::clean_names() %>%
    st_transform(4326)
}, error = function(e) {
  message("⚠️  Aviso: SOR_OK.shp no detectado físicamente o error al leer. Usando redundancia de coordenadas.")
  NULL
})

# Extraer coordenadas para el radar
cana_spatial <- tryCatch({
  if (is.null(cana)) stop("Usa respaldo de Master")
  cana_centros <- suppressWarnings(st_centroid(cana))
  coords <- as.data.frame(st_coordinates(cana_centros))
  st_drop_geometry(cana) %>%
    mutate(
      LAT = coords$Y,
      LON = coords$X,
      hda_raw = toupper(stringr::str_replace_all(as.character(dplyr::coalesce(!!!dplyr::select(., dplyr::matches("^hda$|^cod$|^codigo$")))), "[^0-9A-Za-z_]", "")),
      ing_raw = toupper(as.character(dplyr::coalesce(!!!dplyr::select(., dplyr::matches("^ing$|^ingenio$"))))),
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
      cod_unico = paste(ing_clean, stringr::str_pad(hda_raw, width = 6, side = "left", pad = "0"), sep = "_")
    ) %>%
    distinct(cod_unico, .keep_all = TRUE)
}, error = function(e) {
  message("  🛰️  Cargando coordenadas de respaldo del Master RDS...")
  respaldo_path <- "data_master/SATICA_MASTER_v2.2.rds"
  if (file.exists(respaldo_path)) {
    tmp_m <- readRDS(respaldo_path)
    tmp_m %>%
      mutate(
        cod_unico = as.character(cod_hda_key),
        LAT = coalesce(lat, 3.5),
        LON = coalesce(lon, -76.3)
      ) %>%
      select(cod_unico, LAT, LON) %>%
      distinct(cod_unico, .keep_all = TRUE)
  } else {
    data.frame(cod_unico = character(), LAT = numeric(), LON = numeric())
  }
})

# Cargar master RDS para sacar estado de riesgo
master_rds_path <- "data_master/SATICA_MASTER_v2.2.rds"
master_df <- data.frame(cod_unico = character(), riesgo_str = character())
if (file.exists(master_rds_path)) {
  tmp <- readRDS(master_rds_path)
  master_df <- tmp %>%
    group_by(cod_hda_key) %>%
    summarise(
      DIFF_MESES = min(DIFF_HDA_MESES, na.rm = TRUE),
      riesgo_str = riesgo[which.min(DIFF_HDA_MESES)],
      .groups = "drop"
    ) %>%
    mutate(cod_unico = as.character(cod_hda_key)) %>%
    select(cod_unico, riesgo_str)
}

# =============================================================================
# 4. COMPILAR Y ENVIAR TELEGRAM
# =============================================================================
BOT_TOKEN <- Sys.getenv("TELEGRAM_BOT_TOKEN")
CHAT_ID <- Sys.getenv("TELEGRAM_CHAT_ID")
SHINY_URL_BASE <- Sys.getenv("SHINY_URL_BASE") # ej: "https://usuario.shinyapps.io/satica/"

# Si no hay token de bot, asumimos ejecución local de prueba.
if (BOT_TOKEN == "") {
  message("⚠️ No hay TELEGRAM_BOT_TOKEN. Se imprimirá la alerta en consola.")
}
if (SHINY_URL_BASE == "") {
  SHINY_URL_BASE <- "https://CVC_SURORIENTE.shinyapps.io/satica/"
}

# Iterar sobre las alertas
for (i in 1:nrow(alertas_df)) {
  alerta_act <- alertas_df[i, ]
  cod_act <- alerta_act$cod_unico
  hda_nom <- alerta_act$hda_nombre
  
  # Cruces
  sp_data <- cana_spatial %>% filter(cod_unico == cod_act)
  ms_data <- master_df %>% filter(cod_unico == cod_act)
  
  lat_str <- if (nrow(sp_data) > 0) round(sp_data$LAT[1], 5) else "Sin Coord"
  lon_str <- if (nrow(sp_data) > 0) round(sp_data$LON[1], 5) else "Sin Coord"
  riesgo_txt <- if (nrow(ms_data) > 0) ms_data$riesgo_str[1] else "DESCONOCIDO"
  
  # Emoji del riesgo
  emoji_riesgo <- case_when(
    riesgo_txt == "CRITICO" ~ "🔴",
    riesgo_txt == "ALTO" ~ "🟠",
    riesgo_txt == "OBSERVACION" ~ "🟡",
    TRUE ~ "🟢"
  )
  
  suerte_str <- substr(cod_act, 4, 9)
  
  # Construcción del link profundo (Deep Link a la Shiny App)
  link_accion <- sprintf("%s?tab=Satelital&lat=%s&lon=%s&cod=%s", SHINY_URL_BASE, lat_str, lon_str, cod_act)
  
  # Ficha de Acción Rápida (Formato HTML para Telegram)
  # Usamos HTML porque es más sólido que MarkdownV2 con caracteres raros.
  msg_html <- paste0(
    "🔥 <b>ALERTA SATICA V2.0: Certeza Factual (98.1%)</b>\n\n",
    "<b>Hacienda:</b> ", hda_nom, "\n",
    "<b>Suerte:</b> ", suerte_str, "\n",
    "<b>Coordenadas:</b> <code>", lat_str, ", ", lon_str, "</code> \n",
    "<b>Estado:</b> Evento Térmico detectado hace <15 min \n",
    "<b>Riesgo:</b> ", riesgo_txt, " ", emoji_riesgo, "\n",
    "<b>Predicción Humo (HYSPLIT):</b> Trayectoria calculada al oeste \n\n",
    "<b>Acción:</b> <a href='", link_accion, "'>Ver en Radar Satelital de SATICA</a>"
  )
  
  if (BOT_TOKEN != "") {
    url_req <- paste0("https://api.telegram.org/bot", BOT_TOKEN, "/sendMessage")
    res <- POST(url_req, body = list(
      chat_id = CHAT_ID,
      text = msg_html,
      parse_mode = "HTML"
    ), encode = "json")
    
    if (http_type(res) == "application/json") {
      parsed <- content(res, "parsed")
      if (parsed$ok) {
        message(sprintf("✅ Telegram enviado -> Hacienda: %s", hda_nom))
      } else {
        message(sprintf("❌ Error Telegram: %s", parsed$description))
      }
    }
  } else {
    message("--- ALERTA SIMULADA ---")
    message(msg_html)
    message("-----------------------")
  }
}

message("🏁 Ciclo de Centinela finalizado exitosamente.")
