# Script para consolidar boletines históricos en visitas_cvc.csv
library(pdftools)
library(dplyr)
library(sf)
library(openxlsx)

message("--- INICIANDO CONSOLIDACIÓN DE BOLETINES HISTÓRICOS ---")

# 1. Cargar base de datos maestra
master_rds <- readRDS("data_master/SATICA_MASTER_v2.2.rds")

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

# Crear una base única de haciendas
haciendas_db <- master_rds %>%
  distinct(cod_hda_key, hda_nombre, municipio, corregimiento, lat, lon, ing) %>%
  mutate(
    num_part = regmatches(cod_hda_key, regexpr("\\d+", cod_hda_key)),
    ing_part = ing,
    INGENIO_VAL = mapear_ingenio(ing_part)
  )

# 2. Cargar visitas CVC existentes o inicializar
ruta_visitas_master <- "visitas_cvc.csv"
if (file.exists(ruta_visitas_master)) {
  df_visitas <- read.csv(ruta_visitas_master, stringsAsFactors = FALSE)
  colnames(df_visitas) <- tolower(colnames(df_visitas))
  # Asegurar que todas las columnas existan
  required_cols <- c("cod_hda_key", "hda_label", "ingenio_full", "municipio", "corregimiento", "coordenadas", "categoria_alerta", "fecha_visita", "radicado", "boletin_n")
  for (col in required_cols) {
    if (!col %in% colnames(df_visitas)) {
      df_visitas[[col]] <- ""
    }
    df_visitas[[col]] <- as.character(df_visitas[[col]])
  }
} else {
  df_visitas <- data.frame(
    cod_hda_key = character(), hda_label = character(), ingenio_full = character(),
    municipio = character(), corregimiento = character(), coordenadas = character(),
    categoria_alerta = character(), fecha_visita = character(), radicado = character(),
    boletin_n = character(), stringsAsFactors = FALSE
  )
}

df_visitas$cod_hda_key <- toupper(trimws(as.character(df_visitas$cod_hda_key)))

# Helper para mapear fechas a boletines
obtener_num_boletin_desde_fecha <- function(fecha_str) {
  d <- as.Date(fecha_str)
  y <- as.numeric(format(d, "%Y"))
  m <- as.numeric(format(d, "%m"))
  day <- as.numeric(format(d, "%d"))
  
  if (y == 2026) {
    if (m == 1) return("1")
    if (m == 2) {
      if (day <= 15) return("2")
      else return("3")
    }
    if (m == 3) return("4")
    if (m == 4) return("5")
    if (m == 5) return("6")
    if (m == 6) return("7")
  }
  val <- (y - 2026) * 12 + m + 1
  return(as.character(val))
}

# Helper para buscar coincidencia en DB
find_matching_hacienda <- function(code_part, line_text, db) {
  code_clean <- toupper(trimws(code_part))
  is_num <- grepl("^\\d+$", code_clean)
  num_val <- if (is_num) as.integer(code_clean) else NA
  
  candidates <- db %>%
    filter(
      if (is_num) {
        as.integer(gsub("[^0-9]", "", cod_hda_key)) == num_val
      } else {
        grepl(paste0("_0*", code_clean, "$|_", code_clean, "$"), cod_hda_key)
      }
    )
  
  if (nrow(candidates) == 0) return(NULL)
  if (nrow(candidates) == 1) return(candidates[1, ])
  
  # Buscar ingenio en la línea
  ing_prefixes <- list(
    "CA" = c("INCAUCA", "INCA"),
    "MY" = c("MAYAGUEZ", "MAYAG", "MAYA"),
    "CB" = c("CABAÑA", "CABANA", "CABA"),
    "MN" = c("MANUELITA", "MANU"),
    "PC" = c("PICHICHI", "PICH"),
    "PR" = c("PROVIDENCIA", "PROVID"),
    "RP" = c("RIOPAILA", "RIOP"),
    "CC" = c("CASTILLA", "CAST"),
    "ML" = c("MARIA LUISA", "MARIA")
  )
  
  matched_prefix <- NA_character_
  for (pref in names(ing_prefixes)) {
    for (pattern in ing_prefixes[[pref]]) {
      if (grepl(pattern, line_text, ignore.case = TRUE)) {
        matched_prefix <- pref
        break
      }
    }
    if (!is.na(matched_prefix)) break
  }
  
  if (!is.na(matched_prefix)) {
    res <- candidates %>% filter(ing == matched_prefix)
    if (nrow(res) > 0) return(res[1, ])
  }
  
  # Buscar municipio o corregimiento
  for (i in 1:nrow(candidates)) {
    mun <- candidates$municipio[i]
    corr <- candidates$corregimiento[i]
    if (!is.na(mun) && grepl(mun, line_text, ignore.case = TRUE)) {
      return(candidates[i, ])
    }
    if (!is.na(corr) && grepl(corr, line_text, ignore.case = TRUE)) {
      return(candidates[i, ])
    }
  }
  
  return(candidates[1, ])
}

# 3. Escanear carpeta Boletines
pdf_files <- list.files("Boletines", pattern = "\\.pdf$", recursive = TRUE, full.names = TRUE)
excel_files <- list.files("Boletines", pattern = "\\.xlsx$", recursive = TRUE, full.names = TRUE)

extracted_records <- list()

# A. Procesar PDFs
for (f in pdf_files) {
  # Extraer fecha del nombre
  date_str <- regmatches(f, regexpr("\\d{4}-\\d{2}-\\d{2}", f))
  if (length(date_str) == 0) next
  
  num_boletin <- obtener_num_boletin_desde_fecha(date_str)
  message("Procesando PDF: ", f, " (Boletín N. ", num_boletin, ")...")
  
  txt <- tryCatch(pdf_text(f), error = function(e) NULL)
  if (is.null(txt) || length(txt) == 0) next
  
  all_lines <- unlist(strsplit(txt, "\n"))
  for (line in all_lines) {
    m <- regmatches(line, regexec("([A-Za-z0-9áéíóúÁÉÍÓÚñÑ\\s-]+)_([0-9A-Z]+)", line))[[1]]
    if (length(m) > 1) {
      hda_code <- m[3]
      hda_matched <- find_matching_hacienda(hda_code, line, haciendas_db)
      
      if (!is.null(hda_matched)) {
        # Determinar categoría de alerta basada en palabras clave
        cat_alerta <- "Alerta Boletín Histórico"
        if (grepl("CRITICO|CRÍTICO|ALTO", line, ignore.case = TRUE)) cat_alerta <- "Alerta Inminente"
        else if (grepl("OBSERV", line, ignore.case = TRUE)) cat_alerta <- "Focalización Operativa"
        
        rec <- list(
          cod_hda_key = hda_matched$cod_hda_key,
          hda_label = hda_matched$hda_nombre,
          ingenio_full = hda_matched$INGENIO_VAL,
          municipio = hda_matched$municipio,
          corregimiento = hda_matched$corregimiento,
          coordenadas = ifelse(!is.na(hda_matched$lat) & !is.na(hda_matched$lon), 
                               paste(round(hda_matched$lat, 4), round(hda_matched$lon, 4), sep = ", "), 
                               "Sin Coordenadas"),
          categoria_alerta = cat_alerta,
          boletin_n = num_boletin
        )
        extracted_records[[length(extracted_records) + 1]] <- rec
      }
    }
  }
}

# B. Procesar Excels
for (f in excel_files) {
  date_str <- regmatches(f, regexpr("\\d{4}-\\d{2}-\\d{2}", f))
  if (length(date_str) == 0) next
  num_boletin <- obtener_num_boletin_desde_fecha(date_str)
  message("Procesando Excel: ", f, " (Boletín N. ", num_boletin, ")...")
  
  wb <- tryCatch(loadWorkbook(f), error = function(e) NULL)
  if (is.null(wb)) next
  
  sheets <- names(wb)
  for (sh in sheets) {
    df_excel <- tryCatch(read.xlsx(f, sheet = sh), error = function(e) NULL)
    if (is.null(df_excel) || !"Hacienda" %in% colnames(df_excel)) next
    
    cat_alerta <- "Alerta Boletín Histórico"
    if (sh == "Cronograma Inminentes") cat_alerta <- "Alerta Inminente"
    else if (sh == "Top 10 Municipio") cat_alerta <- "Focalización Operativa"
    else if (sh == "Ficha Tecnica DAR") cat_alerta <- "Ficha Técnica Regional"
    
    for (i in seq_len(nrow(df_excel))) {
      hda_full_name <- df_excel$Hacienda[i]
      m <- regmatches(hda_full_name, regexec("_([0-9A-Z]+)$", hda_full_name))[[1]]
      hda_code <- if (length(m) > 1) m[2] else hda_full_name
      
      line_txt <- paste(df_excel$Hacienda[i], df_excel$Ingenio[i], df_excel$Municipio[i], df_excel$Corregimiento[i], sep = " ")
      hda_matched <- find_matching_hacienda(hda_code, line_txt, haciendas_db)
      
      if (!is.null(hda_matched)) {
        rec <- list(
          cod_hda_key = hda_matched$cod_hda_key,
          hda_label = hda_matched$hda_nombre,
          ingenio_full = hda_matched$INGENIO_VAL,
          municipio = hda_matched$municipio,
          corregimiento = hda_matched$corregimiento,
          coordenadas = ifelse(!is.na(hda_matched$lat) & !is.na(hda_matched$lon), 
                               paste(round(hda_matched$lat, 4), round(hda_matched$lon, 4), sep = ", "), 
                               "Sin Coordenadas"),
          categoria_alerta = cat_alerta,
          boletin_n = num_boletin
        )
        extracted_records[[length(extracted_records) + 1]] <- rec
      }
    }
  }
}

# 4. Consolidar registros en df_visitas
message("Registros extraídos de históricos: ", length(extracted_records))

if (length(extracted_records) > 0) {
  df_extracted <- bind_rows(lapply(extracted_records, as.data.frame, stringsAsFactors = FALSE)) %>%
    distinct()
  
  message("Registros únicos extraídos: ", nrow(df_extracted))
  
  # Mezclar en df_visitas
  for (i in seq_len(nrow(df_extracted))) {
    new_rec <- df_extracted[i, ]
    key <- new_rec$cod_hda_key
    
    if (key %in% df_visitas$cod_hda_key) {
      idx <- which(df_visitas$cod_hda_key == key)
      
      # Si ya existe, actualizamos los metadatos pero NUNCA pisamos fecha_visita o radicado
      df_visitas$hda_label[idx] <- new_rec$hda_label
      df_visitas$ingenio_full[idx] <- new_rec$ingenio_full
      df_visitas$municipio[idx] <- new_rec$municipio
      df_visitas$corregimiento[idx] <- new_rec$corregimiento
      df_visitas$coordenadas[idx] <- new_rec$coordenadas
      
      # Combinar categorías de alerta
      old_cats <- unique(trimws(strsplit(df_visitas$categoria_alerta[idx], "\\+")[[1]]))
      new_cats <- unique(trimws(strsplit(new_rec$categoria_alerta, "\\+")[[1]]))
      combined_cats <- unique(c(old_cats, new_cats))
      combined_cats <- combined_cats[combined_cats != "" & !is.na(combined_cats)]
      df_visitas$categoria_alerta[idx] <- paste(combined_cats, collapse = " + ")
      
      # Combinar números de boletín
      old_b <- unique(trimws(strsplit(as.character(df_visitas$boletin_n[idx]), ",")[[1]]))
      new_b <- unique(trimws(strsplit(as.character(new_rec$boletin_n), ",")[[1]]))
      combined_b <- unique(c(old_b, new_b))
      combined_b <- combined_b[combined_b != "" & !is.na(combined_b)]
      # Ordenar numéricamente para limpieza
      combined_b <- combined_b[order(as.numeric(combined_b))]
      df_visitas$boletin_n[idx] <- paste(combined_b, collapse = ", ")
      
    } else {
      # Si es nueva, la insertamos
      df_visitas <- bind_rows(df_visitas, data.frame(
        cod_hda_key = new_rec$cod_hda_key,
        hda_label = new_rec$hda_label,
        ingenio_full = new_rec$ingenio_full,
        municipio = new_rec$municipio,
        corregimiento = new_rec$corregimiento,
        coordenadas = new_rec$coordenadas,
        categoria_alerta = new_rec$categoria_alerta,
        fecha_visita = "",
        radicado = "",
        boletin_n = new_rec$boletin_n,
        stringsAsFactors = FALSE
      ))
    }
  }
}

# Escribir visitas_cvc.csv ordenado por boletín y ingenio
df_visitas <- df_visitas %>%
  arrange(cod_hda_key)

write.csv(df_visitas, ruta_visitas_master, row.names = FALSE, fileEncoding = "UTF-8")
message("✅ CONSOLIDACIÓN EXITOSA. visitas_cvc.csv actualizado.")
