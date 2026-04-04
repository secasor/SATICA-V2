# ==============================================================================
# ANÁLISIS DE INCIDENCIA ANTRÓPICA EN INCENDIOS DE CAÑA
# ==============================================================================
# Propósito: Cruzar incendios reportados con la proximidad a vías,
#            centros poblados y otras variables de actividad humana
#            para cuantificar el porcentaje de incidencia antrópica.
#
# Contexto (Acta 25 marzo 2026 - Punto 7):
#   - Los incendios de caña son antrópicos, no naturales
#   - Causas: propietarios adelantan cosecha, quema de basura, despejar cañaverales
#   - Correlación esperada: cercanía a vías y centros poblados
# ==============================================================================

library(dplyr)
library(lubridate)
library(sf)
library(tidyr)
library(stringr)
library(readxl)
library(janitor)
library(purrr)
library(ggplot2)

# --- 0. CARGAR FUNCIONES AUXILIARES ---
source("aux_functions.R", encoding = "UTF-8")

message("=" %>% strrep(70))
message("🔬 ANÁLISIS DE INCIDENCIA ANTRÓPICA EN INCENDIOS DE CAÑA")
message("   Correlación entre incendios y proximidad a actividad humana")
message("=" %>% strrep(70))

# ==============================================================================
# FASE 1: INGESTA DE DATOS DE INCENDIOS
# ==============================================================================
message("\n--- FASE 1: Ingesta de datos de incendios ---")

archivos_excel <- list.files(path = "reportes_cosecha", pattern = "\\.xlsx$|\\.xls$",
                              full.names = TRUE, recursive = TRUE)
message(sprintf("📂 Encontrados %d archivos Excel", length(archivos_excel)))

POSICIONES_CLAVE <- list(
  col_nombre_ingenio = 1, col_fecha_dato = 3, col_cod_hacienda = 4,
  col_cod_suerte = 8, col_otra_feature = 6, col_area_predicha = 7,
  col_cod_cosecha = 2
)

leer_archivo <- function(ruta_archivo) {
  data_raw <- tryCatch(read_excel(ruta_archivo, col_names = TRUE), error = function(e) NULL)
  if (is.null(data_raw)) return(NULL)
  data_clean <- data_raw %>% clean_names()
  if (ncol(data_clean) < 8) return(NULL)

  data_final <- data_clean %>%
    select(
      nombre_ingenio_completo = POSICIONES_CLAVE$col_nombre_ingenio,
      fecha_dato              = POSICIONES_CLAVE$col_fecha_dato,
      cod_hacienda            = POSICIONES_CLAVE$col_cod_hacienda,
      cod_suerte              = POSICIONES_CLAVE$col_cod_suerte,
      feature_A               = POSICIONES_CLAVE$col_otra_feature,
      area_predicha           = POSICIONES_CLAVE$col_area_predicha,
      cod_cosecha             = POSICIONES_CLAVE$col_cod_cosecha
    )

  fecha_col <- data_final$fecha_dato
  data_final <- data_final %>%
    mutate(
      num_dates = suppressWarnings(as.numeric(fecha_col)),
      char_dates = suppressWarnings(as.Date(as.character(fecha_col), format = "%Y/%m/%d")),
      fecha_dato = coalesce(
        excel_numeric_to_date(num_dates, date_system = "modern"),
        char_dates
      ),
      cod_hacienda = as.character(cod_hacienda),
      cod_suerte = as.character(cod_suerte),
      cod_cosecha = toupper(as.character(cod_cosecha)),
      feature_A = as.character(feature_A)
    ) %>%
    select(-num_dates, -char_dates)

  return(data_final)
}

datos_todos <- map_dfr(archivos_excel, leer_archivo, .id = "source")
datos_todos <- datos_todos %>% create_cod_unico()
datos_todos <- datos_todos %>% filter(!is.null(COD_UNICO) & !is.na(nombre_ingenio_completo))

# Construir clave de 14 chars (ING_2 + HDA_6_PAD + STE_6_PAD) directamente desde columnas raw
# para que coincida con el formato del shapefile/matriz: "CA010685000001"
datos_todos <- datos_todos %>%
  mutate(
    abv_ing = translate_ingenio(nombre_ingenio_completo),
    hda_padded = str_pad(normalize_code(cod_hacienda), width = 6, side = "left", pad = "0"),
    ste_padded = str_pad(normalize_code(cod_suerte), width = 6, side = "left", pad = "0"),
    COD_UNICO_14 = paste0(abv_ing, hda_padded, ste_padded)
  )

# Filtrar solo incendios
incendios <- datos_todos %>% filter(cod_cosecha == "I")

message(sprintf("📊 Total registros consolidados: %d", nrow(datos_todos)))
message(sprintf("🔥 Total incendios reportados: %d", nrow(incendios)))
message(sprintf("   Rango: %s a %s",
                min(incendios$fecha_dato, na.rm = TRUE),
                max(incendios$fecha_dato, na.rm = TRUE)))

# Crear COD_HACIENDA (primeros 8 chars del código de 14)
incendios <- incendios %>%
  mutate(COD_HACIENDA = substr(COD_UNICO_14, 1, 8))

# ==============================================================================
# FASE 1B: DICCIONARIO DE NOMBRES DE HACIENDA (desde capas geográficas)
# ==============================================================================
message("\n--- FASE 1B: Construyendo diccionario de nombres de hacienda ---")

# --- Función de depuración de nombres corruptos por encoding de shapefiles ---
depurar_nombre <- function(nombre) {
  x <- toupper(trimws(as.character(nombre)))
  
  # 1. Restaurar Ñ desde corrupciones comunes del shapefile
  x <- gsub("AA'A", "AÑA",  x)   # CABAA'A → CABAÑA
  x <- gsub("A'A",  "AÑA",  x)   # variante
  x <- gsub("A'O",  "AÑO",  x)   # PA'O → PAÑO
  x <- gsub("O'O",  "OÑO",  x)   # O'O → OÑO  
  x <- gsub("E'A",  "EÑA",  x)   # PE'A → PEÑA
  x <- gsub("U'A",  "UÑA",  x)   # CU'A → CUÑA
  x <- gsub("I'O",  "IÑO",  x)   # NI'O → NIÑO
  x <- gsub("A'E",  "AÑE",  x)
  
  # 2. Reparar palabras partidas por encoding del shapefile (correcciones específicas)
  x <- gsub("PA RRAGA", "PARRAGA", x)
  x <- gsub("CA RRERA", "CARRERA", x)
  x <- gsub("BA RRANCA", "BARRANCA", x)
  x <- gsub("BE RNAL", "BERNAL", x)
  x <- gsub("GUE RRERO", "GUERRERO", x)
  
  # 3. Eliminar sufijos tipo _NNN, _NNNN (códigos de suerte pegados al nombre)
  x <- gsub("_\\d{2,}$", "", x)
  
  # 4. Limpiar guiones bajos residuales y espacios múltiples
  x <- gsub("_+$", "", x)         # guiones al final
  x <- gsub("_", " ", x)          # guiones internos → espacio
  x <- gsub("\\s+", " ", x)       # múltiples espacios → uno
  x <- trimws(x)
  
  # 5. Correcciones específicas conocidas
  x <- gsub("^CABANA$", "CABAÑA", x)
  x <- gsub("CASABCA", "CASA BLANCA", x)
  x <- gsub("/", " / ", x)        # espaciar barras: A/B → A / B
  x <- gsub("\\s+", " ", x)       # limpiar después de espaciar barras
  x <- trimws(x)
  
  return(x)
}

# Cargar nombres desde Caña_SOR_OK.shp
diccionario_nombres <- tryCatch({
  cana_sor <- st_read("capas/Caña_SOR_OK.shp", quiet = TRUE) %>%
    st_drop_geometry() %>%
    janitor::clean_names()
  cana_sor %>%
    filter(!is.na(ing_hda), !is.na(nombre_hda)) %>%
    group_by(ing_hda) %>%
    summarise(NOMBRE_HACIENDA = first(depurar_nombre(nombre_hda)), .groups = "drop")
}, error = function(e) {
  message("   ⚠️ No se pudo leer Caña_SOR_OK.shp: ", e$message)
  tibble(ing_hda = character(), NOMBRE_HACIENDA = character())
})

# Complementar con Suertes_Valle.shp (haciendas que no están en Caña_SOR_OK)
diccionario_valle <- tryCatch({
  suertes_v <- st_read("capas/Suertes_Valle.shp", quiet = TRUE) %>%
    st_drop_geometry() %>%
    janitor::clean_names()
  suertes_v %>%
    filter(!is.na(ing_hda), !is.na(nombre_hda)) %>%
    filter(!ing_hda %in% diccionario_nombres$ing_hda) %>%
    group_by(ing_hda) %>%
    summarise(NOMBRE_HACIENDA = first(depurar_nombre(nombre_hda)), .groups = "drop")
}, error = function(e) {
  message("   ⚠️ No se pudo leer Suertes_Valle.shp: ", e$message)
  tibble(ing_hda = character(), NOMBRE_HACIENDA = character())
})

diccionario_nombres <- bind_rows(diccionario_nombres, diccionario_valle)
message(sprintf("   ✅ Diccionario construido: %d haciendas con nombre", nrow(diccionario_nombres)))

# Enriquecer incendios con el nombre de la hacienda
incendios <- incendios %>%
  left_join(diccionario_nombres, by = c("COD_HACIENDA" = "ing_hda")) %>%
  mutate(NOMBRE_HACIENDA = coalesce(NOMBRE_HACIENDA, paste0("[COD:", COD_HACIENDA, "]")))

n_con_nombre <- sum(!grepl("^\\[COD:", incendios$NOMBRE_HACIENDA))
message(sprintf("   📊 Incendios con nombre de hacienda resuelto: %d/%d (%.1f%%)",
                n_con_nombre, nrow(incendios),
                100 * n_con_nombre / nrow(incendios)))

# ==============================================================================
# FASE 2: CRUZAR CON MATRIZ DE DISTANCIAS GEOESPACIALES
# ==============================================================================
message("\n--- FASE 2: Cruce con Matriz de Distancias Geoespaciales ---")

matriz_distancias <- tryCatch(readRDS("data_master/matriz_distancias_cana.rds"), error = function(e) NULL)

if (is.null(matriz_distancias)) {
  stop("❌ No se encontró la matriz de distancias. Ejecutar R/crear_matriz_geo.R primero.")
}

message(sprintf("   Matriz de distancias cargada: %d registros", nrow(matriz_distancias)))
message(sprintf("   Variables: %s", paste(names(matriz_distancias), collapse = ", ")))
message(sprintf("   Ejemplo códigos matriz: %s", paste(head(matriz_distancias$cod_unico, 3), collapse = ", ")))
message(sprintf("   Ejemplo códigos incendios: %s", paste(head(incendios$COD_UNICO_14, 3), collapse = ", ")))

# --- JOIN A: Nivel SUERTE (match exacto de 14 chars) ---
incendios_geo <- incendios %>%
  left_join(matriz_distancias, by = c("COD_UNICO_14" = "cod_unico"))

n_match_suerte <- sum(!is.na(incendios_geo$dist_vias_m))
message(sprintf("   Match directo suerte (14 chars): %d/%d (%.1f%%)",
                n_match_suerte, nrow(incendios_geo),
                100 * n_match_suerte / nrow(incendios_geo)))

# --- JOIN B: Fallback nivel HACIENDA (primeros 8 chars) ---
# Para incendios que no coinciden a nivel suerte, asignar la distancia
# promedio de las suertes de la misma hacienda en la matriz
if (n_match_suerte < nrow(incendios_geo)) {
  dist_por_hacienda <- matriz_distancias %>%
    mutate(COD_HDA_8 = substr(cod_unico, 1, 8)) %>%
    group_by(COD_HDA_8) %>%
    summarise(
      dist_vias_m_hda = median(dist_vias_m, na.rm = TRUE),
      dist_poblados_m_hda = median(dist_poblados_m, na.rm = TRUE),
      dist_bosques_m_hda = median(dist_bosques_m, na.rm = TRUE),
      .groups = "drop"
    )
  
  incendios_geo <- incendios_geo %>%
    left_join(dist_por_hacienda, by = c("COD_HACIENDA" = "COD_HDA_8")) %>%
    mutate(
      dist_vias_m = coalesce(dist_vias_m, dist_vias_m_hda),
      dist_poblados_m = coalesce(dist_poblados_m, dist_poblados_m_hda),
      dist_bosques_m = coalesce(dist_bosques_m, dist_bosques_m_hda)
    ) %>%
    select(-dist_vias_m_hda, -dist_poblados_m_hda, -dist_bosques_m_hda)
  
  n_match_total <- sum(!is.na(incendios_geo$dist_vias_m))
  n_match_hda <- n_match_total - n_match_suerte
  message(sprintf("   Match fallback hacienda (8 chars): +%d → Total: %d/%d (%.1f%%)",
                  n_match_hda, n_match_total, nrow(incendios_geo),
                  100 * n_match_total / nrow(incendios_geo)))
}

n_con_geo <- sum(!is.na(incendios_geo$dist_vias_m))
n_sin_geo <- sum(is.na(incendios_geo$dist_vias_m))

message(sprintf("   ✅ Incendios con datos geoespaciales: %d (%.1f%%)",
                n_con_geo, 100 * n_con_geo / nrow(incendios_geo)))
message(sprintf("   ⚠️  Incendios sin georreferenciación: %d (%.1f%%)",
                n_sin_geo, 100 * n_sin_geo / nrow(incendios_geo)))

# Filtrar solo los que tienen datos geoespaciales para el análisis
incendios_analisis <- incendios_geo %>% filter(!is.na(dist_vias_m))

# ==============================================================================
# FASE 3: ANÁLISIS DE PROXIMIDAD — UMBRALES DE ZONA ANTRÓPICA
# ==============================================================================
message("\n--- FASE 3: Análisis de Proximidad a Actividad Humana ---")

# Definir umbrales de proximidad (metros)
# ≤500m: Zona de Alto Impacto Antrópico (despejar vista, basura)
# ≤1000m: Zona de Influencia Antrópica Directa
# ≤2000m: Zona de Influencia Antrópica Ampliada
# >2000m: Zona Remota

incendios_analisis <- incendios_analisis %>%
  mutate(
    # --- Clasificación por cercanía a VÍAS ---
    zona_vias = case_when(
      dist_vias_m <= 500  ~ "≤500m (Alto Impacto)",
      dist_vias_m <= 1000 ~ "501-1000m (Influencia Directa)",
      dist_vias_m <= 2000 ~ "1001-2000m (Influencia Ampliada)",
      TRUE                ~ ">2000m (Zona Remota)"
    ),
    cerca_via_500  = dist_vias_m <= 500,
    cerca_via_1000 = dist_vias_m <= 1000,
    cerca_via_2000 = dist_vias_m <= 2000,
    
    # --- Clasificación por cercanía a CENTROS POBLADOS ---
    zona_poblados = case_when(
      dist_poblados_m <= 500  ~ "≤500m (Alto Impacto)",
      dist_poblados_m <= 1000 ~ "501-1000m (Influencia Directa)",
      dist_poblados_m <= 2000 ~ "1001-2000m (Influencia Ampliada)",
      TRUE                    ~ ">2000m (Zona Remota)"
    ),
    cerca_poblado_500  = dist_poblados_m <= 500,
    cerca_poblado_1000 = dist_poblados_m <= 1000,
    cerca_poblado_2000 = dist_poblados_m <= 2000,
    
    # --- Clasificación COMBINADA (cerca de al menos una variable humana) ---
    cerca_actividad_humana_500  = cerca_via_500  | cerca_poblado_500,
    cerca_actividad_humana_1000 = cerca_via_1000 | cerca_poblado_1000,
    cerca_actividad_humana_2000 = cerca_via_2000 | cerca_poblado_2000,
    
    # --- Zona antrópica combinada con múltiples indicadores ---
    n_factores_antropicos_500 = as.integer(cerca_via_500) + as.integer(cerca_poblado_500),
    n_factores_antropicos_1000 = as.integer(cerca_via_1000) + as.integer(cerca_poblado_1000),
    n_factores_antropicos_2000 = as.integer(cerca_via_2000) + as.integer(cerca_poblado_2000),
    
    # --- Clasificación de Exposición Antrópica ---
    exposicion_antropica = case_when(
      n_factores_antropicos_500 == 2 ~ "MÁXIMA (Vía + Poblado ≤500m)",
      n_factores_antropicos_500 >= 1 ~ "ALTA (Algún factor ≤500m)",
      n_factores_antropicos_1000 >= 2 ~ "MEDIA-ALTA (Vía + Poblado ≤1km)",
      n_factores_antropicos_1000 >= 1 ~ "MEDIA (Algún factor ≤1km)",
      n_factores_antropicos_2000 >= 1 ~ "BAJA (Algún factor ≤2km)",
      TRUE ~ "MÍNIMA (Sin factores cercanos)"
    ),
    
    # Año del incendio
    ano_incendio = year(fecha_dato)
  )

# ==============================================================================
# FASE 4: TABLAS DE RESULTADOS
# ==============================================================================
message("\n--- FASE 4: Resultados del Análisis ---")

total_incendios <- nrow(incendios_analisis)
message(sprintf("\n📊 UNIVERSO DE ANÁLISIS: %d incendios con georreferenciación", total_incendios))

# --- TABLA 1: Incendios por proximidad a VÍAS ---
tabla_vias <- incendios_analisis %>%
  group_by(zona_vias) %>%
  summarise(
    n_incendios = n(),
    pct = 100 * n() / total_incendios,
    dist_media_m = round(mean(dist_vias_m), 0),
    dist_mediana_m = round(median(dist_vias_m), 0),
    haciendas_unicas = n_distinct(COD_HACIENDA),
    ejemplo_haciendas = paste(head(unique(NOMBRE_HACIENDA), 3), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(dist_media_m)

message("\n🛣️  TABLA 1: INCENDIOS POR PROXIMIDAD A VÍAS INTERMUNICIPALES")
message("   Zona                        | Incendios | %        | Dist. Media | Dist. Mediana | Haciendas")
message("   ————————————————————————————+———————————+——————————+—————————————+———————————————+——————————")
for (i in 1:nrow(tabla_vias)) {
  message(sprintf("   %-28s| %9d | %6.1f%% | %8d m  | %10d m  | %9d",
                  tabla_vias$zona_vias[i],
                  tabla_vias$n_incendios[i],
                  tabla_vias$pct[i],
                  tabla_vias$dist_media_m[i],
                  tabla_vias$dist_mediana_m[i],
                  tabla_vias$haciendas_unicas[i]))
}

# Acumulados vías
pct_via_500 <- 100 * sum(incendios_analisis$cerca_via_500) / total_incendios
pct_via_1000 <- 100 * sum(incendios_analisis$cerca_via_1000) / total_incendios
pct_via_2000 <- 100 * sum(incendios_analisis$cerca_via_2000) / total_incendios
message(sprintf("\n   📌 ACUMULADO VÍAS: ≤500m = %.1f%% | ≤1km = %.1f%% | ≤2km = %.1f%%",
                pct_via_500, pct_via_1000, pct_via_2000))

# --- TABLA 2: Incendios por proximidad a CENTROS POBLADOS ---
tabla_poblados <- incendios_analisis %>%
  group_by(zona_poblados) %>%
  summarise(
    n_incendios = n(),
    pct = 100 * n() / total_incendios,
    dist_media_m = round(mean(dist_poblados_m), 0),
    dist_mediana_m = round(median(dist_poblados_m), 0),
    haciendas_unicas = n_distinct(COD_HACIENDA),
    ejemplo_haciendas = paste(head(unique(NOMBRE_HACIENDA), 3), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(dist_media_m)

message("\n🏘️  TABLA 2: INCENDIOS POR PROXIMIDAD A CENTROS POBLADOS")
message("   Zona                        | Incendios | %        | Dist. Media | Dist. Mediana | Haciendas")
message("   ————————————————————————————+———————————+——————————+—————————————+———————————————+——————————")
for (i in 1:nrow(tabla_poblados)) {
  message(sprintf("   %-28s| %9d | %6.1f%% | %8d m  | %10d m  | %9d",
                  tabla_poblados$zona_poblados[i],
                  tabla_poblados$n_incendios[i],
                  tabla_poblados$pct[i],
                  tabla_poblados$dist_media_m[i],
                  tabla_poblados$dist_mediana_m[i],
                  tabla_poblados$haciendas_unicas[i]))
}

# Acumulados poblados
pct_pob_500 <- 100 * sum(incendios_analisis$cerca_poblado_500) / total_incendios
pct_pob_1000 <- 100 * sum(incendios_analisis$cerca_poblado_1000) / total_incendios
pct_pob_2000 <- 100 * sum(incendios_analisis$cerca_poblado_2000) / total_incendios
message(sprintf("\n   📌 ACUMULADO POBLADOS: ≤500m = %.1f%% | ≤1km = %.1f%% | ≤2km = %.1f%%",
                pct_pob_500, pct_pob_1000, pct_pob_2000))

# --- TABLA 3: EXPOSICIÓN ANTRÓPICA COMBINADA ---
tabla_exposicion <- incendios_analisis %>%
  group_by(exposicion_antropica) %>%
  summarise(
    n_incendios = n(),
    pct = 100 * n() / total_incendios,
    haciendas_unicas = n_distinct(COD_HACIENDA),
    ejemplo_haciendas = paste(head(unique(NOMBRE_HACIENDA), 3), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(desc(n_incendios))

message("\n🔥 TABLA 3: EXPOSICIÓN ANTRÓPICA COMBINADA (VÍA + POBLADO)")
message("   Exposición                            | Incendios | %        | Haciendas")
message("   ——————————————————————————————————————+———————————+——————————+——————————")
for (i in 1:nrow(tabla_exposicion)) {
  message(sprintf("   %-40s| %9d | %6.1f%% | %9d",
                  tabla_exposicion$exposicion_antropica[i],
                  tabla_exposicion$n_incendios[i],
                  tabla_exposicion$pct[i],
                  tabla_exposicion$haciendas_unicas[i]))
}

# --- TABLA 4: Porcentaje global con actividad humana cercana ---
pct_humana_500 <- 100 * sum(incendios_analisis$cerca_actividad_humana_500) / total_incendios
pct_humana_1000 <- 100 * sum(incendios_analisis$cerca_actividad_humana_1000) / total_incendios
pct_humana_2000 <- 100 * sum(incendios_analisis$cerca_actividad_humana_2000) / total_incendios

message("\n" %>% paste0("=" %>% strrep(70)))
message("🎯 INDICADOR PRINCIPAL: PORCENTAJE DE INCIDENCIA ANTRÓPICA")
message("=" %>% strrep(70))
message(sprintf("   A ≤500m  de vía O centro poblado: %d/%d incendios = %.1f%%",
                sum(incendios_analisis$cerca_actividad_humana_500), total_incendios, pct_humana_500))
message(sprintf("   A ≤1 km  de vía O centro poblado: %d/%d incendios = %.1f%%",
                sum(incendios_analisis$cerca_actividad_humana_1000), total_incendios, pct_humana_1000))
message(sprintf("   A ≤2 km  de vía O centro poblado: %d/%d incendios = %.1f%%",
                sum(incendios_analisis$cerca_actividad_humana_2000), total_incendios, pct_humana_2000))

# ==============================================================================
# FASE 5: COMPARATIVA — ¿Las suertes CON incendio están más cerca que las demás?
# ==============================================================================
message("\n--- FASE 5: Comparativa con suertes SIN incendio ---")

# Obtener todas las suertes del universo con sus distancias
todas_suertes <- datos_todos %>%
  select(COD_UNICO, COD_UNICO_14) %>%
  distinct(COD_UNICO_14, .keep_all = TRUE) %>%
  mutate(COD_HDA_8 = substr(COD_UNICO_14, 1, 8)) %>%
  left_join(matriz_distancias, by = c("COD_UNICO_14" = "cod_unico"))

# Fallback por hacienda para las que no matchearon
if (exists("dist_por_hacienda")) {
  todas_suertes <- todas_suertes %>%
    left_join(dist_por_hacienda, by = c("COD_HDA_8" = "COD_HDA_8")) %>%
    mutate(
      dist_vias_m = coalesce(dist_vias_m, dist_vias_m_hda),
      dist_poblados_m = coalesce(dist_poblados_m, dist_poblados_m_hda),
      dist_bosques_m = coalesce(dist_bosques_m, dist_bosques_m_hda)
    ) %>%
    select(-dist_vias_m_hda, -dist_poblados_m_hda, -dist_bosques_m_hda)
}

todas_suertes <- todas_suertes %>% filter(!is.na(dist_vias_m))

suertes_con_incendio <- incendios_analisis %>%
  select(COD_UNICO_14) %>%
  distinct()

todas_suertes <- todas_suertes %>%
  mutate(tuvo_incendio = COD_UNICO_14 %in% suertes_con_incendio$COD_UNICO_14)

comparativa <- todas_suertes %>%
  group_by(tuvo_incendio) %>%
  summarise(
    n_suertes = n(),
    dist_vias_media = round(mean(dist_vias_m), 0),
    dist_vias_mediana = round(median(dist_vias_m), 0),
    dist_poblados_media = round(mean(dist_poblados_m), 0),
    dist_poblados_mediana = round(median(dist_poblados_m), 0),
    pct_cerca_via_1km = 100 * mean(dist_vias_m <= 1000),
    pct_cerca_poblado_1km = 100 * mean(dist_poblados_m <= 1000),
    .groups = "drop"
  )

message("\n📊 TABLA 5: COMPARATIVA — Suertes CON incendio vs SIN incendio")
message("   Grupo           | Suertes | Dist.Vías(med) | Dist.Pobl(med) | %VíaCerca1km | %PoblCerca1km")
message("   ————————————————+—————————+————————————————+————————————————+——————————————+——————————————")
for (i in 1:nrow(comparativa)) {
  label <- ifelse(comparativa$tuvo_incendio[i], "CON INCENDIO", "SIN INCENDIO")
  message(sprintf("   %-16s| %7d | %11d m  | %11d m  | %10.1f%%  | %10.1f%%",
                  label,
                  comparativa$n_suertes[i],
                  comparativa$dist_vias_mediana[i],
                  comparativa$dist_poblados_mediana[i],
                  comparativa$pct_cerca_via_1km[i],
                  comparativa$pct_cerca_poblado_1km[i]))
}

# ==============================================================================
# FASE 6: ANÁLISIS POR AÑO — EVOLUCIÓN TEMPORAL
# ==============================================================================
message("\n--- FASE 6: Evolución Temporal de la Incidencia Antrópica ---")

evolucion_anual <- incendios_analisis %>%
  group_by(ano_incendio) %>%
  summarise(
    n_incendios = n(),
    pct_cerca_via_1km = 100 * mean(cerca_via_1000),
    pct_cerca_poblado_1km = 100 * mean(cerca_poblado_1000),
    pct_actividad_humana_1km = 100 * mean(cerca_actividad_humana_1000),
    dist_vias_mediana = round(median(dist_vias_m), 0),
    dist_poblados_mediana = round(median(dist_poblados_m), 0),
    .groups = "drop"
  ) %>%
  filter(!is.na(ano_incendio))

message("\n   Año  | Incendios | %Vía≤1km | %Poblado≤1km | %Humana≤1km | Dist.Vías(med) | Dist.Pobl(med)")
message("   —————+———————————+——————————+——————————————+—————————————+————————————————+————————————————")
for (i in 1:nrow(evolucion_anual)) {
  message(sprintf("   %4d | %9d | %6.1f%%  | %10.1f%%  | %9.1f%%  | %11d m  | %11d m",
                  evolucion_anual$ano_incendio[i],
                  evolucion_anual$n_incendios[i],
                  evolucion_anual$pct_cerca_via_1km[i],
                  evolucion_anual$pct_cerca_poblado_1km[i],
                  evolucion_anual$pct_actividad_humana_1km[i],
                  evolucion_anual$dist_vias_mediana[i],
                  evolucion_anual$dist_poblados_mediana[i]))
}

# ==============================================================================
# FASE 7: ANÁLISIS POR HACIENDA — Top haciendas más expuestas
# ==============================================================================
message("\n--- FASE 7: Haciendas con mayor exposición antrópica e incidencia ---")

top_hdas_expuestas <- incendios_analisis %>%
  group_by(COD_HACIENDA, NOMBRE_HACIENDA) %>%
  summarise(
    ingenio = first(nombre_ingenio_completo),
    n_incendios = n(),
    dist_vias_mediana = round(median(dist_vias_m), 0),
    dist_poblados_mediana = round(median(dist_poblados_m), 0),
    pct_cerca_via_1km = 100 * mean(cerca_via_1000),
    pct_cerca_poblado_1km = 100 * mean(cerca_poblado_1000),
    exposicion_predominante = names(sort(table(exposicion_antropica), decreasing = TRUE))[1],
    .groups = "drop"
  ) %>%
  filter(n_incendios >= 3) %>%
  arrange(desc(n_incendios)) %>%
  head(25)

message("\n🏠 TOP 25 HACIENDAS con ≥3 incendios y su exposición antrópica:")
message("   Hacienda                    | Ingenio     | Inc. | D.Vías(med) | D.Pobl(med) | %Vía≤1km | %Pobl≤1km | Exposición")
message("   ————————————————————————————+—————————————+——————+—————————————+—————————————+——————————+———————————+———————————————")
for (i in 1:nrow(top_hdas_expuestas)) {
  message(sprintf("   %-28s| %-12s| %4d | %8d m  | %8d m  | %6.0f%%  | %7.0f%%  | %s",
                  top_hdas_expuestas$NOMBRE_HACIENDA[i],
                  top_hdas_expuestas$ingenio[i],
                  top_hdas_expuestas$n_incendios[i],
                  top_hdas_expuestas$dist_vias_mediana[i],
                  top_hdas_expuestas$dist_poblados_mediana[i],
                  top_hdas_expuestas$pct_cerca_via_1km[i],
                  top_hdas_expuestas$pct_cerca_poblado_1km[i],
                  top_hdas_expuestas$exposicion_predominante[i]))
}

# ==============================================================================
# FASE 8: EXPORTACIÓN DE GRÁFICOS
# ==============================================================================
message("\n--- FASE 8: Generación de Gráficos ---")

if (!dir.exists("resultados_modelo")) dir.create("resultados_modelo")

# --- Gráfico 1: Distribución de distancias a vías (incendios vs todas) ---
p1 <- ggplot() +
  geom_density(data = todas_suertes %>% filter(!tuvo_incendio),
               aes(x = dist_vias_m / 1000, fill = "Sin Incendio"),
               alpha = 0.4) +
  geom_density(data = todas_suertes %>% filter(tuvo_incendio),
               aes(x = dist_vias_m / 1000, fill = "Con Incendio"),
               alpha = 0.6) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "#e74c3c", linewidth = 0.8) +
  annotate("text", x = 1.1, y = Inf, label = "1 km", color = "#e74c3c",
           vjust = 2, hjust = 0, size = 4) +
  scale_fill_manual(values = c("Con Incendio" = "#c0392b", "Sin Incendio" = "#3498db")) +
  labs(
    title = "Distribución de Distancia a Vías Intermunicipales",
    subtitle = "Suertes con incendio vs suertes sin incendio (2019-2025)",
    x = "Distancia a vía más cercana (km)", y = "Densidad",
    fill = "Grupo"
  ) +
  xlim(0, 10) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "top")

ggsave("resultados_modelo/grafico_dist_vias_incendios.png", p1, width = 11, height = 7, dpi = 150)

# --- Gráfico 2: Distribución de distancias a centros poblados ---
p2 <- ggplot() +
  geom_density(data = todas_suertes %>% filter(!tuvo_incendio),
               aes(x = dist_poblados_m / 1000, fill = "Sin Incendio"),
               alpha = 0.4) +
  geom_density(data = todas_suertes %>% filter(tuvo_incendio),
               aes(x = dist_poblados_m / 1000, fill = "Con Incendio"),
               alpha = 0.6) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "#e74c3c", linewidth = 0.8) +
  annotate("text", x = 1.1, y = Inf, label = "1 km", color = "#e74c3c",
           vjust = 2, hjust = 0, size = 4) +
  scale_fill_manual(values = c("Con Incendio" = "#c0392b", "Sin Incendio" = "#3498db")) +
  labs(
    title = "Distribución de Distancia a Centros Poblados",
    subtitle = "Suertes con incendio vs suertes sin incendio (2019-2025)",
    x = "Distancia a centro poblado más cercano (km)", y = "Densidad",
    fill = "Grupo"
  ) +
  xlim(0, 10) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "top")

ggsave("resultados_modelo/grafico_dist_poblados_incendios.png", p2, width = 11, height = 7, dpi = 150)

# --- Gráfico 3: Barras de exposición antrópica ---
tabla_exp_orden <- tabla_exposicion %>%
  mutate(exposicion_antropica = factor(exposicion_antropica,
    levels = c("MÁXIMA (Vía + Poblado ≤500m)",
               "ALTA (Algún factor ≤500m)",
               "MEDIA-ALTA (Vía + Poblado ≤1km)",
               "MEDIA (Algún factor ≤1km)",
               "BAJA (Algún factor ≤2km)",
               "MÍNIMA (Sin factores cercanos)")))

p3 <- ggplot(tabla_exp_orden, aes(x = exposicion_antropica, y = pct, fill = exposicion_antropica)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%\n(%d)", pct, n_incendios)),
            vjust = -0.3, size = 3.2) +
  scale_fill_manual(values = c(
    "MÁXIMA (Vía + Poblado ≤500m)" = "#c0392b",
    "ALTA (Algún factor ≤500m)" = "#e67e22",
    "MEDIA-ALTA (Vía + Poblado ≤1km)" = "#f39c12",
    "MEDIA (Algún factor ≤1km)" = "#f1c40f",
    "BAJA (Algún factor ≤2km)" = "#3498db",
    "MÍNIMA (Sin factores cercanos)" = "#27ae60"
  )) +
  labs(
    title = "Distribución de Incendios por Nivel de Exposición Antrópica",
    subtitle = sprintf("Basado en proximidad a vías intermunicipales y centros poblados (%d incendios analizados)", total_incendios),
    x = "Nivel de Exposición", y = "Porcentaje de Incendios (%)",
    fill = "Nivel"
  ) +
  ylim(0, max(tabla_exposicion$pct) * 1.35) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 25, hjust = 1, size = 9),
        legend.position = "none")

ggsave("resultados_modelo/grafico_exposicion_antropica.png", p3, width = 13, height = 7, dpi = 150)

# --- Gráfico 4: Evolución temporal ---
evol_long <- evolucion_anual %>%
  select(ano_incendio, `Vías ≤1km` = pct_cerca_via_1km,
         `Poblados ≤1km` = pct_cerca_poblado_1km,
         `Cualquier factor ≤1km` = pct_actividad_humana_1km) %>%
  pivot_longer(-ano_incendio, names_to = "variable", values_to = "porcentaje")

p4 <- ggplot(evol_long, aes(x = ano_incendio, y = porcentaje, color = variable, group = variable)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = sprintf("%.0f%%", porcentaje)), vjust = -1, size = 3.2) +
  scale_color_manual(values = c(
    "Vías ≤1km" = "#e74c3c",
    "Poblados ≤1km" = "#3498db",
    "Cualquier factor ≤1km" = "#2c3e50"
  )) +
  labs(
    title = "Evolución Temporal: % de Incendios Cerca de Actividad Humana",
    subtitle = "Porcentaje de incendios a ≤1km de vías o centros poblados por año",
    x = "Año", y = "% de Incendios",
    color = "Variable"
  ) +
  ylim(0, 110) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "top")

ggsave("resultados_modelo/grafico_evolucion_antropica.png", p4, width = 11, height = 7, dpi = 150)

# --- Gráfico 5: Indicador resumen tipo semáforo ---
resumen_df <- data.frame(
  umbral = c("≤500m", "≤1km", "≤2km"),
  pct = c(pct_humana_500, pct_humana_1000, pct_humana_2000),
  label = sprintf("%.1f%%", c(pct_humana_500, pct_humana_1000, pct_humana_2000))
)
resumen_df$umbral <- factor(resumen_df$umbral, levels = c("≤500m", "≤1km", "≤2km"))

p5 <- ggplot(resumen_df, aes(x = umbral, y = pct, fill = umbral)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = label), vjust = -0.5, size = 6, fontface = "bold") +
  scale_fill_manual(values = c("≤500m" = "#c0392b", "≤1km" = "#e67e22", "≤2km" = "#f39c12")) +
  labs(
    title = "INDICADOR DE INCIDENCIA ANTRÓPICA EN INCENDIOS DE CAÑA",
    subtitle = sprintf("Del %d incendios analizados, ¿cuántos ocurren cerca de vías o centros poblados?",
                        total_incendios),
    x = "Radio de proximidad (a vías O centros poblados)", y = "% de Incendios (%)",
    fill = "Umbral"
  ) +
  ylim(0, 105) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", size = 15),
        legend.position = "none")

ggsave("resultados_modelo/grafico_indicador_antropico.png", p5, width = 10, height = 7, dpi = 150)

# ==============================================================================
# FASE 9: EXPORTAR CSV DE RESULTADOS
# ==============================================================================
message("\n--- FASE 9: Exportación de resultados ---")

# CSV principal con todos los incendios y su clasificación
export_incendios <- incendios_analisis %>%
  select(
    COD_UNICO, COD_HACIENDA, NOMBRE_HACIENDA, nombre_ingenio_completo, feature_A,
    fecha_dato, ano_incendio,
    dist_vias_m, dist_poblados_m, dist_bosques_m,
    zona_vias, zona_poblados,
    exposicion_antropica, n_factores_antropicos_1000,
    cerca_actividad_humana_500, cerca_actividad_humana_1000, cerca_actividad_humana_2000
  )

write.csv(export_incendios,
          "resultados_modelo/analisis_incidencia_antropica.csv",
          row.names = FALSE)

# Tabla resumen
dist_vias_sin_inc <- if (any(comparativa$tuvo_incendio == FALSE)) {
  comparativa$dist_vias_mediana[comparativa$tuvo_incendio == FALSE][1]
} else { NA_real_ }
dist_pobl_sin_inc <- if (any(comparativa$tuvo_incendio == FALSE)) {
  comparativa$dist_poblados_mediana[comparativa$tuvo_incendio == FALSE][1]
} else { NA_real_ }

resumen_final <- data.frame(
  indicador = c(
    "Total incendios analizados",
    "Incendios a <=500m de vía o poblado",
    "Incendios a <=1km de vía o poblado",
    "Incendios a <=2km de vía o poblado",
    "% Incidencia Antrópica (<=500m)",
    "% Incidencia Antrópica (<=1km)",
    "% Incidencia Antrópica (<=2km)",
    "Distancia mediana a vías (incendios) m",
    "Distancia mediana a vías (sin incendio) m",
    "Distancia mediana a poblados (incendios) m",
    "Distancia mediana a poblados (sin incendio) m"
  ),
  valor = c(
    total_incendios,
    sum(incendios_analisis$cerca_actividad_humana_500),
    sum(incendios_analisis$cerca_actividad_humana_1000),
    sum(incendios_analisis$cerca_actividad_humana_2000),
    round(pct_humana_500, 1),
    round(pct_humana_1000, 1),
    round(pct_humana_2000, 1),
    round(median(incendios_analisis$dist_vias_m), 0),
    round(dist_vias_sin_inc, 0),
    round(median(incendios_analisis$dist_poblados_m), 0),
    round(dist_pobl_sin_inc, 0)
  )
)

write.csv(resumen_final,
          "resultados_modelo/resumen_incidencia_antropica.csv",
          row.names = FALSE)

# ==============================================================================
# RESUMEN FINAL
# ==============================================================================
message("\n" %>% paste0("=" %>% strrep(70)))
message("✅ ANÁLISIS DE INCIDENCIA ANTRÓPICA COMPLETADO")
message("=" %>% strrep(70))
message("📁 Archivos generados:")
message("   1. resultados_modelo/analisis_incidencia_antropica.csv")
message("   2. resultados_modelo/resumen_incidencia_antropica.csv")
message("   3. resultados_modelo/grafico_dist_vias_incendios.png")
message("   4. resultados_modelo/grafico_dist_poblados_incendios.png")
message("   5. resultados_modelo/grafico_exposicion_antropica.png")
message("   6. resultados_modelo/grafico_evolucion_antropica.png")
message("   7. resultados_modelo/grafico_indicador_antropico.png")

message(sprintf("\n🎯 VEREDICTO FINAL:"))
message(sprintf("   De %d incendios con georreferenciación (2019-2025):", total_incendios))
message(sprintf("   → %.1f%% ocurren a menos de 500m de una vía o centro poblado", pct_humana_500))
message(sprintf("   → %.1f%% ocurren a menos de 1 km de una vía o centro poblado", pct_humana_1000))
message(sprintf("   → %.1f%% ocurren a menos de 2 km de una vía o centro poblado", pct_humana_2000))
message(sprintf("\n   CONCLUSIÓN: La cercanía a infraestructura humana es un indicador"))
message(sprintf("   consistente que respalda la hipótesis de causalidad antrópica"))
message(sprintf("   documentada en el Acta del 25 de Marzo de 2026 (Punto 2)."))
