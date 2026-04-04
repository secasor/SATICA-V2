# ==============================================================================
# DIAGNÓSTICO DE COBERTURA - SISTEMA SATICA V2.4
# ==============================================================================
# Propósito: Clasificar todas las haciendas con incendio registrado según su
#            presencia en las capas geográficas, auditar fórmulas de ciclo, y
#            generar tablas de contingencia cosecha vs incendio.
# Fecha:     2026-03-21
# Referencia: satica_engine.R (lógica de COD_UNICO_14) y global.R
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf, dplyr, purrr, readxl, janitor, lubridate, stringi, stringr, tidyr)

message("═══════════════════════════════════════════════════════")
message("🔍 DIAGNÓSTICO DE COBERTURA SATICA V2.4")
message("═══════════════════════════════════════════════════════")
sf_use_s2(FALSE)

# --- Directorio de resultados ---
dir_resultados <- "resultados_diagnostico"
if (!dir.exists(dir_resultados)) dir.create(dir_resultados)

# --- Función de limpieza (idéntica a satica_engine.R) ---
limpiar_texto <- function(x) {
  x %>% as.character() %>% toupper() %>%
    stri_trans_general("Latin-ASCII") %>% trimws()
}

# --- Tabla de traducción de ingenios (satica_engine.R) ---
traducir_ingenio <- function(ing_clean) {
  case_when(
    grepl("INCAUCA",     ing_clean) ~ "CA",
    grepl("MAYAGUEZ",    ing_clean) ~ "MY",
    grepl("MARIA LUISA", ing_clean) ~ "ML",
    grepl("CASTILLA",    ing_clean) ~ "CC",
    grepl("PROVIDENCIA", ing_clean) ~ "PR",
    grepl("MANUELITA",   ing_clean) ~ "MN",
    grepl("PICHICHI",    ing_clean) ~ "PC",
    grepl("CABA",        ing_clean) ~ "CB",
    grepl("RIOPAILA",    ing_clean) ~ "RP",
    TRUE ~ NA_character_
  )
}

# Tabla inversa para nombres completos
nombre_ingenio <- function(cod) {
  case_when(
    cod == "CA" ~ "INCAUCA",
    cod == "MY" ~ "MAYAGÜEZ",
    cod == "ML" ~ "MARIA LUISA",
    cod == "CC" ~ "CENTRAL CASTILLA",
    cod == "PR" ~ "PROVIDENCIA",
    cod == "MN" ~ "MANUELITA",
    cod == "PC" ~ "PICHICHI",
    cod == "CB" ~ "LA CABAÑA",
    cod == "RP" ~ "RIOPAILA",
    TRUE ~ paste("ING.", cod)
  )
}

# ═══════════════════════════════════════════════════════
# PASO 1 — CONSTRUIR EL UNIVERSO DE HACIENDAS DESDE EXCEL
# ═══════════════════════════════════════════════════════
message("\n📋 PASO 1: Construyendo universo de haciendas desde los Excel...")

path_reportes <- "reportes_cosecha"
archivos <- list.files(path_reportes, pattern = "\\.xlsx$|\\.xls$", full.names = TRUE)
message(sprintf("   Archivos encontrados: %d", length(archivos)))

# Acumuladores
registros_ok     <- list()
casos_especiales <- list()

for (i in seq_along(archivos)) {
  archivo <- archivos[i]
  
  df_raw <- tryCatch({
    read_excel(archivo, skip = 6, col_types = "text") %>% clean_names()
  }, error = function(e) {
    message(sprintf("   ⚠️ Error leyendo '%s': %s", basename(archivo), e$message))
    NULL
  })
  
  if (is.null(df_raw)) next
  
  # Identificación dinámica de columnas (idéntico a satica_engine.R líneas 53-55)
  col_ing <- names(df_raw)[grepl("ingenio",              names(df_raw))][1]
  col_hda <- names(df_raw)[grepl("hacienda|cod_hda",     names(df_raw))][1]
  col_sue <- names(df_raw)[grepl("suerte|lote|cod_sue",  names(df_raw))][1]
  
  # Verificar que también exista cod_cosecha

  col_cos <- names(df_raw)[grepl("cod_cosecha|codigo_cosecha", names(df_raw))][1]
  
  if (any(is.na(c(col_ing, col_hda, col_sue)))) {
    message(sprintf("   ⚠️ Columnas faltantes en '%s' (ing=%s, hda=%s, sue=%s)",
                    basename(archivo), col_ing, col_hda, col_sue))
    next
  }
  
  # Buscar columna de nombre de hacienda para comparación posterior
  col_nombre_hda <- names(df_raw)[grepl("nombre.*hacienda|nombre_hda|nom_hda", names(df_raw))][1]
  
  # Filtrar solo incendios
  if (!is.na(col_cos)) {
    df_incendios <- df_raw %>%
      filter(toupper(trimws(!!sym(col_cos))) == "I")
  } else {
    # Intentar con cod_cosecha directamente
    if ("cod_cosecha" %in% names(df_raw)) {
      df_incendios <- df_raw %>% filter(toupper(trimws(cod_cosecha)) == "I")
    } else {
      message(sprintf("   ⚠️ Sin columna cod_cosecha en '%s', se omite", basename(archivo)))
      next
    }
  }
  
  if (nrow(df_incendios) == 0) next
  
  # Procesar cada registro
  proc <- df_incendios %>%
    mutate(
      archivo_origen = basename(archivo),
      ing_clean = limpiar_texto(!!sym(col_ing)),
      Cod_ing   = traducir_ingenio(ing_clean),
      
      # BLINDAJE ANTI-TRITURACIÓN (idéntico a satica_engine.R líneas 79-82)
      hda_limpia = toupper(str_replace_all(as.character(!!sym(col_hda)), "[^0-9A-Za-z_]", "")),
      sue_limpia = toupper(str_replace_all(as.character(!!sym(col_sue)), "[^0-9A-Za-z_]", "")),
      Cod_hda_full = str_pad(hda_limpia, width = 6, side = "left", pad = "0"),
      Cod_sue_full = str_pad(sue_limpia, width = 6, side = "left", pad = "0"),
      
      COD_UNICO_14 = paste0(Cod_ing, Cod_hda_full, Cod_sue_full),
      COD_HDA_8    = paste0(Cod_ing, Cod_hda_full),
      
      # Nombre desde el Excel (si existe columna)
      Nombre_Reporte = if (!is.na(col_nombre_hda)) limpiar_texto(!!sym(col_nombre_hda)) else NA_character_
    )
  
  # Separar OK vs especiales
  ok <- proc %>% filter(!is.na(Cod_ing), !is.na(hda_limpia), hda_limpia != "")
  esp <- proc %>% filter(is.na(Cod_ing) | is.na(hda_limpia) | hda_limpia == "")
  
  if (nrow(ok) > 0) registros_ok[[length(registros_ok) + 1]] <- ok
  if (nrow(esp) > 0) casos_especiales[[length(casos_especiales) + 1]] <- esp
}

# Consolidar
todos_incendios <- bind_rows(registros_ok) %>%
  select(COD_UNICO_14, COD_HDA_8, Cod_ing, Cod_hda_full, ing_clean,
         Nombre_Reporte, archivo_origen)

df_casos_especiales <- bind_rows(casos_especiales)

# Universo único de haciendas con incendio
universo_hdas <- todos_incendios %>%
  group_by(COD_HDA_8) %>%
  summarise(
    Cod_ing          = first(Cod_ing),
    Cod_hda_full     = first(Cod_hda_full),
    Ingenio          = nombre_ingenio(first(Cod_ing)),
    ing_clean        = first(ing_clean),
    Nombre_Reporte   = first(na.omit(Nombre_Reporte)),
    COD_UNICO_14_rep = first(COD_UNICO_14),
    n_registros      = n(),
    n_suertes        = n_distinct(COD_UNICO_14),
    archivos         = paste(unique(archivo_origen), collapse = " | "),
    .groups = "drop"
  )

message(sprintf("   ✅ Universo Excel: %d haciendas únicas (COD_HDA_8) con incendio",
                nrow(universo_hdas)))
message(sprintf("   ✅ Total registros de incendio procesados: %d", nrow(todos_incendios)))
message(sprintf("   ⚠️  Casos especiales (no codificables): %d registros", nrow(df_casos_especiales)))


# ═══════════════════════════════════════════════════════
# PASO 2 — PREPARAR LAS CAPAS GEOGRÁFICAS
# ═══════════════════════════════════════════════════════
message("\n🗺️ PASO 2: Preparando capas geográficas...")

# --- Caña_SOR_OK ---
message("   Leyendo Caña_SOR_OK.shp...")
cana_sor_raw <- st_read("capas/Caña_SOR_OK.shp", quiet = TRUE) %>%
  st_transform(4326) %>%
  clean_names()

# Verificar campos disponibles
message(sprintf("   Campos en Caña_SOR_OK: %s", paste(names(cana_sor_raw), collapse = ", ")))

# Colapsar a nivel hacienda
cana_sor_hdas <- cana_sor_raw %>%
  st_drop_geometry() %>%
  group_by(ing_hda) %>%
  summarise(
    NOMBRE_HDA     = first(nombre_hda),
    NOM_DAR        = first(if ("nom_dar" %in% names(.)) nom_dar else NA_character_),
    COS_AUTORIZADA = paste(unique(na.omit(cos)), collapse = " / "),
    n_suertes_capa = n(),
    .groups = "drop"
  )

message(sprintf("   ✅ Caña_SOR_OK: %d haciendas únicas (ING_HDA)", nrow(cana_sor_hdas)))

# --- Suertes_Valle ---
message("   Leyendo Suertes_Valle.shp...")
suertes_valle_raw <- st_read("capas/Suertes_Valle.shp", quiet = TRUE) %>%
  st_transform(4326) %>%
  st_make_valid() %>%
  clean_names()

message(sprintf("   Campos en Suertes_Valle: %s", paste(names(suertes_valle_raw), collapse = ", ")))

# Colapsar a nivel hacienda SIN geometría (para join tabular rápido)
suertes_valle_hdas <- suertes_valle_raw %>%
  st_drop_geometry() %>%
  group_by(ing_hda) %>%
  summarise(
    NOMBRE_HDA     = first(nombre_hda),
    NOM_DAR        = first(if ("nom_dar" %in% names(.)) nom_dar else NA_character_),
    COS_AUTORIZADA = paste(unique(na.omit(cos)), collapse = " / "),
    n_suertes_capa = n(),
    .groups = "drop"
  )

message(sprintf("   ✅ Suertes_Valle: %d haciendas únicas (ING_HDA)", nrow(suertes_valle_hdas)))

# Precomputar centroides por suerte (para cruce espacial con DAR luego)
# Usamos suppressWarnings porque centroides de polígonos en lon/lat generan warnings
message("   Calculando centroides por suerte para cruce espacial...")
suertes_valle_centroides <- suertes_valle_raw %>%
  mutate(centroid_geom = suppressWarnings(st_centroid(geometry))) %>%
  st_drop_geometry() %>%
  st_as_sf(sf_column_name = "centroid_geom")

message(sprintf("   ✅ Centroides calculados: %d suertes", nrow(suertes_valle_centroides)))

# --- Dirección Ambiental Regional ---
message("   Leyendo Dirección_Ambiental_Regional.shp...")
dar_shp <- st_read("capas/Dirección_Ambiental_Regional.shp", quiet = TRUE) %>%
  st_transform(4326) %>%
  st_make_valid() %>%
  clean_names()

message(sprintf("   Campos en DAR: %s", paste(names(dar_shp), collapse = ", ")))

# Identificar el nombre del campo DAR en la capa (una sola vez)
dar_col <- names(dar_shp)[grepl("nom_dar|nombre|dar_name|nom_reg|name", names(dar_shp), ignore.case = TRUE)]
if (length(dar_col) == 0) {
  dar_col <- setdiff(names(dar_shp), attr(dar_shp, "sf_column"))
  dar_col <- dar_col[1]
} else {
  dar_col <- dar_col[1]
}
message(sprintf("   Campo DAR identificado: '%s'", dar_col))


# ═══════════════════════════════════════════════════════
# PASO 3 — CATEGORÍA 1: Localizadas en Caña_SOR_OK
# ═══════════════════════════════════════════════════════
message("\n🏷️ PASO 3: Clasificando Categoría 1 (Caña_SOR_OK)...")

cat1 <- universo_hdas %>%
  inner_join(cana_sor_hdas, by = c("COD_HDA_8" = "ing_hda"))

# Registrar discrepancias de nombre
cat1 <- cat1 %>%
  mutate(
    Nombre_Capa       = limpiar_texto(NOMBRE_HDA),
    Nombre_Reporte    = Nombre_Reporte,
    Nombre_Discrepante = ifelse(
      !is.na(Nombre_Capa) & !is.na(Nombre_Reporte) & Nombre_Capa != Nombre_Reporte,
      "SÍ", "NO"
    ),
    Categoria         = "1 - Localizada en Caña_SOR_OK (DAR Suroriente)",
    DAR               = "DAR Suroriente",
    Fuente            = "Caña_SOR_OK.shp",
    Cosecha_Autorizada = COS_AUTORIZADA,
    Tuvo_Incendio     = "SÍ"
  )

residuales_1 <- universo_hdas %>%
  filter(!COD_HDA_8 %in% cat1$COD_HDA_8)

message(sprintf("   ✅ Categoría 1: %d haciendas", nrow(cat1)))
message(sprintf("   ➡️ Residuales para Paso 4: %d haciendas", nrow(residuales_1)))


# ═══════════════════════════════════════════════════════
# PASO 4 — CATEGORÍAS 2 y 3: Cruce con Suertes_Valle
# ═══════════════════════════════════════════════════════
message("\n🏷️ PASO 4: Clasificando Categorías 2 y 3 (Suertes_Valle + DAR)...")

# Join tabular con Suertes_Valle
match_valle <- residuales_1 %>%
  inner_join(suertes_valle_hdas, by = c("COD_HDA_8" = "ing_hda"))

residuales_2 <- residuales_1 %>%
  filter(!COD_HDA_8 %in% match_valle$COD_HDA_8)

message(sprintf("   Coincidencias en Suertes_Valle: %d", nrow(match_valle)))
message(sprintf("   Sin coincidir en ninguna capa: %d", nrow(residuales_2)))

# Cruce espacial con DAR para determinar a qué DAR pertenecen
if (nrow(match_valle) > 0) {
  # Filtrar centroides de suertes cuya hacienda coincidió
  centroides_match <- suertes_valle_centroides %>%
    filter(ing_hda %in% match_valle$COD_HDA_8)
  
  message(sprintf("   Suertes para cruce DAR: %d", nrow(centroides_match)))
  
  # Cruce espacial: centroide de suerte intersecta DAR
  cruce_dar <- st_join(centroides_match, dar_shp, join = st_intersects, left = TRUE)
  
  # Agregar a nivel hacienda: tomar la DAR más frecuente por hacienda
  cruce_resultado <- cruce_dar %>%
    st_drop_geometry() %>%
    mutate(
      DAR_ASIGNADA = as.character(.data[[dar_col]])
    ) %>%
    group_by(ing_hda) %>%
    summarise(
      DAR_ASIGNADA = {
        dars <- na.omit(DAR_ASIGNADA)
        if (length(dars) == 0) NA_character_
        else names(sort(table(dars), decreasing = TRUE))[1]
      },
      .groups = "drop"
    )
  
  # Unir DAR asignada con los datos
  match_valle <- match_valle %>%
    left_join(cruce_resultado, by = c("COD_HDA_8" = "ing_hda"))
  
  # Clasificar: DAR Suroriente → Cat 2; Otra DAR → Cat 3
  cat2 <- match_valle %>%
    filter(grepl("SURORIENTE|SUR.?ORIENTE", DAR_ASIGNADA, ignore.case = TRUE) |
             is.na(DAR_ASIGNADA)) %>%
    mutate(
      Nombre_Capa       = limpiar_texto(NOMBRE_HDA),
      Nombre_Discrepante = ifelse(
        !is.na(Nombre_Capa) & !is.na(Nombre_Reporte) & Nombre_Capa != Nombre_Reporte,
        "SÍ", "NO"
      ),
      Categoria         = ifelse(
        is.na(DAR_ASIGNADA),
        "2 - En Suertes_Valle, DAR no determinada (posible Suroriente)",
        "2 - En Suertes_Valle, confirmada DAR Suroriente"
      ),
      DAR               = coalesce(DAR_ASIGNADA, "NO DETERMINADA"),
      Fuente            = "Suertes_Valle.shp",
      Cosecha_Autorizada = COS_AUTORIZADA,
      Tuvo_Incendio     = "SÍ"
    )
  
  cat3 <- match_valle %>%
    filter(!grepl("SURORIENTE|SUR.?ORIENTE", DAR_ASIGNADA, ignore.case = TRUE) &
             !is.na(DAR_ASIGNADA)) %>%
    mutate(
      Nombre_Capa       = limpiar_texto(NOMBRE_HDA),
      Nombre_Discrepante = ifelse(
        !is.na(Nombre_Capa) & !is.na(Nombre_Reporte) & Nombre_Capa != Nombre_Reporte,
        "SÍ", "NO"
      ),
      Categoria         = "3 - En Suertes_Valle, otra DAR",
      DAR               = DAR_ASIGNADA,
      Fuente            = "Suertes_Valle.shp",
      Cosecha_Autorizada = COS_AUTORIZADA,
      Tuvo_Incendio     = "SÍ"
    )
} else {
  cat2 <- tibble()
  cat3 <- tibble()
}

message(sprintf("   ✅ Categoría 2 (Suertes_Valle, DAR Suroriente): %d", nrow(cat2)))
message(sprintf("   ✅ Categoría 3 (Suertes_Valle, otra DAR): %d", nrow(cat3)))


# ═══════════════════════════════════════════════════════
# PASO 5 — CATEGORÍA 4: Sin ubicación en ninguna capa
# ═══════════════════════════════════════════════════════
message("\n🏷️ PASO 5: Clasificando Categoría 4 (sin ubicación)...")

cat4 <- residuales_2 %>%
  mutate(
    Nombre_Capa        = NA_character_,
    Nombre_Discrepante = NA_character_,
    NOMBRE_HDA         = NA_character_,
    COS_AUTORIZADA     = NA_character_,
    NOM_DAR            = NA_character_,
    n_suertes_capa     = NA_integer_,
    DAR_ASIGNADA       = NA_character_,
    Categoria          = "4 - Sin ubicación en ninguna capa",
    DAR                = "NO DISPONIBLE",
    Fuente             = "Sin fuente geográfica",
    Cosecha_Autorizada = NA_character_,
    Tuvo_Incendio      = "SÍ"
  )

message(sprintf("   ✅ Categoría 4: %d haciendas", nrow(cat4)))


# ═══════════════════════════════════════════════════════
# PASO 6 — AUDITORÍA DEL CICLO ESTIMADO DE INCENDIO
# ═══════════════════════════════════════════════════════
message("\n📐 PASO 6: Auditoría del ciclo estimado de incendio...")

auditoria <- c(
  "═══════════════════════════════════════════════════════",
  "AUDITORÍA DEL CICLO ESTIMADO DE INCENDIO - SATICA V2.4",
  "═══════════════════════════════════════════════════════",
  sprintf("Fecha del diagnóstico: %s", Sys.Date()),
  "",
  "══════════════════════════════════════",
  "a) CICLO POR SUERTE (CICLO_DIAS_SUE)",
  "══════════════════════════════════════",
  "",
  "FÓRMULA ACTUAL (satica_engine.R, línea 107):",
  "  CICLO_DIAS_SUE = ifelse(n()>1, mean(diff(FECHA)), 365)",
  "",
  "ANÁLISIS:",
  "  1. DEFAULT DE 365 DÍAS CON SOLO 1 EVENTO:",
  "     - PROBLEMA: Asignar 365 días como ciclo predeterminado es arbitrario.",
  "       No hay evidencia empírica de que una suerte con un solo evento",
  "       tenga un ciclo anual. Esto produce FECHA_ESTIMADA = FECHA + 365,",
  "       que puede generar falsas alertas o falsos silencios.",
  "     - RECOMENDACIÓN: Dejar CICLO_DIAS_SUE = NA cuando n() == 1.",
  "       Esto comunica honestamente que NO hay suficiente información para",
  "       estimar un ciclo. En el semáforo, estos casos deberían marcarse",
  "       como 'SIN HISTORIAL SUFICIENTE' en lugar de dar una fecha estimada.",
  "     - IMPACTO: Afecta a todas las suertes con un solo evento de incendio,",
  "       que probablemente son la mayoría del universo.",
  "",
  "  2. mean(diff(FECHA)) VS MEDIANA:",
  "     - PROBLEMA: La media aritmética es sensible a outliers temporales.",
  "       Ejemplo: Si una suerte tuvo incendios en 2019, 2020, 2021, 2024,",
  "       los intervalos son [365, 365, 1095]. La media es 608 días, pero el",
  "       patrón real sugiere un ciclo anual con un intervalo anómalo. La",
  "       mediana (365) capturaría mejor el comportamiento típico.",
  "     - RECOMENDACIÓN: Usar median(diff(FECHA)) para mayor robustez.",
  "       Adicionalmente, considerar descartar intervalos > 2*median como",
  "       outliers antes de calcular la media final.",
  "     - IMPACTO: Moderado. Afecta suertes con 3+ eventos y al menos un",
  "       intervalo anómalo.",
  "",
  "  PROPUESTA DE AJUSTE:",
  "    CICLO_DIAS_SUE = ifelse(n() > 1, as.numeric(median(diff(FECHA))), NA_real_)",
  "",
  "",
  "══════════════════════════════════════════════",
  "b) FRECUENCIA PREDIAL (FRECUENCIA_HDA_DIAS)",
  "══════════════════════════════════════════════",
  "",
  "FÓRMULA ACTUAL (satica_engine.R, líneas 112-121):",
  "  Se agrupan TODAS las fechas de TODAS las suertes de una hacienda,",
  "  se ordenan cronológicamente, y se aplica:",
  "  FRECUENCIA_HDA_DIAS = ifelse(n()>1, mean(diff(FECHA)), 365)",
  "",
  "ANÁLISIS:",
  "  1. SEMÁNTICA DEL INDICADOR:",
  "     - INTENCIÓN APARENTE: Medir 'cada cuánto arde ALGUNA suerte de la",
  "       hacienda', es decir, la frecuencia de incidentes a nivel predial.",
  "     - ¿CUMPLE? SÍ, pero con matices. Al mezclar fechas de suertes",
  "       distintas y calcular diff(), los intervalos resultantes representan",
  "       el tiempo entre cualquier par consecutivo de eventos en el predio,",
  "       sin importar qué suerte haya ardido. Esto es correcto si la",
  "       intención es medir 'actividad incendiaria del predio'.",
  "",
  "  2. PROBLEMA ESTADÍSTICO:",
  "     - Si una hacienda tiene 5 suertes que arden el mismo día (evento",
  "       único con 5 registros), diff() producirá intervalos de 0 días",
  "       entre ellos, arrastrando la media hacia abajo artificialmente.",
  "     - RECOMENDACIÓN: Antes de calcular diff(), reducir a fechas únicas:",
  "       unique(FECHA) para eliminar eventos del mismo día.",
  "     - IMPACTO: Alto en haciendas con muchas suertes que arden en el",
  "       mismo evento.",
  "",
  "  3. MISMO PROBLEMA DE DEFAULT 365:",
  "     - Aplica el mismo análisis del punto (a).",
  "     - RECOMENDACIÓN: Usar NA_real_ como default.",
  "",
  "  PROPUESTA DE AJUSTE:",
  "    fechas_unicas <- unique(sort(FECHA))",
  "    FRECUENCIA_HDA_DIAS = ifelse(",
  "      length(fechas_unicas) > 1,",
  "      as.numeric(median(diff(fechas_unicas))),",
  "      NA_real_",
  "    )",
  "",
  "",
  "══════════════════════════════════════════",
  "c) FECHA_ESTIMADA y DIFF_MESES",
  "══════════════════════════════════════════",
  "",
  "EN satica_engine.R (líneas 169-172):",
  "  FECHA_ESTIMADA = FECHA_ULT_I_SUE + CICLO_DIAS_SUE",
  "  DIFF_MESES = as.numeric(Sys.Date() - FECHA_ESTIMADA) / 30.44",
  "  → Calcula a NIVEL DE SUERTE: cuándo debería arder ESTA suerte de nuevo.",
  "",
  "EN global.R (línea 76):",
  "  FECHA_ESTIMADA = max(FECHA_ULT_I_HDA) + mean(FRECUENCIA_HDA_DIAS)",
  "  → Calcula a NIVEL DE HACIENDA: cuándo debería ocurrir el próximo",
  "    evento en ALGUNA suerte del predio.",
  "",
  "ANÁLISIS DE CONSISTENCIA:",
  "  1. SON DOS CÁLCULOS DIFERENTES PARA DOS NIVELES DISTINTOS:",
  "     - Engine: Predicción suerte × suerte (granularidad fina)",
  "     - Global: Predicción predial (visión operativa agregada)",
  "     - Ambos coexisten: el semáforo del engine usa DIFF_MESES (suerte)",
  "       pero también DIFF_HDA_MESES (hacienda) para la alerta predial.",
  "",
  "  2. INCONSISTENCIA EN EL SEMÁFORO:",
  "     - En satica_engine.R (líneas 179-186), el riesgo final combina",
  "       DIFF_MESES (suerte) y DIFF_HDA_MESES (hacienda) en un solo",
  "       case_when. Pero la prioridad no está explícita:",
  "       · CRITICO si la suerte está en ventana ±1 mes",
  "       · CRITICO también si la hacienda tiene DIFF_HDA >= -0.5",
  "       → Esto puede generar CRITICO por la hacienda aunque la suerte",
  "         individual esté lejos de su ciclo.",
  "",
  "  3. ¿CUÁL ES LA FUENTE DE VERDAD?",
  "     - Para el DASHBOARD (global.R): Se usa la FECHA_ESTIMADA predial.",
  "       Esta es la que ve el usuario en la interfaz Shiny.",
  "     - Para el RDS (satica_engine.R): Se calcula por suerte primero,",
  "       pero luego se agrega en global.R con lógica diferente.",
  "     - RECOMENDACIÓN: Definir explícitamente:",
  "       · FUENTE DE VERDAD OPERATIVA: global.R (nivel hacienda) para",
  "         el semáforo y las alertas del dashboard.",
  "       · FUENTE DE VERDAD ANALÍTICA: satica_engine.R (nivel suerte)",
  "         para análisis detallado y reportes técnicos.",
  "       · Documentar ambos cálculos y su propósito en comentarios claros.",
  "",
  "",
  "══════════════════════════════════════════",
  "d) RESUMEN DE CASOS BORDE",
  "══════════════════════════════════════════",
  "",
  "  CASO 1: Suerte con exactamente 1 evento",
  "    → CICLO_DIAS_SUE = 365 (actual) → genera FECHA_ESTIMADA potencialmente engañosa",
  "    → PROPUESTA: CICLO_DIAS_SUE = NA → FECHA_ESTIMADA = NA → riesgo = 'SIN HISTORIAL'",
  "",
  "  CASO 2: Hacienda con múltiples suertes ardiendo el mismo día",
  "    → diff(FECHA) incluye intervalos de 0 → FRECUENCIA_HDA_DIAS artificialmente baja",
  "    → PROPUESTA: unique(FECHA) antes de diff()",
  "",
  "  CASO 3: Intervalos con outlier temporal (ej: 3 años sin evento)",
  "    → mean(diff()) sesgada hacia arriba",
  "    → PROPUESTA: median(diff()) o filtro de outliers al doble de la mediana",
  "",
  "  CASO 4: Hacienda sin geometría (huérfana) con historial",
  "    → satica_engine hereda nombre/ubicación de suertes hermanas si existen,",
  "      pero si no, quedan sin localización. Los CICLOS se calculan correctamente",
  "      porque son independientes de la geometría.",
  "",
  "  CASO 5: Inconsistencia global.R vs engine en FECHA_ESTIMADA",
  "    → La hacienda puede tener RIESGO=CRITICO por su frecuencia predial",
  "      mientras que ninguna suerte individual está cerca de su ciclo.",
  "    → PROPUESTA: Hacer explícito en el dashboard que el riesgo es",
  "      'CRITICO (predial)' o 'CRITICO (suerte)' para transparencia.",
  ""
)

# Guardar auditoría
writeLines(auditoria, file.path(dir_resultados, "auditoria_ciclo.txt"))
message("   ✅ Auditoría del ciclo guardada en auditoria_ciclo.txt")


# ═══════════════════════════════════════════════════════
# PASO 7 — PRODUCIR RESULTADOS
# ═══════════════════════════════════════════════════════
message("\n📊 PASO 7: Produciendo resultados finales...")

# --- 7a. Tabla maestra ---
columnas_comunes <- c("COD_HDA_8", "COD_UNICO_14_rep", "Nombre_Capa",
                       "Nombre_Reporte", "Nombre_Discrepante", "Ingenio",
                       "DAR", "Categoria", "Fuente", "Cosecha_Autorizada",
                       "Tuvo_Incendio", "n_registros", "n_suertes")

# Asegurar que todas las categorías tengan las mismas columnas
preparar_cat <- function(df, cols) {
  for (col in cols) {
    if (!col %in% names(df)) df[[col]] <- NA
  }
  df %>% select(all_of(cols))
}

tabla_maestra <- bind_rows(
  preparar_cat(cat1, columnas_comunes),
  preparar_cat(cat2, columnas_comunes),
  preparar_cat(cat3, columnas_comunes),
  preparar_cat(cat4, columnas_comunes)
)

# Renombrar para claridad
tabla_maestra <- tabla_maestra %>%
  rename(COD_UNICO_14_Representativo = COD_UNICO_14_rep)

write.csv(tabla_maestra, file.path(dir_resultados, "tabla_maestra_categorias.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
message(sprintf("   ✅ Tabla maestra: %d haciendas clasificadas", nrow(tabla_maestra)))


# --- 7b. Verificación de integridad ---
total_universo <- nrow(universo_hdas)
total_clasificado <- nrow(cat1) + nrow(cat2) + nrow(cat3) + nrow(cat4)

if (total_universo == total_clasificado) {
  message(sprintf("   ✅ INTEGRIDAD OK: %d universo = %d clasificadas (Cat1:%d + Cat2:%d + Cat3:%d + Cat4:%d)",
                  total_universo, total_clasificado, nrow(cat1), nrow(cat2), nrow(cat3), nrow(cat4)))
} else {
  message(sprintf("   ❌ ALERTA INTEGRIDAD: %d universo ≠ %d clasificadas",
                  total_universo, total_clasificado))
}


# --- 7c. Tabla resumen ---
resumen <- tabla_maestra %>%
  group_by(Categoria) %>%
  summarise(
    N_Haciendas = n(),
    Pct = round(n() / total_universo * 100, 2),
    .groups = "drop"
  ) %>%
  bind_rows(
    tibble(Categoria = "TOTAL", N_Haciendas = total_universo, Pct = 100.00)
  )

write.csv(resumen, file.path(dir_resultados, "resumen_categorias.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

message("\n   ══════════════════════════════════════")
message("   RESUMEN DE CATEGORÍAS:")
message("   ══════════════════════════════════════")
for (i in seq_len(nrow(resumen))) {
  message(sprintf("   %s: %d haciendas (%.2f%%)",
                  resumen$Categoria[i], resumen$N_Haciendas[i], resumen$Pct[i]))
}


# --- 7d. Análisis cruzado cosecha vs incendio ---
message("\n   Generando análisis cruzado cosecha vs incendio...")

# Para las haciendas que SÍ están en alguna capa (cat1, cat2, cat3), analizar COS
hdas_con_capa <- tabla_maestra %>%
  filter(!is.na(Cosecha_Autorizada) & Cosecha_Autorizada != "")

contingencia <- hdas_con_capa %>%
  mutate(
    Tipo_Cosecha = case_when(
      grepl("[Vv]erde",                Cosecha_Autorizada) &
        !grepl("[Qq]uema|[Aa]utorizada.*[Qq]uema", Cosecha_Autorizada) ~ "Solo Cosecha en Verde",
      grepl("[Qq]uema",                Cosecha_Autorizada) &
        !grepl("[Vv]erde",             Cosecha_Autorizada) ~ "Solo Quema Autorizada",
      grepl("[Vv]erde",                Cosecha_Autorizada) &
        grepl("[Qq]uema",              Cosecha_Autorizada) ~ "Mixta (Verde + Quema)",
      TRUE ~ "Otro/No clasificado"
    )
  ) %>%
  group_by(Tipo_Cosecha, Tuvo_Incendio) %>%
  summarise(N = n(), .groups = "drop") %>%
  mutate(
    Pct_Total = round(N / total_universo * 100, 2),
    Interpretacion = case_when(
      Tipo_Cosecha == "Solo Cosecha en Verde" & Tuvo_Incendio == "SÍ" ~
        "⚠️ Posible quema no autorizada o evento no programado",
      Tipo_Cosecha %in% c("Solo Quema Autorizada", "Mixta (Verde + Quema)") & Tuvo_Incendio == "SÍ" ~
        "✅ Coherente con lo autorizado",
      Tuvo_Incendio == "NO" ~
        "Sin registro de incendio en Excel",
      TRUE ~ "Requiere revisión manual"
    )
  )

write.csv(contingencia, file.path(dir_resultados, "contingencia_cosecha_incendio.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

message("\n   ══════════════════════════════════════")
message("   TABLA DE CONTINGENCIA: COSECHA vs INCENDIO")
message("   ══════════════════════════════════════")
for (i in seq_len(nrow(contingencia))) {
  message(sprintf("   %s | Incendio: %s → %d haciendas (%.2f%%) → %s",
                  contingencia$Tipo_Cosecha[i],
                  contingencia$Tuvo_Incendio[i],
                  contingencia$N[i],
                  contingencia$Pct_Total[i],
                  contingencia$Interpretacion[i]))
}


# --- 7d bis. Haciendas en capas SIN incendio registrado ---
# Identificar haciendas que están en las capas pero NO en el universo Excel
message("\n   Identificando haciendas en capas sin incendio registrado...")

hdas_capa_sin_incendio_sor <- cana_sor_hdas %>%
  filter(!ing_hda %in% universo_hdas$COD_HDA_8) %>%
  mutate(
    Fuente = "Caña_SOR_OK.shp",
    Tuvo_Incendio = "NO"
  )

hdas_capa_sin_incendio_valle <- suertes_valle_hdas %>%
  filter(!ing_hda %in% universo_hdas$COD_HDA_8 &
           !ing_hda %in% cana_sor_hdas$ing_hda) %>%
  mutate(
    Fuente = "Suertes_Valle.shp",
    Tuvo_Incendio = "NO"
  )

message(sprintf("   → Haciendas en Caña_SOR_OK SIN incendio: %d", nrow(hdas_capa_sin_incendio_sor)))
message(sprintf("   → Haciendas en Suertes_Valle (exclusivas) SIN incendio: %d",
                nrow(hdas_capa_sin_incendio_valle)))

# Contingencia incluyendo las que NO tuvieron incendio
contingencia_completa <- bind_rows(
  contingencia,
  hdas_capa_sin_incendio_sor %>%
    mutate(
      Tipo_Cosecha = case_when(
        grepl("[Vv]erde",  COS_AUTORIZADA) & !grepl("[Qq]uema", COS_AUTORIZADA) ~ "Solo Cosecha en Verde",
        grepl("[Qq]uema",  COS_AUTORIZADA) & !grepl("[Vv]erde", COS_AUTORIZADA) ~ "Solo Quema Autorizada",
        grepl("[Vv]erde",  COS_AUTORIZADA) &  grepl("[Qq]uema", COS_AUTORIZADA) ~ "Mixta (Verde + Quema)",
        TRUE ~ "Otro/No clasificado"
      ),
      Tuvo_Incendio = "NO"
    ) %>%
    group_by(Tipo_Cosecha, Tuvo_Incendio) %>%
    summarise(N = n(), .groups = "drop") %>%
    mutate(
      Pct_Total = round(N / (total_universo + nrow(hdas_capa_sin_incendio_sor) +
                               nrow(hdas_capa_sin_incendio_valle)) * 100, 2),
      Interpretacion = "Sin registro de incendio en Excel (existente en capa)"
    )
)

write.csv(contingencia_completa,
          file.path(dir_resultados, "contingencia_completa_con_sin_incendio.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")


# --- 7e. Casos especiales ---
if (nrow(df_casos_especiales) > 0) {
  cols_esp <- intersect(
    c("archivo_origen", "ing_clean", "Cod_ing", "hda_limpia", "sue_limpia"),
    names(df_casos_especiales)
  )
  if (length(cols_esp) > 0) {
    write.csv(df_casos_especiales %>% select(any_of(cols_esp)),
              file.path(dir_resultados, "casos_especiales.csv"),
              row.names = FALSE, fileEncoding = "UTF-8")
  } else {
    write.csv(df_casos_especiales,
              file.path(dir_resultados, "casos_especiales.csv"),
              row.names = FALSE, fileEncoding = "UTF-8")
  }
  message(sprintf("\n   ✅ Casos especiales exportados: %d registros", nrow(df_casos_especiales)))
} else {
  message("\n   ✅ Sin casos especiales (todos los registros fueron codificables)")
  writeLines("Sin casos especiales", file.path(dir_resultados, "casos_especiales.csv"))
}


# --- 7f. Discrepancias de nombres ---
discrepancias <- tabla_maestra %>%
  filter(Nombre_Discrepante == "SÍ") %>%
  select(COD_HDA_8, Nombre_Capa, Nombre_Reporte, Ingenio, Categoria)

if (nrow(discrepancias) > 0) {
  write.csv(discrepancias, file.path(dir_resultados, "discrepancias_nombres.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  message(sprintf("   ✅ Discrepancias de nombres exportadas: %d haciendas", nrow(discrepancias)))
} else {
  message("   ✅ Sin discrepancias de nombres detectadas")
}


# ═══════════════════════════════════════════════════════
# CIERRE
# ═══════════════════════════════════════════════════════
message("\n═══════════════════════════════════════════════════════")
message("✅ DIAGNÓSTICO DE COBERTURA COMPLETADO")
message("═══════════════════════════════════════════════════════")
message(sprintf("📁 Resultados guardados en: %s/", dir_resultados))
message(sprintf("📋 Total haciendas con incendio (universo): %d", total_universo))
message(sprintf("🗺️ Categoría 1 (Caña_SOR_OK):           %d (%.1f%%)",
                nrow(cat1), nrow(cat1)/total_universo*100))
message(sprintf("🗺️ Categoría 2 (Suertes_Valle-SOR):     %d (%.1f%%)",
                nrow(cat2), nrow(cat2)/total_universo*100))
message(sprintf("🗺️ Categoría 3 (Suertes_Valle-otra DAR): %d (%.1f%%)",
                nrow(cat3), nrow(cat3)/total_universo*100))
message(sprintf("❓ Categoría 4 (sin ubicación):          %d (%.1f%%)",
                nrow(cat4), nrow(cat4)/total_universo*100))
message(sprintf("⚠️  Casos especiales:                    %d registros",
                nrow(df_casos_especiales)))
message(sprintf("📐 Auditoría de ciclos en: auditoria_ciclo.txt"))
message("═══════════════════════════════════════════════════════")
