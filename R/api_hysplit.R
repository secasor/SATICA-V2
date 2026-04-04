# ==============================================================================
# MODELACIÓN PROSPECTIVA DE PLUMAS DE HUMO HYSPLIT (SATICA V3.0)
# ==============================================================================
# Propósito: Calcular trayectorias predictivas de dispersión de humo (12h)
# simulando conatos hipotéticos en las haciendas con nivel CRÍTICO inminente.
# Permite anticipar el impacto poblacional en el Boletín Quincenal de la CVC.
# ==============================================================================

if (!requireNamespace("splitr", quietly = TRUE)) {
  message("Instalando paquete splitr desde GitHub (rich-iannone/splitr)...")
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  remotes::install_github("rich-iannone/splitr")
}

library(splitr)
library(dplyr)
library(sf)
library(lubridate)
library(purrr)

message("🌬️ INICIANDO MODELADOR PREDICTIVO DE HUMO HYSPLIT (12 HORAS)...")

# 1. Cargar Base Maestra con Semáforo Consolidado
master_rds_path <- "data_master/SATICA_MASTER_v2.2.rds"

if (!file.exists(master_rds_path)) {
  message("❌ ERROR: No se encontró SATICA_MASTER_v2.2.rds. Abortando modelo.")
  saveRDS(NULL, "data_master/HYSPLIT_plumas.rds")
  return(invisible(NULL))
}

# 2. Extraer Top 5 de Vulnerabilidad Absoluta (CRÍTICOS y Vencidos)
# Nos enfocamos en las haciendas que ya excedieron su ciclo (DIFF_MESES negativo o 0)
# o que ya activaron sequedad/fuego.
master_db <- readRDS(master_rds_path) %>%
  st_drop_geometry() %>%
  # Limpiamos para obtener lat/lon central de la hacienda que está en rojo
  filter(riesgo == "CRITICO") %>%
  # Evitamos que incendios múltiples dominen, nos quedamos a nivel hacienda o suerte inminente
  distinct(cod_hda_key, .keep_all = TRUE) %>%
  # Priorizamos las haciendas que llevan más tiempo con la ventana abierta (DIFF más negativo)
  # O las que muestran Alerta_Combustion Crítica
  arrange(DIFF_MESES) %>%
  head(5)

if (nrow(master_db) == 0) {
  message("✅ Nivel de Riesgo Regional Óptimo. Sin simulaciones críticas requeridas.")
  saveRDS(NULL, "data_master/HYSPLIT_plumas.rds")
  return(invisible(NULL))
}

message(sprintf("🌪️ Simulando Proyección de Humo para las %d haciendas más críticas...", nrow(master_db)))

# 3. Configurar Parámetros del Escenario de Quema (Hipotético)
# Consideramos el mediodía local de hoy (cuando los vientos térmicos suelen levantar ceniza)
hora_simulacion <- round_date(Sys.time(), unit = "day") + hours(12)

# 4. Ejecutar HYSPLIT (Modelo Forward - Simulación de Propagación)
# Altura simplificada a 50m (elevación media de pluma inicial de caña) para agilizar.
# Duración: 6 horas hacia adelante (óptimo sugerido para sobrepasar fronteras municipales).
lista_trayectorias <- map(1:nrow(master_db), function(i) {
  predio <- master_db[i, ]
  
  message(sprintf("   ▶ Analizando vulnerabilidad poblacional de: %s", predio$hda_nombre))
  
  tryCatch({
    trayectoria <- hysplit_trajectory(
      lat = predio$lat,
      lon = predio$lon,
      height = 50,               # Elevación única y optimizada (50 metros)
      duration = 6,              # Impacto en las siguientes 6 horas
      days = as.character(as.Date(hora_simulacion)),
      daily_hours = hour(hora_simulacion),
      direction = "forward",     # Propagación prospectiva
      met_type = "gdas1",        # Clima global 1-grado (~100km)
      extended_met = TRUE,
      met_dir = "met_data",
      exec_dir = "hysplit_exec"
    )
    
    # Asignar código para renderización UI identificando la Hacienda, no solo la suerte.
    trayectoria_df <- trayectoria %>% as.data.frame() %>% mutate(cod_hda_key = predio$cod_hda_key, hda_nombre = predio$hda_nombre)
    return(trayectoria_df)
    
  }, error = function(e) {
    message(sprintf("   ⚠️ Falla de simulación en %s: %s", predio$cod_unico, e$message))
    return(NULL)
  })
})

trayectorias_prev <- bind_rows(lista_trayectorias[!sapply(lista_trayectorias, is.null)])

# 5. Exportar a RDS
if (nrow(trayectorias_prev) > 0) {
  # Convertir tabla de puntos a vectores continuos LINESTRING
  trayectorias_sf <- trayectorias_prev %>%
    arrange(cod_hda_key, hour_along) %>%
    group_by(cod_hda_key, hda_nombre) %>%
    filter(n() > 1) %>% # Mínimo 2 puntos vitales
    summarise(do_union = FALSE, .groups = "drop") %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    group_by(cod_hda_key, hda_nombre) %>%
    summarise(geometry = st_cast(st_combine(geometry), "LINESTRING"), .groups = "drop") %>%
    # Añadimos meta "height" a 50m para compatibilidad con server.R
    mutate(height = 50)
  
  saveRDS(trayectorias_sf, "data_master/HYSPLIT_plumas.rds")
  message("✅ Modelación Preventiva (HYSPLIT) completada exitosamente.")
} else {
  message("✅ Trayectorias no concluidas debido a red climatológica. Usando fallback.")
  saveRDS(NULL, "data_master/HYSPLIT_plumas.rds")
}

return(invisible(NULL))
