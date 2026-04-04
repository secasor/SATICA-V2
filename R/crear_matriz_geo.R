# ==============================================================================
# MATRIZ ANALITICA ESPACIAL ESTATICA (SATICA V3.0)
# ==============================================================================
# Propósito: Calcular por UNICA VEZ la distancia en metros desde cada suerte a:
# 1. Vías Intermunicipales
# 2. Centros Poblados
# 3. Zonas Forestales (Bosques)
# Requisito para: Inyectar inteligencia antrópica en el modelo XGBoost V9.
# ==============================================================================

library(sf)
library(dplyr)

sf_use_s2(FALSE) # Cálculos planos para alto rendimiento

message("🚀 Iniciando Motor de Generación de Matriz Geográfica Estática...")

# EPSG:3115 corresponde a Colombia Magna-SIRGAS / Oeste, para que as.numeric() devuelva METROS reales y no grados de Lat/Lon.
crs_metros <- 3115

# 1. CARGA DE METADATOS MAESTROS (Cultivos)
message("🗺️ Cargando Mapa Maestro de Cultivos...")
mapa_cana <- st_read("capas/Caña_SOR_OK.shp", quiet = TRUE) %>% 
  janitor::clean_names() %>%
  st_transform(crs_metros)

# Acelerador Computacional: Usar centroides de la caña para medir distancia (Punto a Polígono es 10x más rápido que Polígono a Polígono)
centroides_cana <- suppressWarnings(st_centroid(mapa_cana))

# --- Función de Alto Rendimiento para Nearest Neighbor ---
# Encuentra la figura más cercana en milisegundos y calcula la mínima distancia espacial asíncrona.
calcular_distancia_minima <- function(puntos_sf, destino_sf, nombre_variable) {
  message(sprintf("   -> Calculando distancias espaciales hacia: %s", nombre_variable))
  
  # st_nearest_feature localiza el índice del polígono o línea más cercana
  idx_cercanos <- st_nearest_feature(puntos_sf, destino_sf)
  
  # Calcula la distancia exacta solo entre el punto y ese polígono específico
  distancias <- st_distance(puntos_sf, destino_sf[idx_cercanos, ], by_element = TRUE)
  
  return(as.numeric(distancias))
}

# 2. CARGA Y CÁLCULO DE RESTRICCIONES
# ====== A. VÍAS INTERMUNICIPALES ======
vias <- st_read("capas/restricciones/Vias_Intermunicipales.shp", quiet = TRUE) %>% 
  st_transform(crs_metros)
dist_vias <- calcular_distancia_minima(centroides_cana, vias, "Vías Intermunicipales")
rm(vias); gc()

# ====== B. CENTROS POBLADOS ======
poblados <- st_read("capas/restricciones/Centros_Poblados_V2.shp", quiet = TRUE) %>% 
  st_transform(crs_metros)
dist_poblados <- calcular_distancia_minima(centroides_cana, poblados, "Centros Poblados")
rm(poblados); gc()

# ====== C. ZONAS FORESTALES PROTEGIDAS ======
bosques <- st_read("capas/restricciones/Área Forestal Protectora (R_Q_Ar).shp", quiet = TRUE) %>% 
  st_transform(crs_metros)
dist_bosques <- calcular_distancia_minima(centroides_cana, bosques, "Área Forestal Protectora")
rm(bosques); gc()

# 3. ENSAMBLAJE DE MATRIZ Y EXPORTACIÓN
message("📊 Ensamblando el ADN Geográfico de Restricciones...")

matriz_distancias <- tibble(
  cod_unico = mapa_cana$cod_unico,
  dist_vias_m = dist_vias,
  dist_poblados_m = dist_poblados,
  dist_bosques_m = dist_bosques
) %>% distinct(cod_unico, .keep_all = TRUE)

# Guardar la matriz compilada
if(!dir.exists("data_master")) dir.create("data_master")
saveRDS(matriz_distancias, "data_master/matriz_distancias_cana.rds")

message("✅ PROCESO COMPLETADO: Matriz Estática Generada en `data_master/matriz_distancias_cana.rds`.")
message("Muestra aleatoria:")
print(head(matriz_distancias))
