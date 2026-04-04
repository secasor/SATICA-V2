# ----------------------------------------------------
# Archivo: R/geo_join.R - Versión Limpia (Sin filtros internos)
# Objetivo: Unir mapas conservando TODAS las columnas originales
# ----------------------------------------------------

library(sf)
library(dplyr)

spatial_join_haciendas <- function(haciendas_sf, corregimientos_sf) {
  
  # 1. Capturar el CRS de la capa de Haciendas
  CRS_BASE <- st_crs(haciendas_sf) 
  
  # 2. Reproyectar la capa de Corregimientos al mismo sistema
  corregimientos_reproyectados <- corregimientos_sf %>%
    st_transform(crs = CRS_BASE)
  
  # 3. Realizar la unión espacial SIN FILTROS (Trae todas las columnas)
  haciendas_con_info <- haciendas_sf %>%
    # Solo renombramos las claves del shapefile de haciendas para evitar conflictos
    rename(ING_SHP = ING, HDA_SHP = HDA, STE_SHP = STE) %>%
    st_join(
      corregimientos_reproyectados, 
      join = st_intersects, 
      left = TRUE
    )
  
  return(haciendas_con_info)
}