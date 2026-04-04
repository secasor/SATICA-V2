# ==============================================================================
# MÓDULO: SATICA - CORRECCIÓN ESPACIAL HÍBRIDA (V31000 - GEO FIRST)
# UBICACIÓN: R/mod_correccion_espacial.R
# DESCRIPCIÓN: Prioriza validación geométrica (recupera 22k) + respaldo texto.
# ==============================================================================

library(sf)
library(stringi)
library(dplyr)
library(stringdist)

corregir_ubicacion_satica <- function(df_reporte, shp_cana, shp_corr, umbral = 0.25) {
  
  message(">> [SATICA V3.1] Iniciando validación Híbrida (Geo-Prioridad)...")
  
  # --- 1. FUNCIÓN DE LIMPIEZA RÁPIDA ---
  limpiar_vector <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x <- toupper(x)
    x <- stri_trans_general(x, "Latin-ASCII")
    return(trimws(x))
  }
  
  # Preparar salida
  df_out <- df_reporte
  
  # Asegurar ID único temporal para poder unir después
  df_out$ID_TEMP_VALIDACION <- 1:nrow(df_out)
  df_out$Estado_Validacion <- "Pendiente"
  
  # Detectar columnas objetivo en el reporte
  col_mun_rep <- "Municipio"
  col_cor_rep <- "Corregimiento"
  
  # --- PASO 1: VALIDACIÓN ESPACIAL MASIVA (LA CLAVE DE LOS 22K) ---
  # Si el dato tiene geometría (es un mapa), cruzamos contra municipios DIRECTAMENTE.
  
  es_espacial <- inherits(df_out, "sf")
  ids_validados_geo <- c()
  
  if (es_espacial) {
    message("   -> Detectada capa espacial. Ejecutando Cruce Geográfico Masivo...")
    
    # Asegurar proyecciones
    if (st_crs(df_out) != st_crs(shp_corr)) {
      df_out <- st_transform(df_out, st_crs(shp_corr))
    }
    
    # 1. Calculamos centroides rápidos para el cruce (más rápido que polígono vs polígono)
    suppressWarnings({
      pts_reporte <- st_point_on_surface(df_out)
    })
    
    # 2. JOIN ESPACIAL (Magic Moment)
    # Cruzamos los 22,000 puntos contra el mapa de corregimientos de una sola vez
    cruce_geo <- st_join(pts_reporte, shp_corr, join = st_intersects)
    
    # Detectamos columnas del mapa administrativo
    cols_mapa <- names(cruce_geo)
    nm_mun <- cols_mapa[grep("MUNICI", cols_mapa, ignore.case = TRUE)][1]
    nm_corr <- cols_mapa[grep("DIV_PO|CORREG", cols_mapa, ignore.case = TRUE)][1]
    
    # 3. Asignar resultados espaciales
    # Filtramos los que SÍ cayeron dentro de algún municipio
    cruce_exitoso <- cruce_geo %>% 
      filter(!is.na(!!sym(nm_mun))) %>%
      st_drop_geometry() %>%
      select(ID_TEMP_VALIDACION, MUN_GEO = !!sym(nm_mun), CORR_GEO = !!sym(nm_corr))
    
    # Actualizamos el dataframe original
    if(nrow(cruce_exitoso) > 0) {
      df_out <- df_out %>%
        left_join(cruce_exitoso, by = "ID_TEMP_VALIDACION") %>%
        mutate(
          # Si hubo match espacial, sobrescribimos Municipio/Corregimiento con la verdad del mapa
          !!sym(col_mun_rep) := ifelse(!is.na(MUN_GEO), limpiar_vector(MUN_GEO), !!sym(col_mun_rep)),
          !!sym(col_cor_rep) := ifelse(!is.na(CORR_GEO), limpiar_vector(CORR_GEO), !!sym(col_cor_rep)),
          
          # Marcamos como validado
          Estado_Validacion = ifelse(!is.na(MUN_GEO), "OK - Validado (Geometría)", Estado_Validacion)
        ) %>%
        select(-MUN_GEO, -CORR_GEO)
      
      ids_validados_geo <- cruce_exitoso$ID_TEMP_VALIDACION
      message(paste("   -> [GEO] Registros validados espacialmente:", length(ids_validados_geo)))
    }
  }
  
  # --- PASO 2: VALIDACIÓN POR TEXTO (SOLO PARA HUÉRFANOS) ---
  # Solo procesamos lo que NO se validó por mapa (o si no tenía mapa)
  
  df_pendientes <- df_out %>% filter(!ID_TEMP_VALIDACION %in% ids_validados_geo)
  n_pendientes <- nrow(df_pendientes)
  
  if (n_pendientes > 0) {
    message(paste("   -> [TEXTO] Intentando validar por nombre los restantes:", n_pendientes))
    
    # --- PREPARAR TABLA MAESTRA (IGUAL QUE ANTES) ---
    cols_shp <- names(shp_cana)
    col_ing_shp <- cols_shp[grep("^ING", cols_shp, ignore.case = TRUE)][1]
    col_hac_shp <- cols_shp[grep("^NOMBRE_HDA|^HDA", cols_shp, ignore.case = TRUE)][1]
    
    if (!is.na(col_ing_shp) && !is.na(col_hac_shp)) {
      
      # Tabla de referencia (Nombres oficiales)
      ref_nombres <- shp_cana %>%
        st_drop_geometry() %>%
        select(KEY_ING = !!sym(col_ing_shp), KEY_HAC = !!sym(col_hac_shp)) %>%
        mutate(
          KEY_ING = limpiar_vector(KEY_ING),
          KEY_HAC = limpiar_vector(KEY_HAC),
          NOMBRE_OFICIAL = !!sym(col_hac_shp)
        ) %>%
        distinct(KEY_ING, KEY_HAC, .keep_all = TRUE)
      
      # Preparar pendientes
      col_ing_rep <- "Ingenio"
      col_hac_rep <- "Hacienda"
      
      df_pendientes <- df_pendientes %>%
        mutate(
          TEMP_ING = limpiar_vector(!!sym(col_ing_rep)),
          TEMP_HAC = limpiar_vector(!!sym(col_hac_rep))
        )
      
      # JOIN POR TEXTO EXACTO
      match_txt <- df_pendientes %>%
        inner_join(ref_nombres, by = c("TEMP_ING" = "KEY_ING", "TEMP_HAC" = "KEY_HAC")) %>%
        select(ID_TEMP_VALIDACION, NOMBRE_OFICIAL)
      
      # Actualizar pendientes que hicieron match
      if(nrow(match_txt) > 0) {
        df_out <- df_out %>%
          left_join(match_txt, by = "ID_TEMP_VALIDACION") %>%
          mutate(
            !!sym(col_hac_rep) := ifelse(!is.na(NOMBRE_OFICIAL), NOMBRE_OFICIAL, !!sym(col_hac_rep)),
            Estado_Validacion = ifelse(!is.na(NOMBRE_OFICIAL) & Estado_Validacion == "Pendiente", 
                                       "OK - Validado (Texto Exacto)", Estado_Validacion)
          ) %>%
          select(-NOMBRE_OFICIAL)
      }
    }
  }
  
  # Limpieza final
  df_out <- df_out %>% 
    select(-ID_TEMP_VALIDACION)
  
  # Rellenar estado final para los que fallaron todo
  df_out$Estado_Validacion[df_out$Estado_Validacion == "Pendiente"] <- "ALERTA: Sin Ubicación Confirmada"
  
  message(">> [SATICA V3.1] Proceso finalizado.")
  return(df_out)
}