library(sf); library(dplyr); library(stringr)

sf::sf_use_s2(FALSE)

preparar_geometria_base <- function(ruta_shp, ruta_correg) {
  print("   -> [MÓDULO GEO] Cargando capas (V12000 - Nearest Fix)...")
  
  if(!file.exists(ruta_shp)) stop("No se encuentra el archivo SHP")
  base <- st_read(ruta_shp, quiet = TRUE) %>% st_make_valid()
  correg <- st_read(ruta_correg, quiet = TRUE) %>% st_make_valid()
  
  if (is.na(st_crs(base)$epsg)) st_crs(base) <- 4326
  if (is.na(st_crs(correg)$epsg)) st_crs(correg) <- 4326
  base <- st_transform(base, 4326)
  correg <- st_transform(correg, 4326)
  
  # --- 1. EXTRACCIÓN ESTRICTA POR POSICIÓN (MAPA CAÑA) ---
  # Col 3: Nombre Hda, Col 4: Ingenio, Col 7: Cod Hda, Col 8: Cod Ste
  c_nom_hda <- names(base)[3]
  c_ing     <- names(base)[4]
  c_cod_hda <- names(base)[7]
  c_cod_ste <- names(base)[8]
  
  # --- 2. EXTRACCIÓN ESTRICTA POR POSICIÓN (CORREGIMIENTOS) ---
  # Col 3: NOM_MUNICI, Col 5: NOM_DIV_PO
  c_mun_nom <- names(correg)[3]
  c_cor_nom <- names(correg)[5]
  
  # --- 3. CRUCE ESPACIAL INTELIGENTE (CENTROIDES + CERCANÍA) ---
  puntos_centro <- st_centroid(base)
  
  # A. Cruce normal (Intersección)
  cruce_pts <- st_join(puntos_centro, correg, join = st_intersects, left = TRUE)
  
  # B. Corrección de "Huérfanos Espaciales" (Bordes, Ríos)
  # Si quedó NA en Municipio, buscamos el polígono más cercano
  idx_sin_ubic <- which(is.na(cruce_pts[[c_mun_nom]]))
  
  if(length(idx_sin_ubic) > 0) {
    print(paste("      [GEO] Corrigiendo", length(idx_sin_ubic), "suertes en bordes (Nearest Feature)..."))
    # Encuentra el índice del polígono más cercano en 'correg' para cada punto huérfano
    indices_cercanos <- st_nearest_feature(puntos_centro[idx_sin_ubic,], correg)
    
    # Asignar valores del más cercano
    cruce_pts[idx_sin_ubic, c_mun_nom] <- correg[[c_mun_nom]][indices_cercanos]
    cruce_pts[idx_sin_ubic, c_cor_nom] <- correg[[c_cor_nom]][indices_cercanos]
  }
  
  # Transferir datos limpios a la base
  base$RAW_MUNIC  <- cruce_pts[[c_mun_nom]]
  base$RAW_CORREG <- cruce_pts[[c_cor_nom]]
  
  final <- base %>%
    mutate(
      # Visuales
      MAPA_HACIENDA = str_trim(as.character(.data[[c_nom_hda]])),
      MAPA_INGENIO  = str_trim(as.character(.data[[c_ing]])),
      
      # Ubicación (Garantizada sin NAs)
      GEO_MUNICIPIO = str_to_title(str_trim(as.character(RAW_MUNIC))),
      GEO_CORREGIM  = str_to_title(str_trim(as.character(RAW_CORREG))),
      
      # Limpieza Matemática
      CODE_ING = toupper(MAPA_INGENIO),
      INT_HDA = suppressWarnings(as.integer(as.numeric(as.character(.data[[c_cod_hda]])))),
      INT_STE = suppressWarnings(as.integer(as.numeric(as.character(.data[[c_cod_ste]])))),
      
      INT_HDA = ifelse(is.na(INT_HDA), 0, INT_HDA),
      INT_STE = ifelse(is.na(INT_STE), 0, INT_STE),
      
      COD_UNICO = paste(CODE_ING, INT_HDA, INT_STE, sep="-"),
      UID_GEO = paste0("UID_", row_number())
    )
  
  # Coordenadas
  suppressWarnings({
    coords <- st_coordinates(st_centroid(final))
    final$LAT <- coords[,2]
    final$LONG <- coords[,1]
  })
  
  final <- final %>% 
    select(UID_GEO, COD_UNICO, 
           INGENIO = MAPA_INGENIO, 
           HACIENDA = MAPA_HACIENDA, 
           SUERTE = INT_STE, 
           NOMBRE_MUNICIPIO = GEO_MUNICIPIO, 
           NOMBRE_CORREGIMIENTO = GEO_CORREGIM, 
           LAT, LONG, geometry)
  
  print(paste("      [GEO] Procesado con éxito. Registros:", nrow(final)))
  return(final)
}