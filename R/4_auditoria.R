library(dplyr); library(readxl); library(janitor); library(stringr)

# 1. Función para Caja Azul (Huérfanos DAR)
calcular_huerfanos_dar <- function(datos_mapa, datos_excel) {
  codigos_mapa <- unique(datos_mapa$COD_UNICO)
  
  municipios_dar <- c("PALMIRA", "PRADERA", "FLORIDA", "CANDELARIA", "CERRITO", "EL CERRITO")
  
  datos_excel %>%
    filter(!COD_UNICO %in% codigos_mapa) %>%
    # Filtro DAR Suroriente (Columna 5)
    filter(XLS_MUNICIPIO %in% municipios_dar) %>%
    select(MUNICIPIO=XLS_MUNICIPIO, INGENIO=XLS_INGENIO, HACIENDA=XLS_HACIENDA, HISTORIAL=Total_Incendios_Hist, FECHAS=Rango_Fechas) %>%
    arrange(MUNICIPIO, desc(HISTORIAL))
}

# 2. Función para Caja Vino Tinto (Hoja de Vida Admin)
procesar_hoja_vida_admin <- function(ruta_admin, datos_mapa) {
  tryCatch({
    # Leer Excel Específico (Col D, P, Q, S)
    # D=4 (Fecha), P=16 (Muni), Q=17 (Correg), S=19 (Hacienda)
    raw <- read_excel(ruta_admin, col_names = FALSE, skip = 2) # Asumimos cabecera en fila 2
    
    # Seleccionamos por índice fijo según imagen
    df <- raw %>%
      select(
        RAW_FECHA = 4, 
        RAW_MUNI = 16, 
        RAW_CORREG = 17, 
        RAW_HDA = 19
      ) %>%
      filter(!is.na(RAW_HDA)) %>%
      mutate(
        # Limpieza para Triangulación
        HDA_CLEAN = str_replace_all(toupper(str_trim(as.character(RAW_HDA))), "HACIENDA|HDA|S.A.|\\.", ""),
        HDA_CLEAN = str_trim(HDA_CLEAN),
        
        MUNI_CLEAN = toupper(str_trim(as.character(RAW_MUNI))),
        CORREG_CLEAN = toupper(str_trim(as.character(RAW_CORREG))),
        
        FECHA = suppressWarnings(janitor::excel_numeric_to_date(as.numeric(RAW_FECHA)))
      )
    
    # Filtro DAR
    municipios_dar <- c("PALMIRA", "PRADERA", "FLORIDA", "CANDELARIA", "CERRITO", "EL CERRITO")
    df <- df %>% filter(MUNI_CLEAN %in% municipios_dar)
    
    if(nrow(df) == 0) return(data.frame())
    
    return(df %>% select(FECHA, MUNICIPIO=MUNI_CLEAN, CORREGIMIENTO=CORREG_CLEAN, HACIENDA=RAW_HDA))
    
  }, error = function(e) {
    print(paste("Error leyendo Excel Admin:", e$message))
    return(data.frame())
  })
}