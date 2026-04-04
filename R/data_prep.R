# ----------------------------------------------------
# Archivo: R/data_prep.R - Versión V8 (Corrección Definitiva de Ingesta y Target de Regresión de Días)
# ----------------------------------------------------

library(readxl)
library(dplyr)
library(janitor)
library(purrr)
library(lubridate)
library(sf)

# NOTA: Este script requiere que source("aux_functions.R") se ejecute primero.

# --- FUNCIÓN DE CÁLCULO DE DÍAS HASTA EL PRÓXIMO INCENDIO (TARGET) ---
crear_variable_dias_evento <- function(df) {
  
  df_con_dias <- df %>%
    arrange(COD_UNICO, fecha_dato) %>%
    group_by(COD_UNICO) %>%
    
    # 1. Identificar la fecha del próximo incendio (solo eventos 'I')
    mutate(
      # Encuentra la fecha del próximo evento 'I' en la misma unidad
      fecha_proximo_incendio = lead(fecha_dato, 
                                    order_by = if_else(cod_cosecha == "I", 
                                                       fecha_dato, 
                                                       as.Date(NA_real_)))
    ) %>%
    
    # 2. Rellenar las fechas con la fecha del próximo incendio
    fill(fecha_proximo_incendio, .direction = "down") %>%
    
    # 3. Calcular la diferencia en días.
    mutate(
      dias_hasta_siguiente_incendio = as.numeric(fecha_proximo_incendio - fecha_dato)
    ) %>%
    ungroup() %>%
    select(-fecha_proximo_incendio)
  
  return(df_con_dias)
}


# --- FUNCIÓN PRINCIPAL DE INGESTIÓN Y PREPARACIÓN ---
leer_y_preparar_datos <- function(ruta_carpeta, horizonte_dias = 7) {
  
  # 1. LISTADO DE ARCHIVOS
  archivos_excel <- list.files(path = ruta_carpeta, pattern = "\\.xlsx$|\\.xls$", full.names = TRUE, recursive = TRUE)
  if (length(archivos_excel) == 0) { stop("Error: No se encontraron archivos Excel.") }
  
  # 2. DEFINICIÓN DE POSICIONES CLAVE
  POSICIONES_CLAVE <- list(
    col_nombre_ingenio = 1, col_fecha_dato = 3, col_cod_hacienda = 4, 
    col_cod_suerte = 8, col_otra_feature = 6, col_area_predicha = 7,
    col_cod_cosecha = 2
  )
  
  # 3. FUNCIÓN DE LECTURA Y RENOMBRADO POR POSICIÓN
  leer_y_limpiar_archivo <- function(ruta_archivo) {
    
    # Lectura inicial, dejando que read_excel adivine el tipo
    data_raw <- read_excel(ruta_archivo, col_names = TRUE)
    data_clean <- data_raw %>% janitor::clean_names()
    
    if (ncol(data_clean) < 8) { warning("Archivo saltado por pocas columnas"); return(NULL) }
    
    data_final <- data_clean %>%
      select(
        nombre_ingenio_completo = POSICIONES_CLAVE$col_nombre_ingenio,
        fecha_dato              = POSICIONES_CLAVE$col_fecha_dato,
        cod_hacienda            = POSICIONES_CLAVE$col_cod_hacienda,
        cod_suerte              = POSICIONES_CLAVE$col_cod_suerte,
        feature_A               = POSICIONES_CLAVE$col_otra_feature, # Corregimiento
        area_predicha           = POSICIONES_CLAVE$col_area_predicha,
        cod_cosecha             = POSICIONES_CLAVE$col_cod_cosecha
      ) 
    
    # CRÍTICO: CONVERSIÓN DE FECHA MÁS ROBUSTA (Maneja texto y número de Excel)
    fecha_col <- data_final$fecha_dato
    
    data_final <- data_final %>%
      mutate(
        # 1. Intentar convertir la columna a numérica (formato de Excel)
        num_dates = suppressWarnings(as.numeric(fecha_col)),
        
        # 2. Intentar convertir la columna a fecha con el formato confirmado (%Y/%m/%d)
        char_dates = suppressWarnings(as.Date(as.character(fecha_col), format = "%Y/%m/%d")),
        
        # 3. Utilizar coalesce para priorizar la conversión de número (Excel)
        # y si falla (NA), usar el resultado de la conversión de texto.
        fecha_dato = coalesce(
          excel_numeric_to_date(num_dates, date_system = "modern"),
          char_dates
        ),
        
        cod_hacienda = as.character(cod_hacienda),
        cod_suerte = as.character(cod_suerte),
        cod_cosecha = toupper(as.character(cod_cosecha)),
        feature_A = as.character(feature_A) # Corregimiento como carácter
      ) %>%
      select(-num_dates, -char_dates) # Limpiar columnas auxiliares
    
    return(data_final)
  }
  
  # 4. CONSOLIDACIÓN
  datos_consolidados <- purrr::map_dfr(archivos_excel, leer_y_limpiar_archivo, .id = "source")
  
  # 5. CREACIÓN DE LA CLAVE COD_UNICO
  datos_consolidados <- datos_consolidados %>% create_cod_unico()
  
  # 6. FILTRAR REGISTROS INVÁLIDOS
  datos_consolidados <- datos_consolidados %>% filter(!is.null(COD_UNICO) & !is.na(nombre_ingenio_completo)) 
  
  # 7. CÁLCULO DE LA RECURRENCIA HISTÓRICA
  datos_con_recurrencia <- datos_consolidados %>%
    mutate(incendio_real = if_else(cod_cosecha == "I", 1, 0)) %>%
    arrange(fecha_dato) %>%
    group_by(COD_UNICO) %>%
    mutate(
      recurrencia_incendios_acumulada = lag(cumsum(incendio_real), default = 0)
    ) %>%
    ungroup() %>%
    select(-incendio_real)
  
  # 8. CREACIÓN DEL TARGET DE REGRESIÓN: DÍAS HASTA EL SIGUIENTE EVENTO
  datos_con_target <- crear_variable_dias_evento(datos_con_recurrencia) %>%
    # Filtramos la data: solo nos interesa entrenar con registros que NO SON incendios 
    # y que tienen un evento futuro (dias_hasta_siguiente_incendio no es NA).
    filter(cod_cosecha != "I" & !is.na(dias_hasta_siguiente_incendio))
  
  return(datos_con_target)
}