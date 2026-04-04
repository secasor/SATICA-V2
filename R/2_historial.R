library(readxl); library(dplyr); library(stringr); library(janitor); library(lubridate)

obtener_historial_real <- function(ruta_carpeta = "reportes_cosecha/") {
  print("   -> [MÓDULO HISTORIAL] V18000: Leyendo Excel con Jerarquía Administrativa (Cols 5 y 6)...")
  
  archivos <- list.files(ruta_carpeta, pattern = "\\.xls", full.names = TRUE, recursive = TRUE)
  archivos <- archivos[!grepl("^~", basename(archivos))]
  
  if(length(archivos) == 0) return(data.frame(COD_UNICO = character(0)))
  
  lista <- list()
  dicc <- c("MANUELITA"="MN", "INCAUCA"="IN", "PROVIDENCIA"="PR", "MAYAGUEZ"="MY", "PICHICHI"="PI", "RISARALDA"="RI", "CARMELITA"="CM", "MARIA LUISA"="ML", "SANCARLOS"="SC", "TUMACO"="TU", "OCCIDENTE"="OC", "LA CABAÑA"="CB")
  
  for(f in archivos) {
    try({
      # Leer cabecera para encontrar inicio de datos
      raw_head <- read_excel(f, col_names = FALSE, n_max = 50, .name_repair = "minimal")
      skip <- 0
      for(i in 1:nrow(raw_head)) {
        txt <- tolower(paste(raw_head[i,], collapse=" "))
        if(grepl("ingenio", txt) && grepl("hacienda", txt) && grepl("suerte", txt)) { skip <- i-1; break }
      }
      
      temp <- read_excel(f, skip = skip, col_types = "text", .name_repair = "minimal")
      
      if(ncol(temp) >= 13) {
        df_sel <- temp %>% select(
          RAW_ING = 1, 
          RAW_TIPO = 2, 
          RAW_FECHA = 3, 
          RAW_HDA = 4, 
          RAW_MUN_RPT = 5, # Col 5: Municipio Reporte (LA VERDAD)
          RAW_COR_RPT = 6, # Col 6: Corregimiento Reporte (NUEVO V18)
          RAW_HDA_NOM = 7, 
          RAW_STE = 8, 
          RAW_EDAD = 13
        )
        
        df_sel <- df_sel %>% mutate(
          ing_norm = toupper(str_trim(as.character(RAW_ING))),
          CODE_ING = ifelse(ing_norm %in% names(dicc), dicc[ing_norm], substr(ing_norm, 1, 2)),
          
          INT_HDA = suppressWarnings(as.integer(as.numeric(as.character(RAW_HDA)))),
          INT_STE = suppressWarnings(as.integer(as.numeric(as.character(RAW_STE)))),
          
          COD_UNICO = paste(CODE_ING, INT_HDA, INT_STE, sep="-"),
          
          # --- LIMPIEZA DATOS ADMINISTRATIVOS ---
          MUN_CLEAN = toupper(str_trim(as.character(RAW_MUN_RPT))),
          COR_CLEAN = toupper(str_trim(as.character(RAW_COR_RPT))), # Nuevo
          
          TIPO_CLEAN = toupper(str_trim(as.character(RAW_TIPO))),
          ES_INCENDIO = TIPO_CLEAN == "I",
          ES_CORTE_VALIDO = TIPO_CLEAN %in% c("I", "Q", "V"),
          
          DT = suppressWarnings(case_when(
            !is.na(as.numeric(RAW_FECHA)) ~ as.Date(as.numeric(RAW_FECHA), origin="1899-12-30"),
            TRUE ~ parse_date_time(RAW_FECHA, c("ymd", "dmy", "mdy", "y-m-d", "d/m/Y")) %>% as.Date()
          )),
          ANIO = year(DT),
          
          # Lógica V13000 de Decimales (Se mantiene)
          EDAD_RAW_NUM = suppressWarnings(as.numeric(gsub(",", ".", as.character(RAW_EDAD)))),
          EDAD_VAL = case_when(
            is.na(EDAD_RAW_NUM) ~ 0,
            EDAD_RAW_NUM > 10000 ~ EDAD_RAW_NUM / 10000,
            EDAD_RAW_NUM > 100 ~ EDAD_RAW_NUM / 100,
            TRUE ~ EDAD_RAW_NUM
          ),
          EDAD_VAL = ifelse(EDAD_VAL > 36, 0, EDAD_VAL)
        )
        
        df_sel <- df_sel %>% filter(!is.na(INT_HDA) & !is.na(INT_STE))
        lista[[f]] <- df_sel
      }
    }, silent=TRUE)
  }
  
  final <- bind_rows(lista)
  
  resumen <- final %>% 
    filter(!is.na(COD_UNICO)) %>%
    group_by(COD_UNICO) %>%
    summarise(
      XLS_HACIENDA = first(RAW_HDA_NOM),
      XLS_INGENIO = first(RAW_ING),
      
      # DATOS ADMINISTRATIVOS OFICIALES (REPORTADOS)
      XLS_MUNICIPIO = first(MUN_CLEAN),     
      XLS_CORREGIMIENTO = first(COR_CLEAN), 
      
      Total_Incendios_Hist = sum(ES_INCENDIO, na.rm=TRUE),
      Promedio_Ciclo = mean(EDAD_VAL[EDAD_VAL > 0], na.rm=TRUE), 
      
      Ultima_Fecha_Corte = if(any(ES_CORTE_VALIDO)) max(DT[ES_CORTE_VALIDO], na.rm=TRUE) else as.Date(NA),
      
      Ultimo_Tipo_Evento = if(any(ES_CORTE_VALIDO)) {
        tipos_validos <- TIPO_CLEAN[ES_CORTE_VALIDO]
        fechas_validas <- DT[ES_CORTE_VALIDO]
        tipos_validos[which.max(fechas_validas)]
      } else { "Sin Info" },
      
      Rango_Fechas = paste(min(ANIO,na.rm=T), "-", max(ANIO,na.rm=T))
    ) %>% ungroup() %>%
    mutate(
      Promedio_Ciclo = ifelse(is.na(Promedio_Ciclo) | Promedio_Ciclo == 0, 13, round(Promedio_Ciclo, 2)),
      Ultimo_Tipo_Evento = ifelse(is.na(Ultima_Fecha_Corte), "Sin Dato", Ultimo_Tipo_Evento)
    )
  
  print("      [HISTORIAL] Datos limpios. Jerarquía administrativa lista.")
  return(resumen)
}