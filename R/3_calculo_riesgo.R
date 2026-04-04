library(dplyr); library(sf); library(lubridate)

calcular_riesgo_operativo <- function(df, modelo_xgb = NULL) {
  # V20000: Etiquetas Inteligentes (Smart Labels) en Días.
  
  print("   -> [RIESGO] V20000: Calculando Etiquetas Humanas (Días vs Meses)...")
  
  if(nrow(df) == 0) return(df)
  
  df_riesgo <- df %>%
    mutate(
      # --- PASO 0: CALCULO DE VARIABLES BASE ---
      Tiene_Datos = !is.na(Ultima_Fecha_Corte),
      Dias_Dif = as.numeric(Sys.Date() - Ultima_Fecha_Corte),
      
      Edad_Meses_Actual = case_when(
        !Tiene_Datos ~ 0,
        is.na(Dias_Dif) ~ 0,
        Dias_Dif < 0 ~ 0,
        TRUE ~ Dias_Dif / 30.44
      ),
      
      Meses_Faltantes = Promedio_Ciclo - Edad_Meses_Actual,
      
      # --- PASO 1: ALGORITMO EXPERTO ---
      Nivel_Riesgo = case_when(
        !Tiene_Datos ~ "Bajo",
        Edad_Meses_Actual > 24 ~ "Auditoría",
        Total_Incendios_Hist == 0 ~ "Bajo",
        Total_Incendios_Hist > 0 & Meses_Faltantes > 3 ~ "Medio",
        Total_Incendios_Hist > 0 & Meses_Faltantes >= 1 ~ "Alto",
        Total_Incendios_Hist > 0 & Meses_Faltantes < 1 ~ "Crítico",
        TRUE ~ "Bajo"
      ),
      
      # --- PASO 2: Colores ---
      Color_Hex = case_when(
        Nivel_Riesgo == "Crítico" ~ "#d32f2f", 
        Nivel_Riesgo == "Alto"    ~ "#f57c00", 
        Nivel_Riesgo == "Medio"   ~ "#fbc02d", 
        Nivel_Riesgo == "Bajo"    ~ "#388e3c", 
        TRUE                      ~ "#7b1fa2"
      ),
      
      # --- PASO 3: ETIQUETAS INTELIGENTES (SMART LABELS) ---
      # Aquí convertimos los decimales feos en texto útil para humanos.
      Texto_Tiempo_Faltante = case_when(
        # Caso A: Negativo (Ya se pasó) -> Muestra días de vencimiento
        Meses_Faltantes < 0 ~ paste0("<b style='color:red'>⚠️ Vencida hace ", round(abs(Meses_Faltantes * 30.44), 0), " días</b>"),
        
        # Caso B: Menos de 1 mes -> Muestra días faltantes
        Meses_Faltantes < 1 ~ paste0("<b>Faltan ", round(Meses_Faltantes * 30.44, 0), " días</b>"),
        
        # Caso C: Normal -> Muestra meses
        TRUE ~ paste0("Faltan ", round(Meses_Faltantes, 1), " meses")
      ),
      
      # --- PASO 4: Popup Final ---
      Popup_Info = paste0(
        "<b>", HACIENDA, " / ", SUERTE, "</b><br>",
        "Municipio: ", NOMBRE_MUNICIPIO, " (", NOMBRE_CORREGIMIENTO, ")<br>",
        "-----------------------<br>",
        "<b>Riesgo: ", Nivel_Riesgo, "</b><br>",
        "Edad Actual: ", round(Edad_Meses_Actual, 1), " meses<br>",
        "Ciclo Histórico: ", round(Promedio_Ciclo, 1), " meses<br>",
        "Estado: ", Texto_Tiempo_Faltante, "<br>", # <--- NUEVA ETIQUETA
        "<b>Hist. Incendios: ", Total_Incendios_Hist, "</b>"
      )
    )
  
  return(df_riesgo)
}