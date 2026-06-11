# ==============================================================================
# SCRIPT DE ACTUALIZACIÓN AUTOMÁTICA - SATICA 2.0
# ==============================================================================
# Propósito: Ejecutar todo el flujo de datos (Satélites + Motor) en un solo clic.
# ==============================================================================

message("🚀 INICIANDO ACTUALIZACIÓN INTEGRAL DE SATICA 2.0...")


# 0. Verificación e Instalación de Dependencias Críticas
if (file.exists("instalar_dependencias.R")) {
  source("instalar_dependencias.R", encoding = "UTF-8")
} else {
  message("⚠️ ADVERTENCIA: No se encontró 'instalar_dependencias.R'. Procure tener todas las librerías instaladas.")
}

# 1. Actualización de Alertas Térmicas (NASA FIRMS)
message("\n--- [1/3] Consultando Satélites NASA FIRMS (24h) ---")
tryCatch(
  {
    source("R/api_nasa_firms.R", encoding = "UTF-8")
  },
  error = function(e) {
    message("⚠️ Error en NASA FIRMS: ", e$message)
  }
)

# 1.5. Actualización de Alertas Dinámicas (GOES-16)
message("\n--- [1.5/3] Consultando Satélite GOES-16 (Anomalías Térmicas 1h) ---")
tryCatch(
  {
    source("R/api_goes16.R", encoding = "UTF-8")
  },
  error = function(e) {
    message("⚠️ Error en GOES-16: ", e$message)
  }
)

# 2. Actualización de Biomasa (Sentinel-2 via Google Earth Engine)
message("\n--- [2/3] Consultando Biomasa Sentinel-2 (Indices NDVI/NBR) ---")
tryCatch(
  {
    if (requireNamespace("rgee", quietly = TRUE)) {
      source("R/api_sentinel_rgee.R", encoding = "UTF-8")
    } else {
      message("ℹ️ Saltando Sentinel-2: El paquete 'rgee' no está instalado.")
    }
  },
  error = function(e) {
    message("⚠️ Error en Sentinel-2: ", e$message, "\n(Asegúrese de tener configurado Google Earth Engine)")
  }
)

# 3. Consolidación del Motor SATICA (Generación del Master RDS)
message("\n--- [3/3] Ejecutando Motor de Consolidación y Predicción ---")
tryCatch(
  {
    source("satica_engine.R", encoding = "UTF-8")
  },
  error = function(e) {
    stop("❌ Error Fatal en el Motor SATICA: ", e$message)
  }
)

message("\n✅ ACTUALIZACIÓN COMPLETADA CON ÉXITO.")
message("Presione 'Run App' en RStudio para ver los resultados en el Dashboard.")

# Opcional: Descomente la siguiente línea si desea que la app se abra automáticamente
# shiny::runApp()
