# =========================================================================
# SATICA V2.0 - Instalador de Dependencias Globales
# =========================================================================
# Este script verifica e instala automáticamente todas las librerías
# necesarias para ejecutar de forma local la aplicación SATICA V2.0.

paquetes_requeridos <- c(
  "shiny", "bs4Dash", "leaflet", "sf", "dplyr", "tidyr", "lubridate",
  "stringr", "stringi", "readxl", "readr", "openxlsx", "DT",
  "shinyWidgets", "janitor", "lwgeom", "httr", "rmarkdown", 
  "pacman", "purrr", "xgboost", "Matrix", "reticulate", "rgee", "plotly"
)

# Identificar cuáles faltan
paquetes_nuevos <- paquetes_requeridos[!(paquetes_requeridos %in% installed.packages()[,"Package"])]

# Instalar si hay algún faltante
if(length(paquetes_nuevos)) {
  message("📦 SATICA V2: Instalando dependencias faltantes...")
  message(paste(paquetes_nuevos, collapse = ", "))
  install.packages(paquetes_nuevos, repos = "https://cran.rstudio.com/")
} else {
  message("✅ SATICA V2: Todas las dependencias requeridas están instaladas.")
}
