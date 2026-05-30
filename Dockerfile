# ==============================================================================
# DOCKERFILE — SATICA V2.0 (HUGGING FACE SPACES EDITION)
# ==============================================================================
# Propósito: Construir un contenedor Linux ultra-estable con R 4.4 y soporte GIS 
# completo (GDAL/GEOS) para correr SATICA en la nube 24/7 sin caídas por RAM.
# ==============================================================================

FROM rocker/r-ver:4.4.0

# 1. Instalar dependencias del sistema operativo (GIS y red robustos)
RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libxt-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    git \
    && rm -rf /var/lib/apt/lists/*

# 2. Configurar directorio de trabajo en la imagen
WORKDIR /app

# 3. Utilizar RSPM (Posit Package Manager) para instalar binarios de R super rápido
RUN R -e 'options(repos = c(RSPM = "https://packagemanager.posit.co/cran/__linux__/jammy/latest", CRAN = "https://cloud.r-project.org"))'

# 4. Instalar paquetes de R requeridos para SATICA
RUN R -e 'install.packages(c("shiny", "bs4Dash", "dplyr", "tidyr", "leaflet", "sf", "readxl", "lubridate", "DT", "shinyWidgets", "stringr", "janitor", "stringi", "lwgeom", "readr", "rmarkdown", "openxlsx", "ggplot2", "visNetwork"), repos="https://packagemanager.posit.co/cran/__linux__/jammy/latest")'

# 5. Copiar todo el código fuente de SATICA al contenedor
COPY . /app

# 6. Definir variables de entorno de producción y puerto HF
ENV ON_CLOUD=true
ENV PORT=7860
EXPOSE 7860

# 7. Asignar permisos seguros al contenedor
RUN chmod -R 777 /app

# 8. Comando para encender Shiny en el puerto 7860 (Hugging Face standard)
CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=7860)"]
