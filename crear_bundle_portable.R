# ==============================================================================
# SATICA V2.0 — GENERADOR DE BUNDLE PORTÁTIL
# ==============================================================================
# Propósito: Crear una carpeta autocontenida que incluye R-Portable + todos
# los paquetes necesarios para ejecutar SATICA en CUALQUIER PC Windows
# sin necesidad de instalar R, RStudio, ni ningún otro programa.
#
# USO:
#   1. Ejecute este script UNA VEZ en un PC que YA tenga R instalado
#   2. El script descargará R-Portable y empaquetará todo automáticamente
#   3. Copie la carpeta resultante (SATICA_Portable/) a un USB o compártala
#
# REQUISITOS:
#   - Conexión a Internet (para descargar R-Portable y paquetes)
#   - R >= 4.4 instalado en ESTE PC
#   - ~2 GB de espacio libre en disco
# ==============================================================================

message("
╔══════════════════════════════════════════════════════════════╗
║         SATICA V2.0 — Generador de Bundle Portátil          ║
║         Corporación Autónoma Regional del Valle del Cauca    ║
╚══════════════════════════════════════════════════════════════╝
")

# --- CONFIGURACIÓN ---
R_VERSION       <- paste0(R.version$major, ".", R.version$minor)  # Usa la misma versión que el R actual
R_PORTABLE_URL  <- paste0("https://cloud.r-project.org/bin/windows/base/R-", R_VERSION, "-win.exe")
BUNDLE_DIR      <- file.path(getwd(), "SATICA_Portable")
R_PORTABLE_DIR  <- file.path(BUNDLE_DIR, "R-Portable")
APP_DIR         <- file.path(BUNDLE_DIR, "SATICA")
LIB_DIR         <- file.path(R_PORTABLE_DIR, "library")

# Paquetes requeridos por SATICA (idénticos a instalar_dependencias.R)
PAQUETES_SATICA <- c(
  "shiny", "bs4Dash", "leaflet", "sf", "dplyr", "tidyr", "lubridate",
  "stringr", "stringi", "readxl", "readr", "openxlsx", "DT",
  "shinyWidgets", "janitor", "lwgeom", "httr", "rmarkdown",
  "pacman", "purrr", "xgboost", "Matrix", "ggplot2", "visNetwork"
)

# ==============================================================================
# PASO 1: Crear estructura de carpetas
# ==============================================================================
message("\n📂 [1/5] Creando estructura de carpetas...")
if (dir.exists(BUNDLE_DIR)) {
  message("  ⚠️  La carpeta SATICA_Portable/ ya existe. Se actualizará.")
} else {
  dir.create(BUNDLE_DIR, recursive = TRUE)
}
if (!dir.exists(APP_DIR)) dir.create(APP_DIR, recursive = TRUE)

# ==============================================================================
# PASO 2: Descargar R-Portable
# ==============================================================================
message("\n📥 [2/5] Preparando R-Portable...")

if (dir.exists(R_PORTABLE_DIR) && file.exists(file.path(R_PORTABLE_DIR, "bin", "Rscript.exe"))) {
  message("  ✅ R-Portable ya existe. Saltando descarga.")
} else {
  # Estrategia: copiar la instalación local de R en vez de descargar
  # Esto es más rápido y garantiza compatibilidad al 100%
  R_HOME_LOCAL <- R.home()
  message(paste0("  📋 Copiando R local desde: ", R_HOME_LOCAL))
  message("  ⏳ Esto puede tomar 2-3 minutos...")
  
  if (dir.exists(R_PORTABLE_DIR)) unlink(R_PORTABLE_DIR, recursive = TRUE)
  
  tryCatch({
    # Copiar toda la instalación de R
    dir.create(R_PORTABLE_DIR, recursive = TRUE)
    
    # Copiar directorios esenciales
    dirs_to_copy <- c("bin", "etc", "include", "lib", "library", 
                       "modules", "share", "doc")
    for (d in dirs_to_copy) {
      src <- file.path(R_HOME_LOCAL, d)
      dst <- file.path(R_PORTABLE_DIR, d)
      if (dir.exists(src)) {
        message(paste0("    Copiando ", d, "/..."))
        file.copy(src, R_PORTABLE_DIR, recursive = TRUE, overwrite = TRUE)
      }
    }
    
    # Copiar archivos raíz
    root_files <- list.files(R_HOME_LOCAL, full.names = TRUE, recursive = FALSE)
    root_files <- root_files[!file.info(root_files)$isdir]
    file.copy(root_files, R_PORTABLE_DIR, overwrite = TRUE)
    
    message("  ✅ R-Portable creado exitosamente.")
  }, error = function(e) {
    stop(paste("❌ Error copiando R:", e$message))
  })
}

# ==============================================================================
# PASO 3: Instalar paquetes en la librería portable
# ==============================================================================
message("\n📦 [3/5] Instalando paquetes en la librería portátil...")
LIB_DIR <- file.path(R_PORTABLE_DIR, "library")

# Verificar cuáles faltan en la librería portátil
paquetes_instalados <- list.dirs(LIB_DIR, full.names = FALSE, recursive = FALSE)
paquetes_faltantes <- PAQUETES_SATICA[!(PAQUETES_SATICA %in% paquetes_instalados)]

if (length(paquetes_faltantes) > 0) {
  message(paste0("  📋 Paquetes a instalar: ", paste(paquetes_faltantes, collapse = ", ")))
  message("  ⏳ Esto puede tomar 5-10 minutos...")
  
  install.packages(
    paquetes_faltantes,
    lib = LIB_DIR,
    repos = "https://cran.rstudio.com/",
    type = "win.binary",
    dependencies = TRUE,
    quiet = TRUE
  )
  message("  ✅ Paquetes instalados.")
} else {
  message("  ✅ Todos los paquetes ya están presentes.")
}

# Copiar paquetes que podrían estar en la librería del usuario pero no en la de R
user_lib_pkgs <- .libPaths()[1]
for (pkg in PAQUETES_SATICA) {
  pkg_dst <- file.path(LIB_DIR, pkg)
  if (!dir.exists(pkg_dst)) {
    pkg_src <- file.path(user_lib_pkgs, pkg)
    if (dir.exists(pkg_src)) {
      message(paste0("  📋 Copiando ", pkg, " desde librería de usuario..."))
      file.copy(pkg_src, LIB_DIR, recursive = TRUE, overwrite = TRUE)
    }
  }
}

# ==============================================================================
# PASO 4: Copiar archivos de SATICA
# ==============================================================================
message("\n📁 [4/5] Copiando archivos de SATICA al bundle...")

# Archivos y carpetas a incluir
items_to_copy <- c(
  "global.R", "server.R", "ui.R", "satica_engine.R",
  "instalar_dependencias.R", "aux_functions.R",
  "centinela_satelital.R", "actualizar_y_ejecutar.R",
  "generar_documentos.R", "descargar_reportes.R",
  "reporte.Rmd",
  "informe_ejecutivo_satica.Rmd", "informe_incidencia_antropica.Rmd",
  "presentacion_satica.Rmd", "presentacion_satica_evolucion.Rmd",
  "presentacion_incidencia_antropica.Rmd",
  "sin_georref_coords.csv",
  "MANUAL_INTEGRAL_SATICA.md", "GUIA_OPERACION_SATICA.md",
  ".gitignore"
)

dirs_to_copy <- c(
  "R", "capas", "data_master", "data_estatica", "modelo_rds",
  "www", "reportes_cosecha", "reportes", "reportes_alerta",
  "reportes_finales", "resultados_diagnostico", "resultados_modelo",
  "actas"
)

# Copiar archivos individuales
for (item in items_to_copy) {
  src <- file.path(getwd(), item)
  if (file.exists(src)) {
    file.copy(src, APP_DIR, overwrite = TRUE)
  }
}

# Copiar directorios
for (d in dirs_to_copy) {
  src <- file.path(getwd(), d)
  dst <- file.path(APP_DIR, d)
  if (dir.exists(src)) {
    if (dir.exists(dst)) unlink(dst, recursive = TRUE)
    message(paste0("    Copiando ", d, "/..."))
    file.copy(src, APP_DIR, recursive = TRUE, overwrite = TRUE)
  }
}

# Copiar visitas si existen
if (file.exists("visitas_cvc.csv")) {
  file.copy("visitas_cvc.csv", APP_DIR, overwrite = TRUE)
}

message("  ✅ Archivos de SATICA copiados.")

# ==============================================================================
# PASO 5: Crear Lanzador Inteligente
# ==============================================================================
message("\n🚀 [5/5] Creando lanzador portátil...")

launcher_bat <- '
@echo off
chcp 65001 > nul
title SATICA V2.0 - Sistema de Alerta Temprana (Modo Portátil)
color 0A

echo.
echo ╔══════════════════════════════════════════════════════════════╗
echo ║           SATICA V2.0 - Sistema de Alertas Tempranas        ║
echo ║         Corporacion Autonoma Regional del Valle del Cauca   ║
echo ║                      MODO PORTATIL                          ║
echo ╚══════════════════════════════════════════════════════════════╝
echo.
echo Iniciando Dashboard...
echo (El navegador se abrira automaticamente. NO cierre esta ventana.)
echo.

set "APP_DIR=%~dp0SATICA"
set "RSCRIPT=%~dp0R-Portable\\bin\\Rscript.exe"

if not exist "%RSCRIPT%" (
    echo [ERROR] No se encontro R-Portable en la carpeta esperada.
    echo          Verifique que la carpeta R-Portable\\ existe junto a este archivo.
    pause
    exit /b 1
)

if not exist "%APP_DIR%\\global.R" (
    echo [ERROR] No se encontro la aplicacion SATICA en la carpeta esperada.
    echo          Verifique que la carpeta SATICA\\ existe junto a este archivo.
    pause
    exit /b 1
)

cd /d "%APP_DIR%"
"%RSCRIPT%" --vanilla -e "shiny::runApp('"'"'.'"'"', launch.browser = TRUE, port = 3838)"

echo.
echo ══════════════════════════════════════════════════════════════
echo La aplicacion se ha detenido.
echo Si hubo un error, revise los mensajes anteriores.
echo ══════════════════════════════════════════════════════════════
pause
'

# Arreglar las comillas simples para el comando R
launcher_bat <- gsub("'\"'\"'", "'", launcher_bat, fixed = TRUE)

# Escribir el .bat con encoding correcto
writeLines(launcher_bat, file.path(BUNDLE_DIR, "INICIAR_SATICA.bat"), useBytes = FALSE)

# Crear LEEME
readme_txt <- '
╔══════════════════════════════════════════════════════════════╗
║         SATICA V2.0 - VERSIÓN PORTÁTIL                      ║
║         Corporación Autónoma Regional del Valle del Cauca    ║
╚══════════════════════════════════════════════════════════════╝

═══════════════════════════════════════════════════════════════
  INSTRUCCIONES DE USO
═══════════════════════════════════════════════════════════════

1. Descomprima esta carpeta en cualquier ubicación de su PC
   (Escritorio, USB, carpeta personal, etc.)

2. Haga doble clic en "INICIAR_SATICA.bat"

3. El dashboard se abrirá automáticamente en su navegador
   (Chrome, Edge, Firefox)

4. NO cierre la ventana negra de consola mientras usa SATICA

5. Para cerrar: cierre la pestaña del navegador y luego
   la ventana de consola (o presione Ctrl+C)


═══════════════════════════════════════════════════════════════
  REQUISITOS
═══════════════════════════════════════════════════════════════

- Sistema Operativo: Windows 10 o superior
- Navegador web: Chrome, Edge o Firefox (actualizado)
- NO necesita instalar R, RStudio ni ningún otro programa
- Conexión a Internet SOLO necesaria para actualizar datos
  satelitales (funciona offline con datos previos)


═══════════════════════════════════════════════════════════════
  ESTRUCTURA DE LA CARPETA
═══════════════════════════════════════════════════════════════

SATICA_Portable/
├── INICIAR_SATICA.bat    ← Doble clic aquí para iniciar
├── LEEME.txt             ← Este archivo
├── R-Portable/           ← R + paquetes (no modificar)
└── SATICA/               ← Código y datos de la aplicación
    ├── capas/            ← Capas geográficas
    ├── data_master/      ← Datos satelitales actualizados
    └── ...


═══════════════════════════════════════════════════════════════
  SOPORTE
═══════════════════════════════════════════════════════════════

Para soporte técnico, contacte al equipo DAR Suroriente.
Versión del bundle generada el: %FECHA%
'

readme_txt <- gsub("%FECHA%", as.character(Sys.Date()), readme_txt)
writeLines(readme_txt, file.path(BUNDLE_DIR, "LEEME.txt"), useBytes = FALSE)

message("  ✅ Lanzador y documentación creados.")

# ==============================================================================
# RESUMEN FINAL
# ==============================================================================
# Calcular tamaño
total_size_mb <- sum(file.info(
  list.files(BUNDLE_DIR, recursive = TRUE, full.names = TRUE)
)$size, na.rm = TRUE) / (1024^2)

message("
╔══════════════════════════════════════════════════════════════╗
║              ✅ BUNDLE PORTÁTIL GENERADO                    ║
╠══════════════════════════════════════════════════════════════╣
")
message(sprintf("  📂 Ubicación: %s", BUNDLE_DIR))
message(sprintf("  📊 Tamaño total: %.0f MB", total_size_mb))
message("
  📋 Próximos pasos:
     1. Comparta la carpeta SATICA_Portable/ (ZIP o USB)
     2. El destinatario solo necesita hacer doble clic en
        INICIAR_SATICA.bat para iniciar el dashboard
     3. NO necesita instalar R ni ningún otro programa

╚══════════════════════════════════════════════════════════════╝
")
