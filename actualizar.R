# ==============================================================================
# SCRIPT DE ACTUALIZACIÓN Y SINCRONIZACIÓN — SATICA V2.0 (MODO ONE-CLICK)
# ==============================================================================
# Propósito: Ejecutar todo el motor local y sincronizar cambios automáticamente
# con GitHub. Diseñado para evitar bloqueos corporativos de archivos .bat en OneDrive.
# Instrucciones: Abre este archivo en RStudio y presiona el botón "Source" 
# (o ejecuta source("actualizar.R") en la Consola).
# ==============================================================================

message("\n==========================================================")
message("🛰️  INICIANDO PROCESO INTEGRAL DE ACTUALIZACIÓN SATICA")
message("==========================================================\n")

# --- 1. VERIFICAR CONEXIÓN Y EJECUTAR ACTUALIZACIONES DE TELEMETRÍA ---
.hay_internet <- function() {
  tryCatch({
    con <- url("https://www.google.com", open = "r")
    close(con)
    TRUE
  }, error = function(e) FALSE)
}

if (.hay_internet()) {
  message("📡 Conexión a Internet: DETECTADA.")
  
  # A. NASA FIRMS
  message("\n  [1/3] Descargando Órbitas NASA FIRMS (24 Horas)...")
  tryCatch({
    source("R/api_nasa_firms.R", local = TRUE, encoding = "UTF-8")
    message("  ✅ NASA FIRMS: Telemetría en vivo actualizada.")
  }, error = function(e) {
    message("  ⚠️  NASA FIRMS: Saltado o sin datos (", e$message, ")")
  })
  
  # B. GOES-16 (NASA FIRMS Area API)
  message("\n  [2/3] Escaneando Satélite GOES-16 (Fuegos 1 Hora)...")
  tryCatch({
    source("R/api_goes16.R", local = TRUE, encoding = "UTF-8")
    message("  ✅ GOES-16: Alertas dinámicas actualizadas.")
  }, error = function(e) {
    message("  ⚠️  GOES-16: Saltado o sin datos (", e$message, ")")
  })
  
  # C. Google Earth Engine (Sentinel-2)
  message("\n  [3/3] Consultando Biomasa Sentinel-2...")
  tryCatch({
    if (requireNamespace("rgee", quietly = TRUE)) {
      source("R/api_sentinel_rgee.R", local = TRUE, encoding = "UTF-8")
      message("  ✅ Sentinel-2: Índices de biomasa actualizados.")
    } else {
      message("  ℹ️  Sentinel-2: Saltado (rgee no está instalado en este R).")
    }
  }, error = function(e) {
    message("  ⚠️  Sentinel-2: Error en la consulta satelital (", e$message, ")")
  })
  
} else {
  message("📡 Conexión a Internet: NO DETECTADA.")
  message("⚠️  MODO OFFLINE: Se omiten descargas satelitales en vivo.")
}

# --- 2. EJECUTAR EL MOTOR CENTRAL DE PREDICCIÓN Y CONSOLIDACIÓN ---
message("\n🤖 Ejecutando Motor de Consolidación y XGBoost V9...")
tryCatch({
  source("satica_engine.R", local = TRUE, encoding = "UTF-8")
  message("✅ MOTOR SATICA: Base de datos maestra consolidada exitosamente.")
}, error = function(e) {
  stop("❌ ERROR FATAL en el Motor SATICA: ", e$message)
})

# --- 3. SINCRONIZACIÓN AUTOMÁTICA CON GITHUB (SISTEMA NATIVO) ---
message("\n🚀 PREPARANDO SINCRONIZACIÓN CON GITHUB...")

# Verificar que git esté instalado en la máquina
git_check <- system("git --version", ignore.stdout = TRUE, ignore.stderr = TRUE)

if (git_check == 0) {
  # Agregar todos los cambios locales (incluyendo nuevos Excels de cosecha)
  system("git add .")
  
  # Verificar si hay cambios en cola
  status <- system("git status --porcelain", intern = TRUE)
  
  if (length(status) > 0) {
    fecha <- format(Sys.time(), "%Y-%m-%d %H:%M")
    msg_commit <- sprintf("Actualización manual e ingreso de cosecha - %s", fecha)
    
    message(paste("📦 Cambios detectados. Creando envío:", msg_commit))
    
    # Hacer el commit
    system(sprintf('git commit -m "%s"', msg_commit))
    
    # Subir cambios
    message("📤 Subiendo datos y código a GitHub...")
    push_res <- system("git push origin master")
    
    if (push_res == 0) {
      message("\n🎉 ¡SINCRONIZACIÓN COMPLETA CON GITHUB!")
      message("El Robot Centinela en la nube ya tiene tus datos más recientes.")
    } else {
      message("\n⚠️  Error al hacer Push. Revisa tus credenciales o conexión a Internet.")
    }
  } else {
    message("✅ GitHub: No hay datos nuevos ni cambios. Todo está sincronizado y al día.")
  }
} else {
  message("❌ ERROR: No se encontró 'git' instalado en el sistema. Asegúrate de tener Git configurado en tu PC.")
}

message("\n==========================================================")
message("🏁 PROCESO FINALIZADO.")
message("==========================================================\n")
