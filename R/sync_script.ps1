# =========================================================
# 🛰️ SINCRONIZADOR SATICA V2.0 -> GITHUB
# =========================================================
# Este script sube tus cambios locales a GitHub para que 
# el Robot Centinela siempre tenga la última versión.
# =========================================================

$RepoPath = "c:\Users\user\OneDrive - CORPORACION AUTONOMA REGIONAL DEL VALLE DEL CAUCA\SURORIENTE\Seguimiento Ingenios\Incendios\2025\SATICA V2.0"
Set-Location -Path $RepoPath

Write-Host "🔍 Verificando cambios en SATICA..." -ForegroundColor Cyan

# Agregar cambios
git add .

# Verificar si hay algo para subir
$status = git status --porcelain
if (-not $status) {
    Write-Host "✅ No hay cambios nuevos para subir. ¡Todo está al día!" -ForegroundColor Green
    start-sleep -Seconds 3
    exit
}

# Crear el mensaje con la fecha
$fecha = Get-Date -Format "yyyy-MM-dd HH:mm"
$mensaje = "Actualización manual desde PC - $fecha"

Write-Host "📦 Preparando envío: $mensaje" -ForegroundColor Yellow
git commit -m $mensaje

Write-Host "🚀 Subiendo a GitHub..." -ForegroundColor Magenta
git push origin master

if ($LASTEXITCODE -eq 0) {
    Write-Host "`n✅ ¡SINCRONIZACIÓN EXITOSA!" -ForegroundColor Green
    Write-Host "El Robot Centinela ya tiene tus últimas mejoras." -ForegroundColor White
} else {
    Write-Host "`n❌ ERROR AL SINCRONIZAR." -ForegroundColor Red
    Write-Host "Asegúrate de tener conexión a internet." -ForegroundColor White
}

Write-Host "`nEsta ventana se cerrará en 5 segundos..."
start-sleep -Seconds 5
