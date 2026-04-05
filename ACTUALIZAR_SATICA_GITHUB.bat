@echo off
setlocal
:: =========================================================
:: 🚀 BOTÓN DE ACTUALIZACIÓN SATICA V2.0 (REGLAS DE CVC)
:: =========================================================
:: Instrucciones: Haz doble clic en este archivo para subir
:: tus cambios locales a GitHub.
:: =========================================================

echo.
echo  *********************************************************
echo  *              🛰️  SINCRONIZANDO CON GITHUB             *
echo  *********************************************************
echo.

powershell.exe -ExecutionPolicy Bypass -File "R\sync_script.ps1"

exit
