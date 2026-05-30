@echo off
chcp 65001 > nul
title SATICA V2.0 - Sistema de Alerta Temprana
color 0A

echo.
echo ==============================================================
echo           SATICA V2.0 - Sistema de Alerta Temprana          
echo         Corporacion Autonoma Regional del Valle del Cauca   
echo ==============================================================
echo.
echo Iniciando Dashboard...
echo (El navegador se abrira automaticamente. NO cierre esta ventana.)
echo.

:: --- DETECCIÓN INTELIGENTE DE R ---
:: Prioridad: 1) R-Portable local  2) R en ruta estándar  3) R en PATH

set "RSCRIPT="

:: 1. Buscar R-Portable junto a este script
if exist "%~dp0R-Portable\bin\Rscript.exe" (
    set "RSCRIPT=%~dp0R-Portable\bin\Rscript.exe"
    echo [OK] R-Portable detectado (Modo Portatil)
    goto :r_found
)

:: 2. Buscar R en la ruta hardcoded (instalación estándar del usuario)
if exist "D:\Program Files\R\R-4.4.2\bin\Rscript.exe" (
    set "RSCRIPT=D:\Program Files\R\R-4.4.2\bin\Rscript.exe"
    echo [OK] R detectado en D:\Program Files\R\R-4.4.2
    goto :r_found
)

:: 3. Buscar R en C:\Program Files (instalación estándar Windows)
for /d %%d in ("C:\Program Files\R\R-*") do (
    if exist "%%d\bin\Rscript.exe" (
        set "RSCRIPT=%%d\bin\Rscript.exe"
        echo [OK] R detectado en %%d
        goto :r_found
    )
)

:: 4. Buscar Rscript en el PATH del sistema
where Rscript >nul 2>nul
if %errorlevel% equ 0 (
    set "RSCRIPT=Rscript"
    echo [OK] R detectado en PATH del sistema
    goto :r_found
)

:: No se encontró R
echo.
echo [ERROR] No se encontro R en ninguna ubicacion.
echo.
echo   Opciones:
echo     a) Instale R desde https://cran.r-project.org/
echo     b) Coloque la carpeta R-Portable\ junto a este archivo
echo     c) Pida a su equipo de TI el bundle portable de SATICA
echo.
pause
exit /b 1

:r_found
echo.
cd /d "%~dp0"
"%RSCRIPT%" -e "shiny::runApp('.', launch.browser = TRUE, port = 3838)"

echo.
echo ==============================================================
echo La aplicacion se ha detenido.
echo Si hubo un error, revise los mensajes anteriores.
echo ==============================================================
pause
