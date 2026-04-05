# 🛰️ Guía de Operación: SATICA V2.0 (Robot Centinela)

Este documento resume el funcionamiento del sistema automatizado de vigilancia satelital para la CVC - Regional Suroriente y las futuras implementaciones regionales.

## 🤖 1. Robot Centinela (GitHub Actions)
El robot de vigilancia espacial ya está operativo en la nube de GitHub.

- **Frecuencia:** Cada 15 minutos (6:00 AM - 10:00 PM COT).
- **Satélites:** NASA FIRMS (VIIRS 375m + MODIS).
- **Alertas:** Recibirás una "Ficha de Acción Rápida" directamente en tu Telegram personal con:
    - Hacienda y Suerte afectada.
    - Coordenadas GPS con acceso a Deep Link.
    - Nivel de riesgo predicho por el motor SATICA.

- **Repositorio:** [alexbarona-pixel/SATICA-V2](https://github.com/alexbarona-pixel/SATICA-V2)
- **Estado de Ejecución:** [Ver en Vivo](https://github.com/alexbarona-pixel/SATICA-V2/actions/workflows/centinela.yml) ✅

---

## 🔄 2. Smart Sync (Automatización en Shiny)
Se eliminó la necesidad de ejecutar scripts locales por separado. 

- **¿Cómo funciona?**: Al abrir el Dashboard de SATICA ("Run App"), el sistema verifica automáticamente la edad de los datos.
- **Tolerancia**: Si los datos tienen más de **30 minutos**, el sistema se sincroniza solo con los satélites antes de mostrar el mapa.
- **Modo Offline**: Si el técnico no tiene internet, la app abrirá con los últimos datos guardados pero mostrará un aviso de advertencia.

---

## 🔘 3. Botón de Sincronización Manual (PC)
Para que el Robot Centinela tenga siempre tus últimas mejoras de código, hemos creado un botón de un solo clic.

1. Ubica el archivo **`ACTUALIZAR_SATICA_GITHUB.bat`** en la carpeta raíz del proyecto.
2. **Uso:** Haz doble clic.
3. **Resultado:** Tus cambios en los archivos `.R` se subirán automáticamente a GitHub sin necesidad de comandos.

> [!TIP]
> Puedes copiar este archivo y pegarlo directamente en tu **Escritorio** para tener acceso rápido al final de tu jornada laboral.

---

## 🔑 4. Configuración de Seguridad (GitHub Secrets)
Los datos sensibles están protegidos y cifrados en GitHub. No edites estos valores a menos que cambies de Bot o API Key:

| Secreto GITHUB | Propósito |
|---|---|
| `TELEGRAM_BOT_TOKEN` | Identidad del Bot de Alertas |
| `TELEGRAM_CHAT_ID` | Tu ID de chat personal |
| `NASA_FIRMS_KEY` | Llave de acceso a telemetría de NASA |

---

## 🚀 Próximos Pasos para otras Regionales
Esta arquitectura permite que, para replicar SATICA en otra regional:
1. Solo se requiera clonar el código.
2. Cambiar la capa `SOR_OK.shp` por la regional correspondiente.
3. El técnico solo tendrá que preocuparse por **abrir el Dashboard**, el robot hará el resto.

> [!IMPORTANT]
> **Mantenimiento**: Recuerda que el robot centinela consume recursos gratuitos de GitHub (2000 min/mes). Al correr cada 15 min solo en horas críticas, es más que suficiente para el monitoreo regional de todo el año.

---
**Generado por Antigravity AI - 2026**
