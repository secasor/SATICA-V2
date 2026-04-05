# 📑 Guía Técnica: Arquitectura e Ingeniería de SATICA V2.0

Esta guía detalla el funcionamiento interno de SATICA, explicando la lógica de sus componentes, el flujo de datos y la ingeniería detrás de la detección y predicción de incendios.

---

## 1. Arquitectura General del Sistema
SATICA opera bajo una **Arquitectura Híbrida** diseñada para máxima confiabilidad:

1.  **Motor Local (RStudio/OneDrive):** Donde reside el mapa maestro, los históricos de cosecha y el cerebro de predicción XGBoost.
2.  **Robot en la Nube (GitHub Actions):** Vigilante independiente que consulta satélites cada 15 minutos y envía alertas a Telegram sin intervención humana.
3.  **Dashboard Shiny:** Interfaz táctica que consolida la geografía con la inteligencia satelital mediante enlaces profundos (*Deep Linking*).

---

## 2. El Cerebro: `satica_engine.R` (Consolidación)
Este es el componente más crítico. Su función es transformar datos crudos de Excel en inteligencia espacial.

-   **Módulo de Normalización:** Limpia nombres de ingenios y haciendas usando la **Cláusula de Blindaje**. Convierte códigos heterogéneos en una clave única de 14 dígitos (`Cod_ing` + `Cod_hda` + `Cod_sue`).
-   **Fusión Nuclear Espacial:** Realiza un *Spatial Join* entre los reportes de cosecha y la capa catastral `SOR_OK.shp`. Si una suerte no tiene geometría, el motor le "hereda" la ubicación de su hacienda para no perder el rastro.
-   **Modelo Predictivo XGBoost V9:** No solo usa el historial, sino que analiza variables geoespaciales:
    -   Distancia a vías principales.
    -   Proximidad a centros poblados y bosques.
    -   Recurrencia histórica y mes del año.
    -   **Resultado:** Predice el `CICLO_DIAS_SUE` (cuántos días pasarán hasta el próximo incendio).

---

## 3. Adquisición de Datos Satelitales (`R/api_*.R`)

### 🛰️ NASA FIRMS (`api_nasa_firms.R`)
Consulta la API de la NASA para obtener focos activos detectados por los sensores **VIIRS (375m)** y **MODIS**. 
-   **Por qué 375m?**: Es la resolución ideal para detectar "quemas de caña" pequeñas que satélites de baja resolución ignorarían.
-   **Proceso:** Descarga un CSV, filtra por confianza "Nominal" o "Alta", y hace un *Point-in-Polygon* contra los lotes de caña.

### 🍃 Sentinel-2 via GEE (`api_sentinel_rgee.R`)
Usa Google Earth Engine para calcular índices de biomasa:
-   **NDVI:** Mide el verdor y vigor del cultivo.
-   **NBR:** Mide el nivel de quemado posterior al evento.
-   **Lógica:** Si el NDVI cae drásticamente, el sistema activa la "Alerta de Combustión por Resequedad".

### 🌪️ HYSPLIT (`api_hysplit.R`)
Cuando se confirma un incendio, este componente consulta los vientos dominantes integrando modelos meteorológicos de la NOAA para predecir la trayectoria del humo y las partículas.

---

## 4. Automatización y Robot (`centinela_satelital.R`)
Diseñado para correr en los servidores de GitHub (Ubuntu/Linux).

-   **Trigger:** Un archivo `.yml` en `.github/workflows/` dispara el script cada 15 minutos.
-   **Fila de Alerta:** Si los satélites reportan un foco dentro de un polígono de la CVC, el robot construye una **Ficha de Acción Rápida** en HTML y la envía vía API de Telegram.
-   **Deep Linking:** Genera una URL que, al ser clickeada, abre el Dashboard directamente centrado en la geocoordenada del incendio.

---

## 5. El Lado del Cliente: `global.R` y Smart Sync
Para asegurar que los técnicos siempre vean la verdad actualizaba:

-   **Smart Sync:** Al abrir la App, el sistema verifica `file.mtime()` del archivo maestro. Si detecta una antigüedad > 30 minutos, ejecuta `source("actualizar_y_ejecutar.R")` automáticamente antes de cargar el mapa.
-   **Modo Offline:** Incluye un manejo de errores que permite a la aplicación abrir con datos históricos si no hay internet en campo, evitando cierres inesperados.

---

## 6. Estructura de Datos Crítica

-   `data_master/SATICA_MASTER_v2.2.rds`: El archivo binario que contiene toda la inteligencia procesada. Es el "combustible" del Dashboard.
-   `capas/`: Carpeta con los archivos `.shp`. Es la base cartográfica oficial.
-   `www/`: Contiene el logo institucional y estilos CSS personalizados para la estética CVC.

---
**Guía generada por Antigravity AI - Documentación Técnica V2.0**
