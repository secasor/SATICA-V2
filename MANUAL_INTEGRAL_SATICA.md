# 🛰️ Manual Integral: SATICA V2.0 
### Sistema de Alertas Tempranas de Incendios en Caña de Azúcar (CVC)

Este manual es la fuente de verdad única de SATICA. Está diseñado para guiar al lector desde los conceptos fundamentales hasta los detalles de ingeniería avanzada.

---

## 🌎 1. Nivel Concepto: ¿Qué es SATICA?

**SATICA** es un sistema de vigilancia "Ojo de Halcón" diseñado para la **CVC (Corporación Autónoma Regional del Valle del Cauca)**. Su misión es proteger el medio ambiente y la salud pública mediante el monitoreo constante de los cultivos de caña de azúcar.

### 🛡️ Misión Ambiental
- **Detección Precoz**: Identificar fuegos antes de que se vuelvan incontrolables.
- **Calidad del Aire**: Reducir las emisiones de material particulado que afectan a las comunidades.
- **Protección de Fauna**: Alertar para proteger el hábitat de aves y mamíferos locales.

---

## 📱 2. Nivel Operativo: ¿Cómo se usa?

SATICA es una herramienta **táctica**. No requiere que el técnico esté frente a una pantalla todo el día gracias a su automatización.

### 🔥 Alertas en Telegram
Cuando el sistema detecta un incendio, envía una **Ficha de Acción Rápida** al canal oficial con:
- **Hacienda y Suerte**: Identificación precisa del predio.
- **Geolocalización**: Coordenadas exactas (GPS).
- **Deep Link**: Un botón que abre el Dashboard de Shiny automáticamente centrado en el incendio.

### 🔄 Sincronización de un Clic
Para que el sistema de la nube siempre tenga tus últimas mejoras locales, usa el archivo:
**`ACTUALIZAR_SATICA_GITHUB.bat`** (Doble clic y listo).

### ⚡ Smart Sync
Al abrir el Dashboard, el sistema verifica si los datos tienen más de 30 minutos. Si están "viejos", se actualiza solo consultando a la NASA antes de mostrar el mapa.

---

## ⚙️ 3. Nivel Ingeniería: Componentes Técnicos

Esta sección detalla la arquitectura de software y la lógica de datos detras del sistema.

### 🧠 El Motor de Consolidación (`satica_engine.R`)
Es el cerebro que integra la geografía con los datos de negocio:
- **Fusión Nuclear**: Realiza un cruce espacial (*Spatial Join*) entre los reportes de cosecha (.xlsx) y la cartografía oficial (`SOR_OK.shp`).
- **Blindaje de Identidad**: Genera una clave única de 14 dígitos para cada suerte (`Cod_ing` + `Cod_hda` + `Cod_sue`), evitando confusiones entre ingenios.
- **Modelo Predictivo XGBoost V9**: Una red neuronal que predice cuándo ocurrirá el próximo incendio basándose en distancia a vías, centros poblados y recurrencia histórica.

### 🛰️ Telemetría Satelital
- **NASA FIRMS (`api_nasa_firms.R`)**: Consulta sensores **VIIRS (375m)**. Detecta anomalías térmicas con alta precisión espacial.
- **Sentinel-2 (`api_sentinel_rgee.R`)**: Calcula índices de biomasa (NDVI) y quemado (NBR) mediante Google Earth Engine para evaluar la resequedad del cultivo.
- **HYSPLIT (`api_hysplit.R`)**: Simula la trayectoria de los vientos para predecir hacia dónde se moverá el humo.

---

## 🤖 4. Nivel Administración: Infraestructura y Automatización

SATICA es un sistema "Serverless" que corre sin necesidad de servidores propios costosos.

### ☁️ GitHub Actions
El archivo `.github/workflows/centinela.yml` actúa como el "reloj" del sistema:
- Dispara el **Robot Centinela** cada 15 minutos.
- Instala las dependencias de R en servidores de alta velocidad (Ubuntu).
- Gestiona las **Secrets** (Llaves API y Tokens de Telegram) de forma cifrada y segura.

### 📁 Estructura de Directorios Crítica
| Carpeta | Contenido |
|---|---|
| `data_master/` | Contiene el `SATICA_MASTER_v2.2.rds` (Base de inteligencia). |
| `capas/` | Archivos Shapefile oficial (.shp) de la CVC. |
| `reportes_cosecha/` | Archivos Excel crudos suministrados por los ingenios. |
| `www/` | Recursos visuales y estilos CSS del Dashboard. |

---
**Documentación SATICA V2.4 — Generada por Antigravity AI (2026)**
