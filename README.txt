MODELO PREDICTIVO DE INCENDIOS EN CAÑA – VALLE DEL CAUCA (DAR Suroriente) - Versión Definitiva
Autor: Alexander Barona
Generado: 2025-11-12 03:26:31

Contenido y uso rápido:
- Coloque sus archivos Excel en /data (columnas esperadas: fecha, ingenio, cosecha, municipio, corregimiento, hacienda, suerte, area)
- Coloque los shapefiles descomprimidos en /capas.
  * Capa de haciendas debe contener: NOMBRE_HDA (nombre de la hacienda), STE (suerte id/name)
  * Capa de municipios debe contener: NOM_MUNICI (municipio), NOM_DIV_PO (corregimiento)
- Ejecutar en RStudio:
    source("instalar_dependencias.R")  # la primera vez
    # luego abrir app.R y presionar Run App
- Dentro de la app: pulsar "Actualizar y ejecutar (One-Click)"

Salida (en /resultados_modelo y /reportes_alerta):
- CSV/XLSX de predicciones
- Mapa HTML
- Shapefile exportado (carpeta shapefile_prediccion_TIMESTAMP)
- Informe PDF tipo 'Alerta Temprana' (vertical)
