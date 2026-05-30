library(dplyr)
library(sf)
library(lubridate)
library(tidyr)
library(stringr)

# 1. Cargar datos
master <- readRDS("data_master/SATICA_MASTER_v2.2.rds")
matriz <- readRDS("data_master/matriz_distancias_cana.rds")
historial <- readRDS("data_master/SATICA_HISTORIAL_v2.2.rds") %>%
  filter(FECHA >= as.Date("2019-01-01"))

# 2. Preparar Unión
# Cruce a nivel de Suerte (14 chars) para obtener distancias desde la matriz
# Luego a nivel de Hacienda para el reporte
master_geo <- master %>%
  select(cod_unico, nombre_hda, municipio, corregimiento, lat, lon, cod_hda_key) %>%
  left_join(matriz %>% select(cod_unico, dist_poblados_m), by = "cod_unico") %>%
  filter(!is.na(dist_poblados_m))

# Recurrencia por Hacienda (8 chars)
hist_hda <- historial %>%
  mutate(cod_hda_key = paste(substr(COD_UNICO_14, 1, 2), substr(COD_UNICO_14, 3, 8), sep = "_")) %>%
  group_by(cod_hda_key) %>%
  summarise(Recurrencia = n(), .groups = "drop")

# Unir con metadatos y distancias (usando el promedio de distancia de las suertes de la hacienda)
hda_final <- master_geo %>%
  group_by(cod_hda_key) %>%
  summarise(
    Nombre = first(nombre_hda),
    Municipio = first(municipio),
    Corregimiento = first(corregimiento),
    Distancia = min(dist_poblados_m, na.rm = TRUE),
    Lat = mean(lat, na.rm = TRUE),
    Lon = mean(lon, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(hist_hda, by = "cod_hda_key") %>%
  mutate(
    Recurrencia = coalesce(Recurrencia, 0),
    Ingenio = substr(cod_hda_key, 1, 2),
    Rango = case_when(
      Distancia <= 500 ~ "0 a 500 metros",
      Distancia <= 1500 ~ "501 metros a 1.5 km",
      Distancia <= 2000 ~ "1.5 hasta 2 km",
      TRUE ~ ">2 km"
    )
  )

# 3. Top 10 por Rango
# Formatear el Ingenio para que sea legible
mapear_ingenio <- function(codigo) {
  case_when(
    codigo == "CA" ~ "INCAUCA",
    codigo == "MY" ~ "MAYAGÜEZ",
    codigo == "ML" ~ "MARIA LUISA",
    codigo == "CC" ~ "CENTRAL CASTILLA",
    codigo == "PR" ~ "PROVIDENCIA",
    codigo == "MN" ~ "MANUELITA",
    codigo == "PC" ~ "PICHICHI",
    codigo == "CB" ~ "LA CABAÑA",
    codigo == "RP" ~ "RIOPAILA",
    TRUE ~ codigo
  )
}

top_10 <- hda_final %>%
  mutate(Ingenio = mapear_ingenio(Ingenio)) %>%
  group_by(Rango) %>%
  slice_max(order_by = Recurrencia, n = 10, with_ties = FALSE) %>%
  arrange(Rango, desc(Recurrencia)) %>%
  select(Rango, Nombre, Ingenio, Municipio, Corregimiento, Recurrencia, Distancia, Lat, Lon)

# 4. Estadísticas Generales
stats <- hda_final %>%
  group_by(Rango) %>%
  summarise(
    Incendios_Totales = sum(Recurrencia),
    Haciendas_Afectadas = sum(Recurrencia > 0),
    Distancia_Media = mean(Distancia),
    .groups = "drop"
  )

# 5. Salida
saveRDS(list(stats = stats, top10 = top_10), "resultados_modelo/reporte_incidencia_final.rds")

if (requireNamespace("openxlsx", quietly = TRUE)) {
  library(openxlsx)
  wb <- createWorkbook()
  addWorksheet(wb, "Estadisticas")
  writeData(wb, "Estadisticas", as.data.frame(stats))
  addWorksheet(wb, "Top 10 por Rango")
  writeData(wb, "Top 10 por Rango", as.data.frame(top_10))
  saveWorkbook(wb, "resultados_modelo/Reporte_Incidencia_Antropica_SATICA.xlsx", overwrite = TRUE)
  cat("\n✅ Archivo Excel generado: resultados_modelo/Reporte_Incidencia_Antropica_SATICA.xlsx\n")
}

# Imprimir para captura de Antigravity
cat("\n---STATS---\n")
print(as.data.frame(stats))
cat("\n---TOP10---\n")
print(as.data.frame(top_10))
