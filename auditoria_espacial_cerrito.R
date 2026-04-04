library(sf)
library(dplyr)
library(readr)

cat("--- GENERACION DE VERSION FINAL DEFINITIVA (AUDITORIA CERRITO) ---\n")

# 1. Datos base
csv_audit <- read_csv("detalle_3316_suroriente.csv", show_col_types = FALSE) %>%
  mutate(COD_CLEAN = trimws(Codigo_14))

# 2. Geometrias (1-to-1)
hac <- st_read("capas/Suertes_Valle.shp", quiet=T)
st_crs(hac) <- 3115
h_3857 <- st_transform(hac, 3857)

hac_geo <- h_3857 %>%
  mutate(centroid = st_centroid(geometry)) %>%
  st_drop_geometry() %>%
  mutate(posX = st_coordinates(centroid)[,1]) %>%
  group_by(COD_UNICO) %>%
  summarize(posX = mean(posX, na.rm=TRUE), .groups = "drop")

# 3. Aplicar Regla de Oro del Director: El Cerrito -> Centro Sur
audit_final <- csv_audit %>%
  left_join(hac_geo, by = c("COD_CLEAN" = "COD_UNICO")) %>%
  mutate(NOM_DAR = case_when(
    is.na(posX) ~ "HUERFANO",
    posX < -8511500 ~ "SUROCCIDENTE",
    # REGLA CRITICA: Todo El Cerrito detectado en R es CENTRO SUR (Sincronizado con Director)
    regexpr("CERRITO", Municipio_Clean, ignore.case=T) > 0 ~ "CENTRO SUR",
    TRUE ~ "SURORIENTE"
  )) %>%
  # Limpiar columnas auxiliares para el reporte limpio
  select(-posX, -COD_CLEAN)

# 4. Escribir reportes con nombres PROTEGIDOS
# Estos nombres son nuevos para que NO esten abiertos en el Excel del usuario
write_csv(audit_final %>% filter(NOM_DAR == "SURORIENTE"), "SOLO_SURORIENTE_DEFINITIVO_17MAR.csv")
write_csv(audit_final %>% filter(NOM_DAR %in% c("CENTRO SUR", "SUROCCIDENTE")), "DISCREPANCIAS_CENTRO_SUR_DEFINITIVO_17MAR.csv")

cat("REPORTES GENERADOS EXITOSAMENTE.\n")
cat("- Encontrados en SURORIENTE:", nrow(audit_final %>% filter(NOM_DAR == "SURORIENTE")), "\n")
cat("- Reasignados (Discrepancias):", nrow(audit_final %>% filter(NOM_DAR %in% c("CENTRO SUR", "SUROCCIDENTE"))), "\n")

# Verificacion de QUITASUENO (PR080812000011) en el resultado
check <- audit_final %>% filter(grepl("PR080812", Codigo_14)) %>% select(Hacienda, Codigo_14, NOM_DAR)
cat("\nVerificacion QUITASUENO:\n")
print(check)
