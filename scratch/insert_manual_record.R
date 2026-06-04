# Restore CA_011037 manual record
library(dplyr)

master_rds <- readRDS("data_master/SATICA_MASTER_v2.2.rds")
df_visitas <- read.csv("visitas_cvc.csv", stringsAsFactors = FALSE)

mapear_ingenio <- function(codigo) {
  if (is.null(codigo)) return("OTRO")
  codigo_clean <- toupper(trimws(as.character(codigo)))
  case_when(
    is.na(codigo) | codigo_clean %in% c("NA", "", "NULL") ~ "OTRO",
    codigo_clean == "MN" ~ "MANUELITA",
    codigo_clean == "CB" ~ "LA CABAÑA",
    codigo_clean == "PR" ~ "PROVIDENCIA",
    codigo_clean == "PC" ~ "PICHICHI",
    codigo_clean == "CA" ~ "INCAUCA",
    codigo_clean == "CC" ~ "CENTRAL CASTILLA",
    codigo_clean == "ML" ~ "MARIA LUISA",
    codigo_clean == "MY" ~ "MAYAGÜEZ",
    TRUE ~ paste("ING.", codigo_clean)
  )
}

# Lookup details
db_row <- master_rds %>%
  filter(cod_hda_key == "CA_011037") %>%
  distinct(cod_hda_key, hda_nombre, municipio, corregimiento, lat, lon, ing) %>%
  mutate(INGENIO_VAL = mapear_ingenio(ing)) %>%
  head(1)

if (nrow(db_row) > 0) {
  new_row <- data.frame(
    cod_hda_key = "CA_011037",
    fecha_visita = "2025-09-02",
    radicado = "2025-RAD-12345",
    hda_label = db_row$hda_nombre,
    ingenio_full = db_row$INGENIO_VAL,
    municipio = db_row$municipio,
    corregimiento = db_row$corregimiento,
    coordenadas = paste(round(db_row$lat, 4), round(db_row$lon, 4), sep = ", "),
    categoria_alerta = "Auditoría Manual",
    boletin_n = "",
    stringsAsFactors = FALSE
  )
  
  # Remove existing CA_011037 if any
  df_visitas <- df_visitas %>% filter(cod_hda_key != "CA_011037")
  df_visitas <- bind_rows(df_visitas, new_row) %>%
    arrange(cod_hda_key)
  
  write.csv(df_visitas, "visitas_cvc.csv", row.names = FALSE, fileEncoding = "UTF-8")
  message("Successfully restored CA_011037 to visitas_cvc.csv!")
} else {
  message("Error: CA_011037 not found in database!")
}
