library(dplyr)

# Load shapefile
cana_path <- list.files("capas", pattern = "SOR_OK\\.shp$", full.names = TRUE)[1]
mapa_cana <- sf::st_read(cana_path, quiet = TRUE) %>% janitor::clean_names()

# Load historical fires
historial_incendios <- readRDS("data_master/SATICA_HISTORIAL_v2.2.rds")

print("Number of records in historial_incendios:")
print(nrow(historial_incendios))

print("Number of unique COD_UNICO_14 in historial_incendios:")
print(n_distinct(historial_incendios$COD_UNICO_14))

# Check how many COD_UNICO_14 in historial_incendios do NOT exist in mapa_cana$cod_unico
not_in_shapefile <- historial_incendios %>%
  filter(!COD_UNICO_14 %in% mapa_cana$cod_unico)

print("Number of fire records in historical reports whose COD_UNICO_14 is NOT in the shapefile:")
print(nrow(not_in_shapefile))

print("Number of unique COD_UNICO_14 NOT in the shapefile:")
print(n_distinct(not_in_shapefile$COD_UNICO_14))

# Let's see some of these orphan codes and their frequencies
orphan_summary <- not_in_shapefile %>%
  group_by(COD_UNICO_14) %>%
  summarise(events = n()) %>%
  arrange(desc(events))

print("Top 30 orphan COD_UNICO_14 (historical fires not in shapefile):")
print(head(orphan_summary, 30))

# Let's map their ingenio code prefix (first 2 characters of COD_UNICO_14)
# and see which ingenios they belong to.
orphan_summary <- orphan_summary %>%
  mutate(prefix = substr(COD_UNICO_14, 1, 2))

print("Count of orphan records by ingenio prefix:")
print(table(orphan_summary$prefix))
