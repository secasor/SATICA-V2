library(sf)
library(dplyr)
library(janitor)

cana_path <- list.files("capas", pattern = "SOR_OK\\.shp$", full.names = TRUE)[1]
mapa_cana <- st_read(cana_path, quiet = TRUE) %>% clean_names()

print("Columns in shapefile:")
print(names(mapa_cana))

print("Total number of records in shapefile:")
print(nrow(mapa_cana))

# Let's check the frequency of the 'ing' and 'ing_hda' columns in the shapefile:
if ("ing" %in% names(mapa_cana)) {
  print("Frequency of 'ing' in shapefile:")
  print(table(mapa_cana$ing, useNA = "always"))
}

if ("ing_hda" %in% names(mapa_cana)) {
  print("Frequency of 'ing_hda' in shapefile:")
  print(table(mapa_cana$ing_hda, useNA = "always"))
}

# Let's inspect some records where `ing` or `ing_hda` is missing in the shapefile
# Wait, let's look at records where `ing` is missing or is not a known ingenio.
# Let's see if there are any records with NA in `ing` or `ing_hda` in the shapefile itself:
if ("ing" %in% names(mapa_cana)) {
  na_records <- mapa_cana %>%
    filter(is.na(ing)) %>%
    st_drop_geometry() %>%
    head(20)
  print("Sample of records in shapefile with ing == NA:")
  print(na_records)
}
