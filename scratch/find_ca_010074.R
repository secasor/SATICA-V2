# Search for CA_010074 in the shapefile and historical databases
library(dplyr)

# Load shapefile
cana_path <- list.files("capas", pattern = "SOR_OK\\.shp$", full.names = TRUE)[1]
mapa_cana <- sf::st_read(cana_path, quiet = TRUE) %>% janitor::clean_names()

print("Is CA_010074 in the shapefile?")
print(any(mapa_cana$cod_unico == "CA010074" | mapa_cana$cod_hda == "CA_010074"))
print("Search shapefile for anything containing '010074':")
print(mapa_cana %>% filter(grepl("010074", cod_unico) | grepl("010074", ing_hda)) %>% sf::st_drop_geometry())

# Load historial
historial <- readRDS("data_master/SATICA_HISTORIAL_v2.2.rds")
print("Search historical database for anything containing '010074':")
hist_010074 <- historial %>% filter(grepl("010074", COD_UNICO_14))
print(hist_010074)

# Let's check which Excel file in reportes_cosecha this record CA010074 came from!
# We can search all excel files for "010074" or "CA010074"
message("\nSearching Excel files in reportes_cosecha for '010074' or '010074'...")
library(readxl)
archivos <- list.files("reportes_cosecha", pattern = "\\.xlsx$|\\.xls$", full.names = TRUE)

for (archivo in archivos) {
  tryCatch({
    # Read the file
    df <- read_excel(archivo, col_types = "text", .name_repair = "minimal")
    # Search in all columns
    for (col in names(df)) {
      matches <- which(grepl("010074", df[[col]], fixed = TRUE))
      if (length(matches) > 0) {
        print(paste("Match found in file:", basename(archivo), "column:", col, "row(s):", matches + 1))
        # Print the matching row (plus headers if skip was used)
        print(df[matches, 1:min(ncol(df), 10)])
      }
    }
  }, error = function(e) {})
}
