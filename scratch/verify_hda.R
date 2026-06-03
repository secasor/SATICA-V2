# Verify if CA_011594 exists in shapefile and check its exact location in the Excel report.
library(readxl)
library(dplyr)
library(janitor)

# Load shapefile
cana_path <- list.files("capas", pattern = "SOR_OK\\.shp$", full.names = TRUE)[1]
mapa_cana <- sf::st_read(cana_path, quiet = TRUE) %>% clean_names()

print("Search for 'CA011594' in shapefile:")
print(mapa_cana %>% filter(cod_unico == "CA011594000001" | ing_hda == "CA011594") %>% sf::st_drop_geometry())

# Search for any row with 11594 in the shapefile
print("Search for any row containing '11594' in shapefile:")
print(mapa_cana %>% filter(grepl("11594", cod_unico) | grepl("11594", ing_hda)) %>% sf::st_drop_geometry())

# Inspect the Excel file: sheets, and specific row
archivo_excel <- "reportes_cosecha/19082025 Reportes de Ingenios Cosecha JUL25.xls"
sheets <- excel_sheets(archivo_excel)
print("Sheets in excel file:")
print(sheets)

# Let's read the data
df_raw <- read_excel(archivo_excel, skip = 6, col_types = "text") %>% clean_names()
hda_matches <- df_raw %>% filter(cod_hacienda == "11594")
print("Rows matching cod_hacienda == '11594' in Excel:")
print(hda_matches %>% select(1:min(ncol(df_raw), 10)))
