# Print details of the row
library(readxl)
library(dplyr)
library(janitor)

archivo_excel <- "reportes_cosecha/19082025 Reportes de Ingenios Cosecha JUL25.xls"
df_raw <- read_excel(archivo_excel, skip = 6, col_types = "text") %>% clean_names()
hda_matches <- df_raw %>% filter(cod_hacienda == "11594")

print("All columns for cod_hacienda == '11594':")
print(t(hda_matches))
