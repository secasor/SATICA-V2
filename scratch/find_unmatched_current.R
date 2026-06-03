# Read current excel reports and check if any records do not match the shapefile.
library(readxl)
library(dplyr)
library(purrr)
library(janitor)

# Load shapefile
cana_path <- list.files("capas", pattern = "SOR_OK\\.shp$", full.names = TRUE)[1]
mapa_cana <- sf::st_read(cana_path, quiet = TRUE) %>% clean_names()

archivos <- list.files("reportes_cosecha", pattern = "\\.xlsx$|\\.xls$", full.names = TRUE)

limpiar_texto <- function(x) {
  if (is.null(x)) return("")
  toupper(gsub("[^A-Za-z0-9]", "", as.character(x)))
}

print(paste("Number of files to scan:", length(archivos)))

# Let's scan all files and check if they produce any records that are NOT in the shapefile:
for (archivo in archivos) {
  tryCatch({
    df_raw <- read_excel(archivo, skip = 6, col_types = "text") %>% clean_names()
    
    col_ing <- names(df_raw)[grepl("ingenio", names(df_raw))][1]
    col_hda <- names(df_raw)[grepl("hacienda|cod_hda", names(df_raw))][1]
    col_sue <- names(df_raw)[grepl("suerte|lote|cod_sue", names(df_raw))][1]
    
    if(any(is.na(c(col_ing, col_hda, col_sue)))) next
    
    processed <- df_raw %>%
      filter(cod_cosecha == "I") %>%
      mutate(
        ing_clean = tolower(limpiar_texto(!!sym(col_ing))),
        Cod_ing = case_when(
          grepl("incauca", ing_clean) ~ "CA",
          grepl("mayaguez", ing_clean) ~ "MY",
          grepl("maria", ing_clean) ~ "ML",
          grepl("castilla", ing_clean) ~ "CC",
          grepl("providencia", ing_clean) ~ "PR",
          grepl("manuelita", ing_clean) ~ "MN",
          grepl("pichichi", ing_clean) ~ "PC",
          grepl("caba", ing_clean) ~ "CB",
          grepl("riopaila", ing_clean) ~ "RP",
          TRUE ~ NA_character_
        ),
        hda_limpia = toupper(gsub("[^0-9A-Za-z_]", "", as.character(!!sym(col_hda)))),
        sue_limpia = toupper(gsub("[^0-9A-Za-z_]", "", as.character(!!sym(col_sue)))),
        Cod_hda_full = stringr::str_pad(hda_limpia, width = 6, side = "left", pad = "0"),
        Cod_sue_full = stringr::str_pad(sue_limpia, width = 6, side = "left", pad = "0"),
        COD_UNICO_14 = paste0(Cod_ing, Cod_hda_full, Cod_sue_full),
        cod_hda_key = paste(Cod_ing, Cod_hda_full, sep = "_")
      ) %>%
      filter(!is.na(Cod_ing))
    
    # Check if any COD_UNICO_14 is NOT in shapefile
    unmatched <- processed %>%
      filter(!COD_UNICO_14 %in% mapa_cana$cod_unico)
    
    if (nrow(unmatched) > 0) {
      print(paste("File:", basename(archivo), "has", nrow(unmatched), "unmatched fire records."))
      print("First few unmatched records:")
      print(unmatched %>% select(COD_UNICO_14, cod_hda_key, !!sym(col_ing), !!sym(col_hda), !!sym(col_sue)) %>% head(5))
      break
    }
  }, error = function(e) {
    # print(e)
  })
}
