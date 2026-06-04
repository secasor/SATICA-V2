# Test parsing of PDF bulletins
library(pdftools)
library(dplyr)
library(sf)

master_rds <- readRDS("data_master/SATICA_MASTER_v2.2.rds")
geo_base <- readRDS("data_estatica/GEO_CACHE_SATICA.rds")

# All unique properties in the system
haciendas_db <- master_rds %>%
  distinct(cod_hda_key, hda_nombre, municipio, corregimiento, lat, lon) %>%
  mutate(
    num_part = regmatches(cod_hda_key, regexpr("\\d+", cod_hda_key)),
    ing_part = substr(cod_hda_key, 1, 2)
  )

message("Number of unique properties in Geo DB: ", nrow(haciendas_db))

# Test parsing a specific PDF
pdf_path <- "Boletines/Marzo/Boletin_SATICA_2026-03-16.pdf"
txt <- pdf_text(pdf_path)
all_txt <- paste(txt, collapse = "\n")

# Find matches for _[0-9A-Z]+
matches <- gregexpr("_[0-9A-Z]{3,8}\\b", all_txt)[[1]]
match_vals <- regmatches(all_txt, gregexpr("_[0-9A-Z]{3,8}\\b", all_txt))[[1]]
match_nums <- gsub("_", "", match_vals)

message("Found match numbers in PDF: ", length(match_nums))
print(unique(match_nums))

# Try to find corresponding keys in DB
matched_hdas <- haciendas_db %>%
  filter(as.integer(num_part) %in% as.integer(match_nums))

message("Successfully matched properties in DB: ", nrow(matched_hdas))
print(head(matched_hdas %>% select(cod_hda_key, hda_nombre)))
