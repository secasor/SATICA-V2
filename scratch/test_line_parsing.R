# Test line-by-line PDF parsing
library(pdftools)
library(dplyr)
library(sf)

master_rds <- readRDS("data_master/SATICA_MASTER_v2.2.rds")

# Unicos en DB
haciendas_db <- master_rds %>%
  distinct(cod_hda_key, hda_nombre, municipio, corregimiento, lat, lon, ing)

pdf_path <- "Boletines/Marzo/Boletin_SATICA_2026-03-16.pdf"
txt <- pdf_text(pdf_path)

all_lines <- unlist(strsplit(txt, "\n"))
message("Total lines in PDF: ", length(all_lines))

matches_count <- 0
for (line in all_lines) {
  # Look for name_code pattern like NIAGARA_187 or COSACOS_160
  # Regex: match word containing underscore followed by digits
  m <- regmatches(line, regexec("([A-Za-z0-9áéíóúÁÉÍÓÚñÑ\\s-]+)_(\\d+)", line))[[1]]
  if (length(m) > 1) {
    hda_name_part <- trimws(m[2])
    hda_code_num <- as.integer(m[3])
    
    # Try to find matching ingenio in the line
    ing_prefix <- NA_character_
    if (grepl("INCAUCA", line, ignore.case = TRUE)) ing_prefix <- "CA"
    else if (grepl("MAYAG", line, ignore.case = TRUE)) ing_prefix <- "MY"
    else if (grepl("CABA", line, ignore.case = TRUE)) ing_prefix <- "CB"
    else if (grepl("MANUEL", line, ignore.case = TRUE)) ing_prefix <- "MN"
    else if (grepl("PICHICHI", line, ignore.case = TRUE)) ing_prefix <- "PC"
    else if (grepl("PROVID", line, ignore.case = TRUE)) ing_prefix <- "PR"
    else if (grepl("RIOPAIL", line, ignore.case = TRUE)) ing_prefix <- "RP"
    else if (grepl("CASTILL", line, ignore.case = TRUE)) ing_prefix <- "CC"
    else if (grepl("MARIA", line, ignore.case = TRUE)) ing_prefix <- "ML"
    
    # Lookup in DB
    db_match <- haciendas_db %>%
      filter(as.integer(gsub("[^0-9]", "", cod_hda_key)) == hda_code_num)
    
    if (!is.na(ing_prefix)) {
      db_match <- db_match %>% filter(ing == ing_prefix)
    }
    
    if (nrow(db_match) > 0) {
      matches_count <- matches_count + 1
      cat(sprintf("Match %d: Line: %s\n      -> Key: %s | Hda: %s | Ingenio: %s\n",
                  matches_count, trimws(line), db_match$cod_hda_key[1], db_match$hda_nombre[1], db_match$ing[1]))
    }
  }
}
