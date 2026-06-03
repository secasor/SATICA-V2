library(dplyr)

master_rds <- readRDS("data_master/SATICA_MASTER_v2.2.rds")

# Find cases where `ing` is NA
na_ing <- master_rds %>%
  filter(is.na(ing))

print("Number of records with ing == NA:")
print(nrow(na_ing))

print("Unique values of `ing_hda` for records where `ing` is NA:")
print(unique(na_ing$ing_hda))

print("Unique values of `tiene_geometria` for records where `ing` is NA:")
print(table(na_ing$tiene_geometria, useNA = "always"))

print("Sample records where `ing` is NA and they have geometry:")
na_ing_with_geom <- na_ing %>%
  filter(tiene_geometria == TRUE)

print(nrow(na_ing_with_geom))

if (nrow(na_ing_with_geom) > 0) {
  print(head(na_ing_with_geom %>% select(fid_suerte, nombre_hda, ing, ing_hda, cod_unico, cod_hda_key, municipio_geo), 30))
}

print("Sample records where `ing` is NA and they do NOT have geometry:")
na_ing_no_geom <- na_ing %>%
  filter(tiene_geometria == FALSE | is.na(tiene_geometria))

print(nrow(na_ing_no_geom))

if (nrow(na_ing_no_geom) > 0) {
  print(head(na_ing_no_geom %>% select(fid_suerte, nombre_hda, ing, ing_hda, cod_unico, cod_hda_key, municipio), 30))
}
