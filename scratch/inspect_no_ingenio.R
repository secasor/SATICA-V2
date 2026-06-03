library(dplyr)

# Read the master file
master_rds <- readRDS("data_master/SATICA_MASTER_v2.2.rds")

print("Checking master_rds dimensions:")
print(dim(master_rds))

print("Names of columns in master_rds:")
print(names(master_rds))

# Find records where Ingenio is "OTRO" or NA or where mapping resulted in "OTRO"
# Let's see how Ingenio information is stored in the shapefile/master_rds
# Let's count the frequency of ingenio columns:
if ("INGENIO_FULL" %in% names(master_rds)) {
  print("Frequency of INGENIO_FULL in master_rds (including OTRO):")
  print(table(master_rds$INGENIO_FULL, useNA = "always"))
} else if ("ing" %in% names(master_rds)) {
  print("Frequency of 'ing' column in master_rds:")
  print(table(master_rds$ing, useNA = "always"))
}

# Let's see if we can find any specific hacienda code and name that maps to OTRO or NA:
# Let's print out the first 20 records where ingenio is not mapped or is OTRO/NA
if ("INGENIO_FULL" %in% names(master_rds)) {
  otros <- master_rds %>% 
    filter(INGENIO_FULL == "OTRO" | is.na(INGENIO_FULL)) %>%
    select(one_of("COD_HDA", "Hacienda", "Municipio", "ing", "INGENIO_FULL", "cod_hda_key")) %>%
    head(20)
  print("First 20 records with OTRO or NA INGENIO_FULL:")
  print(otros)
} else {
  # Let's look for columns with 'ing' in the name
  ing_cols <- names(master_rds)[grepl("ing", names(master_rds), ignore.case = TRUE)]
  print(paste("Columns containing 'ing':", paste(ing_cols, collapse = ", ")))
}
