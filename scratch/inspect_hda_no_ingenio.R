# Let's inspect the historical fire database and find:
# 1. Any record where the prefix is NOT MN, CB, PR, PC, CA, CC, ML, MY.
# 2. Or any record in the original master database or visits list that had "ING. NA" or "OTRO"
# 3. We'll identify the specific hacienda, code, and source.

library(dplyr)

# 1. Check SATICA_HISTORIAL_v2.2.rds
historial <- readRDS("data_master/SATICA_HISTORIAL_v2.2.rds")
historial <- historial %>%
  mutate(
    prefix = substr(COD_UNICO_14, 1, 2),
    hda_key = paste(substr(COD_UNICO_14, 1, 2), substr(COD_UNICO_14, 3, 8), sep = "_")
  )

print("Unique prefixes in historical database:")
print(unique(historial$prefix))

# Let's see if there are any prefixes not mapped in mapear_ingenio:
# In global.R: MN, CB, PR, PC, CA, CC, ML, MY.
# RP (Riopaila) is in the historical database but is NOT in mapear_ingenio!
# Let's print out records with prefix "RP":
rp_records <- historial %>% filter(prefix == "RP")
print("Number of records with prefix RP (Riopaila):")
print(nrow(rp_records))
print("Unique hda_key with prefix RP:")
print(unique(rp_records$hda_key))

# 2. Let's look for records in the historical database that had `ing` column as NA in the master RDS
# because they weren't in the shapefile:
master_rds <- readRDS("data_master/SATICA_MASTER_v2.1.rds") # Let's check v2.1 if it exists
if (file.exists("data_master/SATICA_MASTER_v2.1.rds")) {
  print("Master v2.1 loaded.")
} else {
  master_rds <- readRDS("data_master/SATICA_MASTER_v2.2.rds")
}

# Let's see if there are records in master_rds where `ing` is NA
na_ing_records <- master_rds %>% filter(is.na(ing))
print(paste("Total records in master with ing == NA:", nrow(na_ing_records)))
print("Unique cod_hda_key of records with ing == NA in master (first 10):")
print(head(unique(na_ing_records$cod_hda_key), 10))

# Let's check a specific example: "CA_010001" or "CA_010074" or similar
# Let's find out if this hda_key exists in any of the Excel files in reportes_cosecha!
# We can search the Excel files for a code like "010074" or "010001" or the name of the hacienda.
# Let's write a script to search the Excel files for specific hacienda codes.
