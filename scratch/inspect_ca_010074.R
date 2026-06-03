master_rds <- readRDS("data_master/SATICA_MASTER_v2.2.rds")
subset_hda <- master_rds[master_rds$cod_hda_key == "CA_010074", ]
print(nrow(subset_hda))
print(head(subset_hda))
