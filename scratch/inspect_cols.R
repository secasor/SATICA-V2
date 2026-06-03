master_rds <- readRDS("data_master/SATICA_MASTER_v2.2.rds")
print(names(master_rds))
print(head(master_rds[, c("lat", "lon")]))
