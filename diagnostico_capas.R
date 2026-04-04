# diagnostico_capas.R
library(sf)

check_shp <- function(path) {
  message("🔍 Probando: ", path)
  tryCatch({
    data <- st_read(path, quiet = FALSE)
    message("✅ Éxito! Filas: ", nrow(data))
    return(TRUE)
  }, error = function(e) {
    message("❌ FALLO: ", e$message)
    return(FALSE)
  })
}

capas <- list.files("capas", pattern = "\\.shp$", full.names = TRUE)
for (c in capas) {
  check_shp(c)
}
