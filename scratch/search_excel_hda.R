# Search Excel files in reportes_cosecha for "10074" or similar
library(readxl)
library(dplyr)

archivos <- list.files("reportes_cosecha", pattern = "\\.xlsx$|\\.xls$", full.names = TRUE)

found <- FALSE

for (archivo in archivos) {
  tryCatch({
    df <- read_excel(archivo, col_types = "text", .name_repair = "minimal")
    for (col in names(df)) {
      # Remove any NA or empty strings
      vals <- df[[col]]
      vals[is.na(vals)] <- ""
      # Match numeric representation (e.g. 10074, 10074.0, or 010074)
      matches <- which(grepl("10074", vals, fixed = TRUE))
      if (length(matches) > 0) {
        print("=========================================================================")
        print(paste("MATCH FOUND! File:", basename(archivo)))
        print(paste("Column:", col))
        print(paste("Rows:", paste(matches + 1, collapse = ", ")))
        print("Data rows:")
        print(df[matches, 1:min(ncol(df), 10)])
        print("=========================================================================")
        found <- TRUE
      }
    }
  }, error = function(e) {
    # Print error if any
    # print(e)
  })
}

if (!found) {
  print("No matches for 10074 found.")
}
