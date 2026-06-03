library(readxl)
library(purrr)

archivos <- list.files("reportes_cosecha", pattern = "\\.xlsx$|\\.xls$", full.names = TRUE)
for(f in archivos) {
  df <- tryCatch({ read_excel(f, skip = 6, col_types = "text") }, error = function(e) NULL)
  if(!is.null(df)) {
    # Check if there is any row matching 010074 in the hacienda column (usually col 2 or 3)
    matching_rows <- which(apply(df, 1, function(row) any(grepl("010074|CA010074", row, ignore.case = TRUE))))
    if(length(matching_rows) > 0) {
      cat(sprintf("Match found in file: %s at row(s) %s\n", f, paste(matching_rows + 7, collapse = ", ")))
      print(df[matching_rows, 1:6])
    }
  }
}
