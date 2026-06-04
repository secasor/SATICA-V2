# Read excel columns
if (requireNamespace("openpyxl", quietly = TRUE)) {
  library(openpyxl)
  wb <- loadWorkbook("Boletines/Marzo/Seguimiento_Boletin_SATICA_2026-04-09.xlsx")
  sheets <- names(wb)
  message("Sheets: ", paste(sheets, collapse = ", "))
  for (sh in sheets) {
    df <- read.xlsx("Boletines/Marzo/Seguimiento_Boletin_SATICA_2026-04-09.xlsx", sheet = sh)
    message("Sheet: ", sh)
    print(head(df))
  }
} else {
  message("openpyxl is not installed.")
}
