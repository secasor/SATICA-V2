# Print first pages
library(pdftools)
pdf_files <- list.files("Boletines", pattern = "\\.pdf$", recursive = TRUE, full.names = TRUE)

for (f in pdf_files) {
  txt <- pdf_text(f)
  message("File: ", f)
  cat(substring(txt[1], 1, 300), "\n")
  message("--------------------------------------------------")
}
