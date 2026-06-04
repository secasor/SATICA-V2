# Inspect PDF text
library(pdftools)
pdf_files <- list.files("Boletines", pattern = "\\.pdf$", recursive = TRUE, full.names = TRUE)
print(pdf_files)

for (f in pdf_files) {
  txt <- pdf_text(f)
  message("\n==================================================")
  message("File: ", f)
  message("Pages: ", length(txt))
  # print first page text snippet
  cat("PAGE 1:\n")
  cat(substring(txt[1], 1, 1000), "\n")
  if (length(txt) >= 2) {
    cat("PAGE 2:\n")
    cat(substring(txt[2], 1, 1000), "\n")
  }
}
