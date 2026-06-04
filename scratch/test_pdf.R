# Test pdftools and PDF reading
if (requireNamespace("pdftools", quietly = TRUE)) {
  message("pdftools is installed!")
  txt <- pdftools::pdf_text("Boletines/Enero/Boletin_001_SATICA_2026-01-30.pdf")
  message("Successfully read Enero PDF. Number of pages: ", length(txt))
  print(substring(txt[1], 1, 500))
} else {
  message("pdftools is NOT installed.")
}
