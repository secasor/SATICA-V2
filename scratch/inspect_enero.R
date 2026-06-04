# Inspect Enero PDF
library(pdftools)
txt <- pdf_text("Boletines/Enero/Boletin_001_SATICA_2026-01-30.pdf")
message("Enero PDF length: ", length(txt))
for (i in 1:length(txt)) {
  message("Page ", i, ": length = ", nchar(txt[i]))
  if (nchar(txt[i]) > 0) {
    cat(substring(txt[i], 1, 300), "\n")
  }
}
