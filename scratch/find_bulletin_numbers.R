# Extract bulletin numbers and dates
library(pdftools)
pdf_files <- list.files("Boletines", pattern = "\\.pdf$", recursive = TRUE, full.names = TRUE)

for (f in pdf_files) {
  txt <- pdf_text(f)
  first_page <- txt[1]
  
  # Search for Boletin N. XX or similar
  num_match <- regmatches(first_page, regexec("(?i)bolet[íi]n\\s*(?:n[\\.º]|#|n[uú]mero)?\\s*(\\d+)", first_page, perl = TRUE))[[1]]
  date_match <- regmatches(first_page, regexec("(?i)(\\d{1,2}\\s+de\\s+[a-z]+|\\d{1,2}\\s+de\\s+[a-z]+\\s+de\\s+\\d{4}|[a-z]+\\s+\\d{1,2}\\s*,\\s*\\d{4}|\\d{4}-\\d{2}-\\d{2})", first_page, perl = TRUE))[[1]]
  
  message("File: ", f)
  message("  Matched Bulletin Num: ", ifelse(length(num_match) > 1, num_match[2], "Not found"))
  message("  Matched Date Text: ", ifelse(length(date_match) > 0, date_match[1], "Not found"))
}
