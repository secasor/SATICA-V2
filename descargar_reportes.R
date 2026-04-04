# Script para compilar el Informe y la Presentación de SATICA V3.0
# Ejecutar este script desde RStudio para generar directamente los archivos .docx y .pptx

if (!require("rmarkdown")) install.packages("rmarkdown")

message("📄 Generando Informe Ejecutivo en Word (.docx)...")
rmarkdown::render("informe_ejecutivo_satica.Rmd", output_format = "word_document")

message("📊 Generando Presentación en PowerPoint (.pptx)...")
rmarkdown::render("presentacion_satica.Rmd", output_format = "powerpoint_presentation")

message("✅ ¡Listo! Los archivos 'informe_ejecutivo_satica.docx' y 'presentacion_satica.pptx' están listos en tu carpeta.")
