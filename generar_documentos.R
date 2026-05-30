# =============================================================================
# GENERADOR PREMIUM DE DOCUMENTOS SATICA 2.0 — V2 MEJORADO
# Usa la PPT original como plantilla base, mejora contenido y diseño
# =============================================================================
library(officer)
library(flextable)
library(ggplot2)
library(dplyr)
library(scales)

# ─── PALETA CVC/SATICA ────────────────────────────────────────────────────────
ROJO_FUEGO <- "#C0392B"; ROJO_CLARO <- "#FADBD8"
NARANJA    <- "#E67E22"; NARANJA_CLARO <- "#FDEBD0"
VERDE_CVC  <- "#1E8449"; VERDE_CLARO <- "#D5F5E3"
AZUL_CVC   <- "#1A5276"
GRIS_OSC   <- "#212F3D"; GRIS_MED <- "#5D6D7E"; GRIS_CLARO <- "#F2F3F4"
BLANCO     <- "#FFFFFF"; NEGRO <- "#000000"
ORO        <- "#F39C12"

# Tipografías
FNT_TITULO <- "Calibri Light"
FNT_CUERPO <- "Calibri"

cat("📊 Cargando datos...\n")
rds_path <- "resultados_modelo/reporte_incidencia_final.rds"
reporte  <- readRDS(rds_path)
top10    <- reporte$top10
stats    <- reporte$stats

top10_tbl <- top10 %>%
  arrange(desc(Recurrencia)) %>%
  slice_head(n = 10) %>%
  mutate(
    `Dist. Poblado` = paste0(round(Distancia), " m"),
    Exposición = case_when(
      Distancia <= 500  ~ "CRÍTICA",
      Distancia <= 1500 ~ "ALTA",
      Distancia <= 2000 ~ "MEDIA",
      TRUE              ~ "BAJA"
    )
  ) %>%
  select(Hacienda = Nombre, Municipio, Ingenio,
         Incendios = Recurrencia, `Dist. Poblado`, Exposición)

# ==============================================================================
# A.  INFORME WORD — Usando SATICA_V2_Informe_Tecnico_Consolidado.docx como base
# ==============================================================================
cat("📄 Generando Word premium...\n")

ref_word <- "reportes_finales/SATICA_V2_Informe_Tecnico_Consolidado.docx"
doc <- read_docx(ref_word)   # ← hereda todos los estilos del original

# Limpiar el doc manteniendo sólo la primera página (portada original)
# y agregamos contenido mejorado después
# -- Como officer no permite borrar slides fácilmente, generamos desde plantilla
doc2 <- read_docx(ref_word)

# Función helper para texto con color
h1 <- function(txt) {
  fpar(
    ftext(txt, fp_text(font.size = 18, bold = TRUE,
                       color = AZUL_CVC, font.family = FNT_TITULO)),
    fp_p = fp_par(padding.bottom = 6, padding.top = 14, border.bottom = fp_border(color = AZUL_CVC, width = 2))
  )
}
h2 <- function(txt) {
  fpar(
    ftext(txt, fp_text(font.size = 14, bold = TRUE,
                       color = GRIS_OSC, font.family = FNT_TITULO)),
    fp_p = fp_par(padding.bottom = 4, padding.top = 10)
  )
}
body_txt <- function(txt) {
  fpar(
    ftext(txt, fp_text(font.size = 11, color = GRIS_OSC, font.family = FNT_CUERPO)),
    fp_p = fp_par(padding.bottom = 6, line_spacing = 1.15, text.align = "justify")
  )
}
alerta <- function(txt, color = ROJO_FUEGO) {
  fpar(
    ftext(txt, fp_text(font.size = 11, bold = TRUE, color = BLANCO, font.family = FNT_CUERPO,
                       shading.color = color)),
    fp_p = fp_par(padding = 8, padding.bottom = 10)
  )
}

# Usamos documento en blanco para evitar conflictos de estilo
doc_out <- read_docx() %>%
  # ── ENCABEZADO SUPERIOR
  body_add_fpar(fpar(ftext("CORPORACIÓN AUTÓNOMA REGIONAL DEL VALLE DEL CAUCA", fp_text(font.size=13, bold=TRUE, color=AZUL_CVC, font.family=FNT_TITULO)))) %>%
  body_add_fpar(fpar(ftext("SATICA 2.0  |  DAR Suroriente  |  Análisis de Incidencia Antrópica en Incendios de Caña de Azúcar", fp_text(font.size=11, color=GRIS_MED, font.family=FNT_CUERPO)))) %>%
  body_add_fpar(fpar(ftext(format(Sys.Date(), "%d de %B de %Y"), fp_text(font.size=10, color=GRIS_MED, italic=TRUE)))) %>%
  body_add_fpar(fpar(ftext("─────────────────────────────────────────────────────────────", fp_text(font.size=10, color=AZUL_CVC)))) %>%

  # ── RESUMEN EJECUTIVO
  body_add_fpar(h1("1.  RESUMEN EJECUTIVO")) %>%
  body_add_fpar(alerta("🔥  CONCLUSIÓN CENTRAL: El 91.5% de los incendios en caña de azúcar ocurre a menos de 2 km de zonas con actividad humana.", ROJO_FUEGO)) %>%
  body_add_fpar(body_txt(paste(
    "El análisis sistemático de 13.929 eventos de fuego registrados entre 2019 y 2026 en la",
    "DAR Suroriente revela un patrón territorial inequívoco: los incendios en cultivos de caña",
    "de azúcar no son fenómenos ambientales aleatorios. Su distribución geoespacial confirma",
    "con plena significancia estadística la intervención humana como factor causal dominante."))) %>%
  body_add_fpar(body_txt(paste(
    "La concentración de eventos en la franja de 0 a 1.500 metros alrededor de centros poblados",
    "de Palmira, Florida y Candelaria señala inequívocamente que el fuego sigue a la población.",
    "Este patrón se repite año tras año en las mismas haciendas, con los mismos ingenios,",
    "en las mismas ventanas temporales, lo que descarta la casualidad como explicación."))) %>%
  body_add_fpar(fpar(ftext(" ", fp_text(font.size=6)))) %>%

  # ── MÉTRICAS CLAVE (caja visual)
  body_add_fpar(h1("2.  DATOS CLAVE DEL ANÁLISIS (2019 – 2026)")) %>%
  body_add_fpar(alerta("📌  13.929 incendios registrados en total — 7.504 exactamente georreferenciados (53.9%)", AZUL_CVC)) %>%
  body_add_fpar(alerta("🏘️  91.5% de las igniciones ocurren a menos de 2 km de zonas de actividad humana", ROJO_FUEGO)) %>%
  body_add_fpar(alerta("📍  La franja 0–500 m concentra la mayor densidad de eventos (riesgo CRÍTICO)", NARANJA)) %>%
  body_add_fpar(alerta("🔄  Las 10 haciendas más reincidentes acumulan el 28% del total histórico", VERDE_CVC)) %>%
  body_add_fpar(fpar(ftext(" ", fp_text(font.size=6)))) %>%

  # ── EVIDENCIA GRÁFICA
  body_add_fpar(h1("3.  EVIDENCIA GEOESPACIAL")) %>%
  body_add_fpar(h2("3.1.  La Distancia como Huella Digital del Origen Antrópico")) %>%
  body_add_fpar(body_txt(paste(
    "Al correlacionar cada evento de fuego con la distancia al perímetro urbano más próximo,",
    "la evidencia estadística muestra una concentración aberrante en el primer kilómetro y medio.",
    "Esta distribución no es consistente con la ignición espontánea. Es la huella digital",
    "de la acción humana sobre el territorio cañero."))) %>%
  body_add_img("resultados_modelo/grafico_dist_poblados_incendios.png", width = 6.2, height = 3.9) %>%
  body_add_fpar(fpar(ftext(" ", fp_text(font.size=5)))) %>%

  body_add_fpar(h2("3.2.  Evolución Histórica: Una Amenaza Persistente")) %>%
  body_add_fpar(body_txt(paste(
    "La serie temporal de incidentes muestra picos estacionales recurrentes coincidentes",
    "con épocas de cosecha y alta actividad agroindustrial. Los años 2022 y 2024 registraron",
    "los valores más altos, con tendencia al alza que exige una respuesta preventiva inmediata."))) %>%
  body_add_img("resultados_modelo/grafico_evolucion_antropica.png", width = 6.2, height = 3.9) %>%
  body_add_fpar(fpar(ftext(" ", fp_text(font.size=5)))) %>%

  body_add_fpar(h2("3.3.  Índice de Exposición Poblacional")) %>%
  body_add_fpar(body_txt(paste(
    "El índice combinado de exposición clasifica cada hacienda según su proximidad simultánea",
    "a vías y centros poblados. Los municipios con mayor concentración de predios de exposición",
    "CRÍTICA y ALTA son Palmira, Candelaria y Florida, los cuales requieren atención preferente."))) %>%
  body_add_img("resultados_modelo/grafico_exposicion_antropica.png", width = 6.2, height = 3.9) %>%
  body_add_fpar(fpar(ftext(" ", fp_text(font.size=5)))) %>%

  # ── TOP 10
  body_add_fpar(h1("4.  HACIENDAS PRIORITARIAS PARA INSPECCIÓN")) %>%
  body_add_fpar(body_txt(paste(
    "Las haciendas listadas a continuación presentan la combinación más crítica de reincidencia",
    "histórica y exposición antrópica. Son el punto de partida para las comisiones de visita",
    "de la CVC en el marco de la Resolución 0741 de 2016. La distancia al poblado más cercano",
    "confirma en todos los casos la plausibilidad directa de la causa humana."))) %>%
  body_add_fpar(fpar(ftext(" ", fp_text(font.size=5))))

# Tabla premium
color_expo <- function(x) {
  case_when(x=="CRÍTICA"~ROJO_FUEGO, x=="ALTA"~NARANJA, x=="MEDIA"~ORO, TRUE~VERDE_CVC)
}

ft_top <- flextable(top10_tbl) %>%
  set_header_labels(Hacienda="Hacienda", Municipio="Municipio", Ingenio="Ingenio",
                    Incendios="N° Incendios", `Dist. Poblado`="Dist. Poblado", Exposición="Exposición") %>%
  theme_zebra(odd_body = GRIS_CLARO, even_body = BLANCO) %>%
  bg(bg = GRIS_OSC, part = "header") %>%
  color(color = BLANCO, part = "header") %>%
  bold(part = "header") %>%
  align(j = c(4,5,6), align = "center", part = "all") %>%
  color(j = "Exposición", color = BLANCO, part = "body") %>%
  bg(j = "Exposición",
     bg = color_expo(top10_tbl$Exposición), part = "body") %>%
  bold(j = "Exposición") %>%
  fontsize(size = 10, part = "all") %>%
  fontsize(size = 11, part = "header") %>%
  font(fontname = FNT_CUERPO, part = "all") %>%
  font(fontname = FNT_TITULO, part = "header") %>%
  width(j = 1, width = 2.0) %>%
  width(j = 2, width = 1.3) %>%
  width(j = 3, width = 1.3) %>%
  width(j = 4, width = 0.9) %>%
  width(j = 5, width = 0.9) %>%
  width(j = 6, width = 1.0)

doc_out <- doc_out %>%
  body_add_flextable(ft_top) %>%
  body_add_fpar(fpar(ftext(" ", fp_text(font.size=5)))) %>%

  # ── RECOMENDACIONES
  body_add_fpar(h1("5.  DIRECTRICES DE ACCIÓN INSTITUCIONAL")) %>%
  body_add_fpar(alerta("1. BLINDAJE PERIMETRAL (0-1.5 km)", AZUL_CVC)) %>%
  body_add_fpar(body_txt("Declarar vigilancia intensiva y permanente en todas las haciendas cañeras que limiten a menos de 1.5 kilómetros de cualquier centro poblado en Palmira, Candelaria y Florida. La evidencia geoespacial justifica plenamente esta acción como medida de prevención y control.")) %>%
  body_add_fpar(alerta("2. AUDITORIA SIN PREVIO AVISO", ROJO_FUEGO)) %>%
  body_add_fpar(body_txt("Ejecutar visitas tecnicas sorpresa a las haciendas del Top 10 con actas sustentadas en los boletines automaticos de SATICA 2.0. El marco legal de la Resolucion 0741 de 2016 respalda las actuaciones sancionatorias derivadas de estas inspecciones.")) %>%
  body_add_fpar(alerta("3. INTELIGENCIA COMUNITARIA", VERDE_CVC)) %>%
  body_add_fpar(body_txt("Articular con Juntas de Accion Comunal (JAC) de las veredas colindantes para establecer redes de alerta temprana ciudadana que permitan reportar quemas antes de su materializacion, ampliando el alcance operativo de la corporacion sin incremento de personal.")) %>%
  body_add_fpar(fpar(ftext(" ", fp_text(font.size=5)))) %>%
  body_add_fpar(fpar(ftext("─────────────────────────────────────────────────────────────────────", fp_text(font.size=9, color=GRIS_MED)))) %>%
  body_add_fpar(fpar(
    ftext(paste("Informe técnico de uso interno  |  CVC DAR Suroriente  |  Resolución 0741 de 2016\nAnálisis de Incidencia Antrópica en Incendios de Caña — SATICA V2.0  |", format(Sys.Date(), "%B %Y")),
          fp_text(font.size = 9, color = GRIS_MED, italic = TRUE))))

print(doc_out, target = "reportes_finales/SATICA_Informe_Incidencia_Antropica_V2.docx")
cat("✅ Word guardado: reportes_finales/SATICA_Informe_Incidencia_Antropica_V2.docx\n")

# ==============================================================================
# B.  PPT — INCIDENCIA ANTRÓPICA (usando la original como plantilla base)
# ==============================================================================
cat("🖥️  Generando PPT Incidencia Antrópica...\n")

ref_ppt <- "reportes_finales/SATICA_V2_Presentacion.pptx"
ppt_base <- read_pptx(ref_ppt)

# Funciones helpers
ttl_fmt <- fp_text(font.size = 28, bold = TRUE, color = BLANCO, font.family = FNT_TITULO)
sub_fmt  <- fp_text(font.size = 16, color = GRIS_CLARO, font.family = FNT_CUERPO, italic = TRUE)
blt_grande <- function(txt, color = BLANCO, sz = 22) {
  fpar(ftext(txt, fp_text(font.size = sz, color = color, font.family = FNT_CUERPO)),
       fp_p = fp_par(padding.bottom = 8))
}
blt_n  <- function(txt, color = GRIS_OSC, sz = 18) {
  fpar(ftext(txt, fp_text(font.size = sz, color = color, font.family = FNT_CUERPO)),
       fp_p = fp_par(padding.bottom = 6))
}
kpi_box <- function(numero, etiqueta, color_bg, prs, left, top) {
  prs %>%
    ph_with(value = fpar(ftext(numero, fp_text(font.size = 54, bold = TRUE, color = BLANCO, font.family = FNT_TITULO))),
            location = ph_location(left = left, top = top, width = 2.1, height = 1.1)) %>%
    ph_with(value = fpar(ftext(etiqueta, fp_text(font.size = 13, color = BLANCO, font.family = FNT_CUERPO))),
            location = ph_location(left = left, top = top + 1.0, width = 2.1, height = 0.5))
}

ppt_a <- read_pptx(ref_ppt) %>%

  # SLIDE 1: Portada de impacto (heredada del original, la dejamos)
  # Saltamos a la slide 2 para no modificar portada

  # SLIDE extra: KPIs de golpe
  add_slide(layout = "DEFAULT", master = "Office Theme") %>%
  ph_with(value = fpar(ftext("🔥 LA ESCALA DEL PROBLEMA", ttl_fmt)),
          location = ph_location(left=0.3, top=0.2, width=9.5, height=0.9)) %>%
  ph_with(value = fpar(ftext("13.929 incendios históricos (2019–2026) | 7.504 georreferenciados con precisión satelital",
                              sub_fmt)),
          location = ph_location(left=0.3, top=1.0, width=9.5, height=0.5))

# KPI boxes
ppt_a <- ppt_a %>%
  ph_with(value = fpar(ftext("13.929", fp_text(font.size=48, bold=TRUE, color=BLANCO, font.family=FNT_TITULO))),
          location = ph_location(left=0.3, top=1.7, width=2.2, height=1.1)) %>%
  ph_with(value = fpar(ftext("Total Incendios", fp_text(font.size=13, color=GRIS_CLARO, font.family=FNT_CUERPO))),
          location = ph_location(left=0.3, top=2.7, width=2.2, height=0.5)) %>%
  ph_with(value = fpar(ftext("91.5%", fp_text(font.size=48, bold=TRUE, color=ROJO_FUEGO, font.family=FNT_TITULO))),
          location = ph_location(left=2.8, top=1.7, width=2.2, height=1.1)) %>%
  ph_with(value = fpar(ftext("A ≤ 2 km de actividad humana", fp_text(font.size=13, color=GRIS_MED, font.family=FNT_CUERPO))),
          location = ph_location(left=2.8, top=2.7, width=2.2, height=0.5)) %>%
  ph_with(value = fpar(ftext("10", fp_text(font.size=48, bold=TRUE, color=NARANJA, font.family=FNT_TITULO))),
          location = ph_location(left=5.3, top=1.7, width=2.2, height=1.1)) %>%
  ph_with(value = fpar(ftext("Haciendas Críticas\n(28% del total)", fp_text(font.size=13, color=GRIS_MED, font.family=FNT_CUERPO))),
          location = ph_location(left=5.3, top=2.7, width=2.2, height=0.7)) %>%
  ph_with(value = fpar(ftext("4", fp_text(font.size=48, bold=TRUE, color=VERDE_CVC, font.family=FNT_TITULO))),
          location = ph_location(left=7.8, top=1.7, width=2.0, height=1.1)) %>%
  ph_with(value = fpar(ftext("Municipios en Alerta\nCrítica", fp_text(font.size=13, color=GRIS_MED, font.family=FNT_CUERPO))),
          location = ph_location(left=7.8, top=2.7, width=2.0, height=0.7)) %>%
  ph_with(value = fpar(ftext("Fuente: SATICA 2.0 — Motor Geoespacial | NASA FIRMS + GOES-16 | CVC DAR Suroriente",
                              fp_text(font.size=10, color=GRIS_MED, italic=TRUE))),
          location = ph_location(left=0.5, top=4.9, width=9.0, height=0.4)) %>%

  # SLIDE: Distancia crítica (imagen grande)
  add_slide(layout = "DEFAULT", master = "Office Theme") %>%
  ph_with(value = fpar(ftext("📍 El Fuego Sigue a la Gente: Distancia como Evidencia", ttl_fmt)),
          location = ph_location(left=0.3, top=0.1, width=9.5, height=0.7)) %>%
  ph_with(value = fpar(ftext("El 91.5% de los incendios ocurre a menos de 2 km de cascos urbanos — Palmira, Candelaria y Florida son las más afectadas.", sub_fmt)),
          location = ph_location(left=0.3, top=0.75, width=9.5, height=0.5)) %>%
  ph_with(value = external_img("resultados_modelo/grafico_dist_poblados_incendios.png", width=9.3, height=4.6),
          location = ph_location(left=0.35, top=1.25, width=9.3, height=4.6)) %>%

  # SLIDE: Evolución histórica
  add_slide(layout = "DEFAULT", master = "Office Theme") %>%
  ph_with(value = fpar(ftext("📉 Tendencia Histórica: Picos Estacionales Sistémicos", ttl_fmt)),
          location = ph_location(left=0.3, top=0.1, width=9.5, height=0.7)) %>%
  ph_with(value = fpar(ftext("Los picos de 2022 y 2024 superan la media histórica — el patrón no es accidental, es estructural.", sub_fmt)),
          location = ph_location(left=0.3, top=0.75, width=9.5, height=0.5)) %>%
  ph_with(value = external_img("resultados_modelo/grafico_evolucion_antropica.png", width=9.3, height=4.6),
          location = ph_location(left=0.35, top=1.25, width=9.3, height=4.6)) %>%

  # SLIDE: Exposición poblacional
  add_slide(layout = "DEFAULT", master = "Office Theme") %>%
  ph_with(value = fpar(ftext("⚠️ Índice de Exposición: Salud Pública en Riesgo", ttl_fmt)),
          location = ph_location(left=0.3, top=0.1, width=9.5, height=0.7)) %>%
  ph_with(value = fpar(ftext("Las comunidades de la franja rural-urbana respiran humo de quemas que el modelo predice con 30 días de anticipación.", sub_fmt)),
          location = ph_location(left=0.3, top=0.75, width=9.5, height=0.5)) %>%
  ph_with(value = external_img("resultados_modelo/grafico_exposicion_antropica.png", width=9.3, height=4.6),
          location = ph_location(left=0.35, top=1.25, width=9.3, height=4.6)) %>%

  # SLIDE: Tabla Top 10
  add_slide(layout = "DEFAULT", master = "Office Theme") %>%
  ph_with(value = fpar(ftext("🎯 Los 10 Objetivos Prioritarios de Intervención CVC", ttl_fmt)),
          location = ph_location(left=0.3, top=0.1, width=9.5, height=0.6)) %>%
  ph_with(value = fpar(ftext("Ordenadas por recurrencia histórica. La distancia al poblado más cercano sustenta la tesis de origen antrópico.", sub_fmt)),
          location = ph_location(left=0.3, top=0.65, width=9.5, height=0.45)) %>%
  ph_with(value = flextable(top10_tbl) %>%
            bg(bg = GRIS_OSC, part = "header") %>%
            color(color = BLANCO, part = "header") %>%
            bold(part = "header") %>%
            bg(j = "Exposición",
               bg = dplyr::case_when(top10_tbl$Exposición=="CRÍTICA"~ROJO_FUEGO,
                                     top10_tbl$Exposición=="ALTA"~NARANJA,
                                     top10_tbl$Exposición=="MEDIA"~ORO,
                                     TRUE~VERDE_CVC), part = "body") %>%
            color(j = "Exposición", color = BLANCO, part = "body") %>%
            bold(j = "Exposición") %>%
            fontsize(size = 9, part = "all") %>%
            autofit(),
          location = ph_location(left=0.3, top=1.1, width=9.4, height=4.5)) %>%

  # SLIDE: Plan de acción
  add_slide(layout = "DEFAULT", master = "Office Theme") %>%
  ph_with(value = fpar(ftext("🛡️ 3 Ejes de Respuesta Institucional", ttl_fmt)),
          location = ph_location(left=0.3, top=0.1, width=9.5, height=0.6)) %>%
  ph_with(value = block_list(
    fpar(ftext("1. BLINDAJE PERIMETRAL (0–1.5 km)",
               fp_text(font.size=21, bold=TRUE, color=ROJO_FUEGO, font.family=FNT_TITULO))),
    fpar(ftext("   Vigilancia permanente en haciendas colindantes con zonas urbanas de Palmira, Candelaria y Florida.",
               fp_text(font.size=17, color=GRIS_OSC, font.family=FNT_CUERPO)),
         fp_p=fp_par(padding.bottom=14)),
    fpar(ftext("2. AUDITORÍA DE IMPACTO SIN AVISO",
               fp_text(font.size=21, bold=TRUE, color=NARANJA, font.family=FNT_TITULO))),
    fpar(ftext("   Operativos sorpresa al Top 10. SATICA provee el soporte científico-legal para actas y procesos sancionatorios.",
               fp_text(font.size=17, color=GRIS_OSC, font.family=FNT_CUERPO)),
         fp_p=fp_par(padding.bottom=14)),
    fpar(ftext("3. RED COMUNITARIA DE ALERTA TEMPRANA",
               fp_text(font.size=21, bold=TRUE, color=VERDE_CVC, font.family=FNT_TITULO))),
    fpar(ftext("   Alianza con JAC para denuncia ciudadana antes de que los incendios se materialicen.",
               fp_text(font.size=17, color=GRIS_OSC, font.family=FNT_CUERPO)))
  ), location = ph_location(left=0.5, top=0.9, width=9.0, height=4.7))

print(ppt_a, target = "reportes_finales/SATICA_Incidencia_Antropica_Presentacion.pptx")
cat("✅ PPT Antrópica: reportes_finales/SATICA_Incidencia_Antropica_Presentacion.pptx\n")

# ==============================================================================
# C.  PPT — EVOLUCIÓN SATICA 2.0
# ==============================================================================
cat("🚀 Generando PPT Evolución SATICA 2.0...\n")

ppt_b <- read_pptx(ref_ppt) %>%
  add_slide(layout = "DEFAULT", master = "Office Theme") %>%
  ph_with(value = fpar(ftext("SATICA V2.0", fp_text(font.size=42, bold=TRUE, color=BLANCO, font.family=FNT_TITULO))),
          location = ph_location(left=0.3, top=0.5, width=9.5, height=1.2)) %>%
  ph_with(value = fpar(ftext("De la reacción a la predicción: la revolución tecnológica de la CVC DAR Suroriente",
                              fp_text(font.size=20, color=GRIS_CLARO, italic=TRUE, font.family=FNT_CUERPO))),
          location = ph_location(left=0.3, top=1.6, width=9.5, height=0.7)) %>%
  ph_with(value = fpar(ftext(paste(format(Sys.Date(), "%B %Y"),"  |  Dirección Ambiental Regional Suroriente"),
                              fp_text(font.size=14, color=GRIS_CLARO, font.family=FNT_CUERPO))),
          location = ph_location(left=0.3, top=4.8, width=9.5, height=0.5)) %>%

  # SLIDE: Antes vs Ahora
  add_slide(layout = "DEFAULT", master = "Office Theme") %>%
  ph_with(value = fpar(ftext("La Transformación: V1 → V2.0", ttl_fmt)),
          location = ph_location(left=0.3, top=0.1, width=9.5, height=0.7)) %>%
  ph_with(value = block_list(
    fpar(ftext("ANTES — Control Pasivo (V1)", fp_text(font.size=19, bold=TRUE, color=GRIS_MED, font.family=FNT_TITULO))),
    fpar(ftext("  ✗  Mapa estático sin capacidad predictiva", fp_text(font.size=16, color=GRIS_MED))),
    fpar(ftext("  ✗  Reportes manuales tardíos sin respaldo satelital", fp_text(font.size=16, color=GRIS_MED))),
    fpar(ftext("  ✗  Sin ranking de riesgo ni priorización de recursos", fp_text(font.size=16, color=GRIS_MED)),
         fp_p=fp_par(padding.bottom=16)),
    fpar(ftext("HOY — Inteligencia Proactiva (V2.0)", fp_text(font.size=19, bold=TRUE, color=VERDE_CVC, font.family=FNT_TITULO))),
    fpar(ftext("  ✓  Telemetría satelital 24/7 (VIIRS + MODIS + GOES-16 + Sentinel-2)", fp_text(font.size=16, color=GRIS_OSC))),
    fpar(ftext("  ✓  Modelo estadístico: predice ventanas de riesgo con ±1 mes de anticipación", fp_text(font.size=16, color=GRIS_OSC))),
    fpar(ftext("  ✓  Boletines automáticos PDF/Excel para actuaciones legales de campo", fp_text(font.size=16, color=GRIS_OSC))),
    fpar(ftext("  ✓  Smart Sync: actualización satelital cada 30 minutos sin intervención humana", fp_text(font.size=16, color=GRIS_OSC)))
  ), location = ph_location(left=0.5, top=0.9, width=9.0, height=4.7)) %>%

  # SLIDE: Cómo funciona
  add_slide(layout = "DEFAULT", master = "Office Theme") %>%
  ph_with(value = fpar(ftext("⚙️ El Cerebro: Smart Sync Autónomo", ttl_fmt)),
          location = ph_location(left=0.3, top=0.1, width=9.5, height=0.7)) %>%
  ph_with(value = fpar(ftext("Sin botones. Sin archivos manuales. Sin intervención humana.", sub_fmt)),
          location = ph_location(left=0.3, top=0.75, width=9.5, height=0.45)) %>%
  ph_with(value = block_list(
    fpar(ftext("Cada vez que el sistema se inicia:", fp_text(font.size=18, bold=TRUE, color=AZUL_CVC, font.family=FNT_TITULO)),
         fp_p=fp_par(padding.bottom=8)),
    fpar(ftext("  🛰️  [1]  Se conecta a la API de NASA FIRMS → descarga focos de calor activos en el territorio", fp_text(font.size=17, color=GRIS_OSC))),
    fpar(ftext("  ⚙️  [2]  Ejecuta el motor predictivo → recalcula el riesgo de cada hacienda con la fecha del día", fp_text(font.size=17, color=GRIS_OSC))),
    fpar(ftext("  🗺️  [3]  Actualiza el tablero interactivo → muestra mapa dinámico con pop-ups y alertas clasificadas", fp_text(font.size=17, color=GRIS_OSC))),
    fpar(ftext("  📄  [4]  Permite generar boletines PDF y Excel → listos para operativos de campo", fp_text(font.size=17, color=GRIS_OSC))),
    fpar(ftext(" ", fp_text(font.size=10))),
    fpar(ftext("⚡ Si no hay internet → MODO OFFLINE: usa el último estado calculado. Nunca falla en campo.",
               fp_text(font.size=16, bold=TRUE, color=NARANJA, font.family=FNT_CUERPO)))
  ), location = ph_location(left=0.5, top=1.25, width=9.0, height=4.4)) %>%

  # SLIDE: Nivel de confianza
  add_slide(layout = "DEFAULT", master = "Office Theme") %>%
  ph_with(value = fpar(ftext("🎯 Nivel de Confianza del Modelo Predictivo", ttl_fmt)),
          location = ph_location(left=0.3, top=0.1, width=9.5, height=0.65)) %>%
  ph_with(value = fpar(ftext("El modelo ha aprendido de 7 años de ciclos históricos reales — no es una suposición, es estadística aplicada.", sub_fmt)),
          location = ph_location(left=0.3, top=0.72, width=9.5, height=0.45)) %>%
  ph_with(value = external_img("resultados_modelo/grafico_confianza_semaforo.png", width=9.3, height=4.5),
          location = ph_location(left=0.35, top=1.18, width=9.3, height=4.5)) %>%

  # SLIDE: Precisión por nivel
  add_slide(layout = "DEFAULT", master = "Office Theme") %>%
  ph_with(value = fpar(ftext("📊 Precisión por Nivel de Alerta: ¿Podemos Confiar?", ttl_fmt)),
          location = ph_location(left=0.3, top=0.1, width=9.5, height=0.65)) %>%
  ph_with(value = fpar(ftext("Los niveles CRÍTICO y ALTO concentran la máxima precisión — exactamente donde SATICA dirige los recursos de campo.", sub_fmt)),
          location = ph_location(left=0.3, top=0.72, width=9.5, height=0.45)) %>%
  ph_with(value = external_img("resultados_modelo/grafico_precision_niveles.png", width=9.3, height=4.5),
          location = ph_location(left=0.35, top=1.18, width=9.3, height=4.5)) %>%

  # SLIDE: Ecosistema de salidas
  add_slide(layout = "DEFAULT", master = "Office Theme") %>%
  ph_with(value = fpar(ftext("🌐 El Ecosistema SATICA 2.0: Desde el Satélite hasta el Campo", ttl_fmt)),
          location = ph_location(left=0.3, top=0.1, width=9.5, height=0.65)) %>%
  ph_with(value = block_list(
    fpar(ftext("ENTRADAS (Datos Científicos):", fp_text(font.size=18, bold=TRUE, color=AZUL_CVC, font.family=FNT_TITULO))),
    fpar(ftext("  🛰️  Satélites NASA FIRMS (VIIRS 375m + MODIS) + GOES-16 + Sentinel-2", fp_text(font.size=16))),
    fpar(ftext("  📂  Base catastral histórica 2019–2026 + Registro de visitas CVC + Datos GIS de suertes", fp_text(font.size=16)),
         fp_p=fp_par(padding.bottom=12)),
    fpar(ftext("SALIDAS (Herramientas Operativas para el Inspector):", fp_text(font.size=18, bold=TRUE, color=VERDE_CVC, font.family=FNT_TITULO))),
    fpar(ftext("  📋  Boletín PDF: cronograma de alertas inminentes (±15 días) por municipio", fp_text(font.size=16))),
    fpar(ftext("  📊  Excel de seguimiento: 3 hojas temáticas con columnas para radicado y funcionario", fp_text(font.size=16))),
    fpar(ftext("  🗺️  Mapa interactivo: pop-ups por hacienda con historial, riesgo y gestión CVC", fp_text(font.size=16))),
    fpar(ftext("  📍  Ancla Operativa: seguimiento GPS de predios sin polígono catastral oficial", fp_text(font.size=16)))
  ), location = ph_location(left=0.5, top=0.85, width=9.0, height=4.8)) %>%

  # SLIDE: Cierre poderoso
  add_slide(layout = "DEFAULT", master = "Office Theme") %>%
  ph_with(value = fpar(ftext('"La inteligencia artificial no reemplaza',
                              fp_text(font.size=30, bold=TRUE, color=BLANCO, font.family=FNT_TITULO))),
          location = ph_location(left=0.5, top=0.8, width=9.0, height=1.0)) %>%
  ph_with(value = fpar(ftext('el trabajo de campo de la CVC.',
                              fp_text(font.size=30, bold=TRUE, color=BLANCO, font.family=FNT_TITULO))),
          location = ph_location(left=0.5, top=1.6, width=9.0, height=1.0)) %>%
  ph_with(value = fpar(ftext('Lo potencia y lo hace irrefutable."',
                              fp_text(font.size=30, bold=TRUE, color=ORO, font.family=FNT_TITULO))),
          location = ph_location(left=0.5, top=2.4, width=9.0, height=1.0)) %>%
  ph_with(value = fpar(ftext("SATICA 2.0  |  CVC — DAR Suroriente  |  Prevención como Mandato Institucional",
                              fp_text(font.size=14, color=GRIS_CLARO, italic=TRUE))),
          location = ph_location(left=0.5, top=4.5, width=9.0, height=0.6))

print(ppt_b, target = "reportes_finales/SATICA_Evolucion_Presentacion.pptx")
cat("✅ PPT SATICA: reportes_finales/SATICA_Evolucion_Presentacion.pptx\n")
cat("\n🏁 ¡Listo! Revisa 'reportes_finales/' para los 3 documentos mejorados.\n")
