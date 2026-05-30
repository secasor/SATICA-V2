# ==============================================================================
# GENERADOR DE PRESENTACIÓN SATICA V2.0 — ESTILO PREMIUM MODERNO
# ==============================================================================
# Dependencias: officer, ggplot2, dplyr
# Ejecutar desde la raíz del proyecto SATICA V2.0
# ==============================================================================

options(repos = c(CRAN = "https://cloud.r-project.org"))
paquetes <- c("officer", "ggplot2", "dplyr", "scales")
for (pkg in paquetes) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, quiet = TRUE)
}

suppressPackageStartupMessages({
  library(officer)
  library(ggplot2)
  library(dplyr)
  library(scales)
})

cat("🎨 Iniciando generación de presentación SATICA V2.0...\n")

# ==============================================================================
# 1. PALETA Y CONFIGURACIÓN GLOBAL
# ==============================================================================
COLOR_BG       <- "#0f172a"
COLOR_ACCENT   <- "#38bdf8"
COLOR_RED      <- "#ef4444"
COLOR_ORANGE   <- "#f59e0b"
COLOR_YELLOW   <- "#facc15"
COLOR_GREEN    <- "#22c55e"
COLOR_PURPLE   <- "#a855f7"
COLOR_WHITE    <- "#f8fafc"
COLOR_SUBTITLE <- "#94a3b8"
FONT_MAIN      <- "Calibri"

W <- 12; H <- 6.75  # Widescreen 16:9

# Helper limpio: fp_par sin space_after (usamos padding.bottom)
mkpar <- function(align = "left", lsp = 1.2, pad_b = 0) {
  fp_par(text.align = align, line_spacing = lsp, padding.bottom = pad_b)
}

mktxt <- function(t, col = COLOR_WHITE, sz = 12, bold = FALSE, ital = FALSE) {
  fp_text(color = col, font.size = sz, bold = bold, italic = ital, font.family = FONT_MAIN)
}

mkfp <- function(txt, col = COLOR_WHITE, sz = 12, bold = FALSE, ital = FALSE,
                 align = "left", lsp = 1.2, pad_b = 4) {
  fpar(ftext(txt, mktxt(txt, col, sz, bold, ital)),
       fp_p = mkpar(align, lsp, pad_b))
}

# ==============================================================================
# 2. GRÁFICOS GGPLOT2
# ==============================================================================
TMP_DIR <- tempdir()

## 2.1 Precisión por nivel ---------------------------------------------------
df_confianza <- data.frame(
  nivel    = factor(c("CERTEZA\nFACTUAL", "CERTEZA\nALTA", "PREDICCIÓN\nML",
                      "OBSERVACIÓN", "SIN\nHISTORIAL"),
                    levels = c("CERTEZA\nFACTUAL", "CERTEZA\nALTA",
                               "PREDICCIÓN\nML", "OBSERVACIÓN", "SIN\nHISTORIAL")),
  precision = c(98.1, 86.4, 72.3, 44.7, 11.9),
  color     = c("#ef4444", "#f59e0b", "#a855f7", "#38bdf8", "#22c55e")
)

p_conf <- ggplot(df_confianza, aes(x = nivel, y = precision, fill = nivel)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = paste0(precision, "%")), vjust = -0.6,
            color = "white", size = 5.5, fontface = "bold") +
  geom_hline(yintercept = 15, linetype = "dashed", color = "#64748b", linewidth = 0.7) +
  annotate("text", x = 0.6, y = 18, label = "Tasa base: ~15%",
           color = "#64748b", size = 3.5, hjust = 0) +
  scale_fill_manual(values = setNames(df_confianza$color, df_confianza$nivel)) +
  scale_y_continuous(limits = c(0, 115), labels = function(x) paste0(x, "%")) +
  labs(x = NULL, y = "Precisión (%)") +
  theme_minimal(base_size = 13) +
  theme(
    plot.background  = element_rect(fill = "#1e293b", color = NA),
    panel.background = element_rect(fill = "#1e293b", color = NA),
    axis.text.x      = element_text(color = "white",  size = 10),
    axis.text.y      = element_text(color = "#94a3b8", size = 9),
    axis.title.y     = element_text(color = "#94a3b8", size = 10),
    panel.grid.major.y = element_line(color = "#334155", linewidth = 0.4),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )

path_conf <- file.path(TMP_DIR, "conf.png")
ggsave(path_conf, p_conf, width = 8, height = 4.5, dpi = 150, bg = "#1e293b")

## 2.2 Recall acumulado -------------------------------------------------------
df_recall <- data.frame(
  nivel   = factor(c("CERTEZA\nFACTUAL", "+ CERTEZA\nALTA",
                     "+ PREDICCIÓN\nML", "+ OBSERVACIÓN"),
                   levels = c("CERTEZA\nFACTUAL", "+ CERTEZA\nALTA",
                              "+ PREDICCIÓN\nML", "+ OBSERVACIÓN")),
  recall  = c(62.0, 78.5, 91.3, 98.1),
  color   = c("#ef4444", "#f59e0b", "#a855f7", "#22c55e")
)

p_rec <- ggplot(df_recall, aes(x = nivel, y = recall, group = 1)) +
  geom_area(fill = "#38bdf8", alpha = 0.15) +
  geom_line(color = "#38bdf8", linewidth = 2.2) +
  geom_point(aes(color = color), size = 7, show.legend = FALSE) +
  geom_text(aes(label = paste0(recall, "%")), vjust = -1.3,
            color = "white", size = 5.2, fontface = "bold") +
  scale_color_identity() +
  scale_y_continuous(limits = c(0, 115), labels = function(x) paste0(x, "%")) +
  labs(x = NULL, y = "Recall acumulado (%)") +
  theme_minimal(base_size = 13) +
  theme(
    plot.background  = element_rect(fill = "#1e293b", color = NA),
    panel.background = element_rect(fill = "#1e293b", color = NA),
    axis.text.x      = element_text(color = "white",  size = 10),
    axis.text.y      = element_text(color = "#94a3b8", size = 9),
    axis.title.y     = element_text(color = "#94a3b8", size = 10),
    panel.grid.major.y = element_line(color = "#334155", linewidth = 0.4),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )

path_rec <- file.path(TMP_DIR, "rec.png")
ggsave(path_rec, p_rec, width = 8, height = 4.5, dpi = 150, bg = "#1e293b")

## 2.3 Satélites: tabla visual ------------------------------------------------
df_sat <- data.frame(
  satelite = rev(c("VIIRS – Suomi-NPP", "VIIRS – NOAA-20",
                    "MODIS – Terra/Aqua", "Sentinel-2 (Copernicus)", "HYSPLIT (NOAA)")),
  dato     = rev(c("Focos térmicos · 375 m · Tiempo Real",
                    "Respaldo y redundancia del sensor VIIRS",
                    "Potencia Radiativa del Fuego · 1 km",
                    "NDVI+NBR · Biomasa 10 m · Google Earth Engine",
                    "Trayectoria de humo a 6 horas · Open-Meteo")),
  color    = rev(c("#ef4444", "#f59e0b", "#f97316", "#38bdf8", "#a855f7")),
  y        = 1:5
)

p_sat <- ggplot(df_sat) +
  geom_tile(aes(x = 1, y = y, fill = color), width = 0.9, height = 0.75,
            show.legend = FALSE) +
  geom_text(aes(x = 1, y = y, label = satelite), color = "white",
            size = 3.6, fontface = "bold") +
  geom_text(aes(x = 2.6, y = y, label = dato), color = "#cbd5e1",
            size = 3.3, hjust = 0) +
  scale_fill_identity() +
  scale_x_continuous(limits = c(0.5, 5.5)) +
  scale_y_continuous(limits = c(0.3, 5.7)) +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "#1e293b", color = NA),
    panel.background = element_rect(fill = "#1e293b", color = NA),
    plot.margin = margin(15, 15, 15, 15)
  )

path_sat <- file.path(TMP_DIR, "sat.png")
ggsave(path_sat, p_sat, width = 7, height = 4, dpi = 150, bg = "#1e293b")

## 2.4 Roadmap ----------------------------------------------------------------
df_road <- data.frame(
  x     = 1:6,
  label = c("Georref.\nTotal", "Portal Web\nCVC", "App Móvil\nCampo",
            "CDIAC\nClima", "Régimen\nQuemas", "IA V10\nAutolearn"),
  estado = c("En Curso", "En Curso", "Plan.", "Plan.", "Plan.", "Futuro"),
  color  = c("#22c55e", "#22c55e", "#38bdf8", "#38bdf8", "#a855f7", "#a855f7")
)

p_road <- ggplot(df_road) +
  annotate("segment", x = 1, xend = 6, y = 1, yend = 1,
           color = "#334155", linewidth = 2, lineend = "round") +
  geom_point(aes(x = x, y = 1, color = color), size = 14,
             show.legend = FALSE) +
  geom_text(aes(x = x, y = 1, label = as.character(x)),
            color = "white", size = 5, fontface = "bold") +
  geom_text(aes(x = x, y = 0.7, label = label),
            color = "#cbd5e1", size = 2.9, lineheight = 0.9) +
  geom_text(aes(x = x, y = 1.25, label = estado, color = color),
            size = 2.9, fontface = "bold", show.legend = FALSE) +
  scale_color_identity() +
  scale_x_continuous(limits = c(0.5, 6.5)) +
  scale_y_continuous(limits = c(0.4, 1.5)) +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "#1e293b", color = NA),
    panel.background = element_rect(fill = "#1e293b", color = NA),
    plot.margin = margin(15, 20, 15, 20)
  )

path_road <- file.path(TMP_DIR, "road.png")
ggsave(path_road, p_road, width = 9, height = 2.5, dpi = 150, bg = "#1e293b")

cat("  ✅ Gráficos generados.\n")

# ==============================================================================
# 3. HELPERS PARA DIAPOSITIVAS
# ==============================================================================

bg_slide <- function(prs, col = COLOR_BG) {
  ph_with(prs,
    location = ph_location(left = 0, top = 0, width = W, height = H),
    value = block_list(fpar(ftext("", mktxt("", col, 1))))
  )
}

add_title_block <- function(prs, title, subtitle = NULL,
                             title_color = COLOR_ACCENT, sz_title = 26) {
  prs <- ph_with(prs,
    location = ph_location(left = 0.4, top = 0.18, width = 11.2, height = 0.68),
    value = block_list(
      fpar(ftext(title, mktxt("", title_color, sz_title, bold = TRUE)),
           fp_p = mkpar("left", 1.0))
    )
  )
  if (!is.null(subtitle)) {
    prs <- ph_with(prs,
      location = ph_location(left = 0.4, top = 0.82, width = 11.2, height = 0.35),
      value = block_list(
        fpar(ftext(subtitle, mktxt("", COLOR_SUBTITLE, 11.5)),
             fp_p = mkpar("left", 1.0))
      )
    )
  }
  prs
}

add_txt <- function(prs, lines, left, top, w, h,
                    col = COLOR_SUBTITLE, sz = 11, bold = FALSE,
                    ital = FALSE, align = "left", lsp = 1.3, pad_b = 4) {
  blk <- lapply(lines, function(l) {
    fpar(ftext(l, mktxt("", col, sz, bold, ital)),
         fp_p = mkpar(align, lsp, pad_b))
  })
  ph_with(prs, location = ph_location(left = left, top = top, width = w, height = h),
          value = do.call(block_list, blk))
}

stat_box <- function(prs, num, label, left, top, w = 2.3, h = 1.25,
                     col_n = COLOR_ACCENT, sz_n = 30) {
  ph_with(prs,
    location = ph_location(left = left, top = top, width = w, height = h),
    value = block_list(
      fpar(ftext(num, mktxt("", col_n, sz_n, bold = TRUE)),
           fp_p = mkpar("center", 1.0, 2)),
      fpar(ftext(label, mktxt("", COLOR_SUBTITLE, 9.5)),
           fp_p = mkpar("center", 1.1))
    )
  )
}

# ==============================================================================
# 4. CONSTRUCCIÓN
# ==============================================================================
prs <- read_pptx()

# ─────────────────────────────────────────────────────────────────────────────
# SLIDE 1 — PORTADA
# ─────────────────────────────────────────────────────────────────────────────
prs <- add_slide(prs, layout = "Blank", master = "Office Theme")
prs <- bg_slide(prs)

# Franja izquierda de acento
prs <- ph_with(prs,
  location = ph_location(left = 0, top = 0, width = 0.32, height = H),
  value = block_list(fpar(ftext(strrep("▌", 20),
                                mktxt("", COLOR_ACCENT, 36, bold = TRUE)),
                          fp_p = mkpar("left", 1.0)))
)

# Organización
prs <- ph_with(prs,
  location = ph_location(left = 0.55, top = 1.1, width = 10, height = 0.4),
  value = block_list(fpar(
    ftext("CORPORACIÓN AUTÓNOMA REGIONAL DEL VALLE DEL CAUCA — DAR SURORIENTE",
          mktxt("", COLOR_SUBTITLE, 9, bold = TRUE)),
    fp_p = mkpar("left")
  ))
)

# Título principal
prs <- ph_with(prs,
  location = ph_location(left = 0.55, top = 1.55, width = 10.5, height = 1.7),
  value = block_list(
    fpar(ftext("SATICA V2.0", mktxt("", COLOR_WHITE, 68, bold = TRUE)),
         fp_p = mkpar("left", 0.9, 0)),
    fpar(ftext("Sistema de Alertas Tempranas de Incendios en Caña de Azúcar",
               mktxt("", COLOR_ACCENT, 20)),
         fp_p = mkpar("left", 1.0))
  )
)

# Línea
prs <- ph_with(prs,
  location = ph_location(left = 0.55, top = 3.35, width = 9, height = 0.06),
  value = block_list(fpar(
    ftext(strrep("─", 110), mktxt("", COLOR_ACCENT, 7)),
    fp_p = mkpar("left")
  ))
)

# Subtítulo
prs <- ph_with(prs,
  location = ph_location(left = 0.55, top = 3.45, width = 10, height = 0.5),
  value = block_list(fpar(
    ftext("Monitoreo satelital continuo  ·  Inteligencia Artificial XGBoost V9  ·  Alertas en tiempo real",
          mktxt("", COLOR_SUBTITLE, 12)),
    fp_p = mkpar("left")
  ))
)

# Pie
prs <- ph_with(prs,
  location = ph_location(left = 0.55, top = 6.05, width = 10, height = 0.5),
  value = block_list(fpar(
    ftext("Abril 2026  │  Presentación Ejecutiva  │  Corporación Autónoma Regional del Valle del Cauca",
          mktxt("", "#475569", 9)),
    fp_p = mkpar("left")
  ))
)
cat("  ✅ Slide 1 — Portada\n")

# ─────────────────────────────────────────────────────────────────────────────
# SLIDE 2 — ¿QUÉ ES SATICA?
# ─────────────────────────────────────────────────────────────────────────────
prs <- add_slide(prs, layout = "Blank", master = "Office Theme")
prs <- bg_slide(prs)
prs <- add_title_block(prs,
  title    = "¿QUÉ ES SATICA?",
  subtitle = "El \"Ojo de Halcón\" de la CVC para proteger el medio ambiente del Valle del Cauca"
)

# Descripción columna izquierda
prs <- ph_with(prs,
  location = ph_location(left = 0.4, top = 1.2, width = 5.6, height = 5.2),
  value = block_list(
    fpar(ftext("SATICA es mucho más que un software.",
               mktxt("", COLOR_WHITE, 13.5, bold = TRUE)),
         fp_p = mkpar("left", 1.0, 8)),
    fpar(ftext("Es una plataforma de inteligencia ambiental que fusiona datos de satélites NASA, modelos de Inteligencia Artificial y cartografía oficial de la CVC para predecir, detectar y responder a incendios en caña de azúcar antes de que se salgan de control.",
               mktxt("", COLOR_SUBTITLE, 11.5)),
         fp_p = mkpar("left", 1.35, 14)),
    fpar(ftext("Misión Ambiental", mktxt("", COLOR_ACCENT, 12, bold = TRUE)),
         fp_p = mkpar("left", 1.0, 5)),
    fpar(ftext("•  Detectar focos antes de que sean incontrolables.",
               mktxt("", COLOR_WHITE, 11)),
         fp_p = mkpar("left", 1.1, 3)),
    fpar(ftext("•  Reducir emisiones de material particulado (PM2.5).",
               mktxt("", COLOR_WHITE, 11)),
         fp_p = mkpar("left", 1.1, 3)),
    fpar(ftext("•  Proteger comunidades, fauna y ecosistemas locales.",
               mktxt("", COLOR_WHITE, 11)),
         fp_p = mkpar("left", 1.1, 3)),
    fpar(ftext("•  Blindaje jurídico a técnicos CVC (Resolución 0741/2016).",
               mktxt("", COLOR_WHITE, 11)),
         fp_p = mkpar("left", 1.1, 0))
  )
)

# Stats columna derecha
prs <- stat_box(prs, "5",     "Satélites\nintegrados",      left = 6.3, top = 1.2,  col_n = COLOR_ACCENT)
prs <- stat_box(prs, "98.1%", "Certeza\nFactual",            left = 9.1, top = 1.2,  col_n = COLOR_RED,    sz_n = 26)
prs <- stat_box(prs, "6+",    "Años de datos\nhistóricos",   left = 6.3, top = 2.65, col_n = COLOR_GREEN)
prs <- stat_box(prs, "15 min","Ciclo de\nalertas",            left = 9.1, top = 2.65, col_n = COLOR_ORANGE, sz_n = 22)
prs <- stat_box(prs, "8×",    "Más preciso\nque el azar",    left = 6.3, top = 4.1,  col_n = COLOR_PURPLE)
prs <- stat_box(prs, "40%",   "Predios con\nriesgo silencioso", left = 9.1, top = 4.1, col_n = COLOR_YELLOW, sz_n = 26)

cat("  ✅ Slide 2 — ¿Qué es SATICA?\n")

# ─────────────────────────────────────────────────────────────────────────────
# SLIDE 3 — FUNCIONES PRINCIPALES
# ─────────────────────────────────────────────────────────────────────────────
prs <- add_slide(prs, layout = "Blank", master = "Office Theme")
prs <- bg_slide(prs)
prs <- add_title_block(prs,
  title    = "FUNCIONES PRINCIPALES",
  subtitle = "5 módulos integrados que conforman el sistema de inteligencia ambiental"
)

funciones <- list(
  list(icon = "Radar\nSatelital",   c = COLOR_RED,
       d = c("4 sensores NASA activos.", "Focos térmicos en < 15 min.",
             "Cruce automático contra mapa catastral.", "Alertas Telegram + Deep Link.")),
  list(icon = "Análisis\nBiomasa",  c = COLOR_GREEN,
       d = c("Sentinel-2 resolución 10 m.", "NDVI = Nivel de humedad caña.",
             "NBR = Cicatrices post-quema.", "Google Earth Engine.")),
  list(icon = "IA XGBoost\nV9",    c = COLOR_PURPLE,
       d = c("Entrenado 2019–2024 datos reales.", "Opera con cielo nublado.",
             "Variables: vías, pueblos, recurrencia.", "8× más preciso que el azar.")),
  list(icon = "Simulación\nHumo",   c = COLOR_ACCENT,
       d = c("Modelo HYSPLIT + Open-Meteo.", "Trayectoria humo a 6 horas.",
             "Identifica comunidades en riesgo.", "Visible en mapa interactivo.")),
  list(icon = "Centro\nOperaciones", c = COLOR_ORANGE,
       d = c("Boletines visita preventiva PDF/CSV.", "Exportación KML, Shapefile.",
             "Red de riesgo por ingenio.", "Historial auditado 2019–2026."))
)

for (i in seq_along(funciones)) {
  f  <- funciones[[i]]
  Lf <- 0.18 + (i - 1) * 2.35

  prs <- ph_with(prs,
    location = ph_location(left = Lf, top = 1.2, width = 2.2, height = 0.8),
    value = block_list(
      fpar(ftext(f$icon, mktxt("", f$c, 12, bold = TRUE)),
           fp_p = mkpar("center", 1.1))
    )
  )

  desc_blk <- lapply(f$d, function(dd) {
    fpar(ftext(paste0("• ", dd), mktxt("", COLOR_SUBTITLE, 9.5)),
         fp_p = mkpar("left", 1.2, 4))
  })

  prs <- ph_with(prs,
    location = ph_location(left = Lf, top = 2.1, width = 2.2, height = 4.4),
    value = do.call(block_list, desc_blk)
  )
}

cat("  ✅ Slide 3 — Funciones\n")

# ─────────────────────────────────────────────────────────────────────────────
# SLIDE 4 — LOS 5 SATÉLITES
# ─────────────────────────────────────────────────────────────────────────────
prs <- add_slide(prs, layout = "Blank", master = "Office Theme")
prs <- bg_slide(prs)
prs <- add_title_block(prs,
  title    = "5 SATÉLITES · 1 SISTEMA",
  subtitle = "Cada sensor aporta una capa única. SATICA los funde en una sola alerta integrada."
)

# Gráfico izquierdo
prs <- ph_with(prs,
  location = ph_location(left = 0.3, top = 1.15, width = 5.8, height = 3.7),
  value = external_img(path_sat, width = 5.8, height = 3.7)
)

# Lista derecha
sats <- list(
  list(n = "VIIRS – Suomi-NPP",    c = COLOR_RED,    d = "Focos térmicos activos · 375 m · NRT"),
  list(n = "VIIRS – NOAA-20",      c = COLOR_ORANGE, d = "Respaldo y redundancia del sensor VIIRS"),
  list(n = "MODIS – Terra/Aqua",   c = "#f97316",    d = "Potencia Radiativa del Fuego (FRP) · 1 km"),
  list(n = "Sentinel-2 Copernicus",c = COLOR_ACCENT, d = "NDVI & NBR · Biomasa 10 m · Google EE"),
  list(n = "HYSPLIT (NOAA)",       c = COLOR_PURPLE, d = "Trayectoria humo 6 h · viento Open-Meteo")
)

for (j in seq_along(sats)) {
  s    <- sats[[j]]
  top_j <- 1.18 + (j - 1) * 0.97
  prs <- ph_with(prs,
    location = ph_location(left = 6.4, top = top_j, width = 5.3, height = 0.87),
    value = block_list(
      fpar(ftext(s$n, mktxt("", s$c, 12, bold = TRUE)), fp_p = mkpar("left", 1.0, 3)),
      fpar(ftext(s$d, mktxt("", COLOR_SUBTITLE, 10.5)), fp_p = mkpar("left", 1.0))
    )
  )
}

cat("  ✅ Slide 4 — Satélites\n")

# ─────────────────────────────────────────────────────────────────────────────
# SLIDE 5 — FLUJO DE FUNCIONAMIENTO
# ─────────────────────────────────────────────────────────────────────────────
prs <- add_slide(prs, layout = "Blank", master = "Office Theme")
prs <- bg_slide(prs)
prs <- add_title_block(prs,
  title    = "¿CÓMO FUNCIONA SATICA?",
  subtitle = "Desde el satélite hasta el celular del técnico en menos de 15 minutos"
)

pasos <- list(
  list(n = "01", t = "Órbita\nSatelital",     c = COLOR_RED,
       d = "VIIRS, MODIS y Sentinel-2 registran anomalías térmicas y de biomasa sobre el Valle del Cauca."),
  list(n = "02", t = "NASA\nFIRMS API",        c = COLOR_ORANGE,
       d = "SATICA consulta automáticamente cada 15 min la base de datos en vivo de la NASA."),
  list(n = "03", t = "Cruce\nCatastral",       c = COLOR_YELLOW,
       d = "Cada foco se cruza contra el mapa oficial de suertes de caña (SOR_OK.shp) de la CVC."),
  list(n = "04", t = "Análisis\nde Riesgo",    c = COLOR_ACCENT,
       d = "El motor XGBoost V9 calcula la criticidad: historial + telemetría + distancias."),
  list(n = "05", t = "Alerta\nTelegram",       c = COLOR_PURPLE,
       d = "Si el predio es CRÍTICO, se envía Ficha de Acción Rápida con coordenadas y deep link al Dashboard.")
)

for (k in seq_along(pasos)) {
  p2  <- pasos[[k]]
  Lk  <- 0.2 + (k - 1) * 2.38

  prs <- ph_with(prs,
    location = ph_location(left = Lk, top = 1.2, width = 2.2, height = 0.9),
    value = block_list(
      fpar(ftext(p2$n, mktxt("", p2$c, 36, bold = TRUE)),
           fp_p = mkpar("center", 1.0))
    )
  )

  if (k < length(pasos)) {
    prs <- ph_with(prs,
      location = ph_location(left = Lk + 2.05, top = 1.4, width = 0.38, height = 0.5),
      value = block_list(fpar(ftext("→", mktxt("", "#475569", 22)),
                              fp_p = mkpar("center")))
    )
  }

  prs <- ph_with(prs,
    location = ph_location(left = Lk, top = 2.15, width = 2.2, height = 0.5),
    value = block_list(fpar(ftext(p2$t, mktxt("", COLOR_WHITE, 11, bold = TRUE)),
                            fp_p = mkpar("center", 1.0)))
  )

  prs <- ph_with(prs,
    location = ph_location(left = Lk, top = 2.75, width = 2.2, height = 3.8),
    value = block_list(fpar(ftext(p2$d, mktxt("", COLOR_SUBTITLE, 10)),
                            fp_p = mkpar("center", 1.3)))
  )
}

cat("  ✅ Slide 5 — Flujo\n")

# ─────────────────────────────────────────────────────────────────────────────
# SLIDE 6 — CONFIANZA 98.1%
# ─────────────────────────────────────────────────────────────────────────────
prs <- add_slide(prs, layout = "Blank", master = "Office Theme")
prs <- bg_slide(prs)
prs <- add_title_block(prs,
  title    = "NIVEL DE CONFIANZA: 98.1%",
  subtitle = "Back-Testing riguroso: 6 años de datos para entrenar · 1 año de validación ciega",
  title_color = COLOR_RED, sz_title = 28
)

prs <- ph_with(prs,
  location = ph_location(left = 0.3, top = 1.15, width = 6.5, height = 3.9),
  value = external_img(path_conf, width = 6.5, height = 3.9)
)

prs <- ph_with(prs,
  location = ph_location(left = 7.1, top = 1.15, width = 4.65, height = 5.35),
  value = block_list(
    fpar(ftext("El Experimento", mktxt("", COLOR_ACCENT, 13.5, bold = TRUE)),
         fp_p = mkpar("left", 1.0, 7)),
    fpar(ftext("Se entrenó la IA con incendios reales de 2019–2024 y luego se le preguntó cuáles predios ardieron en 2025, sin que el sistema supiera las respuestas de antemano.",
               mktxt("", COLOR_SUBTITLE, 11)),
         fp_p = mkpar("left", 1.3, 14)),
    fpar(ftext("5 Niveles de Certeza", mktxt("", COLOR_ACCENT, 12.5, bold = TRUE)),
         fp_p = mkpar("left", 1.0, 5)),
    fpar(ftext("🔴 Certeza Factual:  98.1% → Acción inmediata.",
               mktxt("", COLOR_WHITE, 10.5)),
         fp_p = mkpar("left", 1.1, 3)),
    fpar(ftext("🟠 Certeza Alta:       86.4% → Visita preventiva.",
               mktxt("", COLOR_WHITE, 10.5)),
         fp_p = mkpar("left", 1.1, 3)),
    fpar(ftext("🟣 Predicción ML:   72.3% → Ruta de vigilancia.",
               mktxt("", COLOR_WHITE, 10.5)),
         fp_p = mkpar("left", 1.1, 3)),
    fpar(ftext("🔵 Observación:      44.7% → Monitoreo pasivo.",
               mktxt("", COLOR_WHITE, 10.5)),
         fp_p = mkpar("left", 1.1, 3)),
    fpar(ftext("🟢 Sin Historial:     11.9% → Línea base.",
               mktxt("", COLOR_WHITE, 10.5)),
         fp_p = mkpar("left", 1.1, 12)),
    fpar(ftext("\"Cuando SATICA emite CRÍTICO, la probabilidad de hallar el predio en flagrancia es casi total.\"",
               mktxt("", COLOR_ACCENT, 10, ital = TRUE)),
         fp_p = mkpar("left", 1.3))
  )
)

cat("  ✅ Slide 6 — Confianza\n")

# ─────────────────────────────────────────────────────────────────────────────
# SLIDE 7 — VALIDACIÓN CIEGA (Recall)
# ─────────────────────────────────────────────────────────────────────────────
prs <- add_slide(prs, layout = "Blank", master = "Office Theme")
prs <- bg_slide(prs)
prs <- add_title_block(prs,
  title    = "EL EXPERIMENTO: VALIDACIÓN CIEGA 2025",
  subtitle = "¿Cuántos de los incendios ocurridos en 2025 logró anticipar SATICA? — Cobertura acumulada por nivel."
)

prs <- ph_with(prs,
  location = ph_location(left = 0.3, top = 1.15, width = 7.5, height = 4.0),
  value = external_img(path_rec, width = 7.5, height = 4.0)
)

prs <- ph_with(prs,
  location = ph_location(left = 8.05, top = 1.15, width = 3.7, height = 5.35),
  value = block_list(
    fpar(ftext("Entrenamiento", mktxt("", COLOR_GREEN, 13, bold = TRUE)),
         fp_p = mkpar("left", 1.0, 5)),
    fpar(ftext("2019–2024. Años de datos reales de incendios en caña del Valle del Cauca.",
               mktxt("", COLOR_SUBTITLE, 10.5)),
         fp_p = mkpar("left", 1.3, 12)),
    fpar(ftext("Validación Ciega", mktxt("", COLOR_RED, 13, bold = TRUE)),
         fp_p = mkpar("left", 1.0, 5)),
    fpar(ftext("2025. El sistema predijo SIN conocer los resultados. Se comparó contra incendios reales reportados.",
               mktxt("", COLOR_SUBTITLE, 10.5)),
         fp_p = mkpar("left", 1.3, 12)),
    fpar(ftext("Resultado Clave", mktxt("", COLOR_ACCENT, 13, bold = TRUE)),
         fp_p = mkpar("left", 1.0, 5)),
    fpar(ftext("Los niveles CERTEZA FACTUAL + ALTA capturaron >78% de todos los incendios del año con alertas emitidas los días previos al evento.",
               mktxt("", COLOR_WHITE, 10.5)),
         fp_p = mkpar("left", 1.3, 12)),
    fpar(ftext("AUC del modelo: 0.89", mktxt("", COLOR_ACCENT, 13, bold = TRUE)),
         fp_p = mkpar("left", 1.0, 4)),
    fpar(ftext("El XGBoost discrimina incendios futuros con alta capacidad diagnóstica.",
               mktxt("", COLOR_SUBTITLE, 10.5)),
         fp_p = mkpar("left", 1.3))
  )
)

cat("  ✅ Slide 7 — Recall acumulado\n")

# ─────────────────────────────────────────────────────────────────────────────
# SLIDE 8 — MÓDULOS DEL DASHBOARD
# ─────────────────────────────────────────────────────────────────────────────
prs <- add_slide(prs, layout = "Blank", master = "Office Theme")
prs <- bg_slide(prs)
prs <- add_title_block(prs,
  title    = "EL DASHBOARD: 5 MÓDULOS OPERATIVOS",
  subtitle = "Una herramienta táctica y estratégica para técnicos y directivos de la CVC"
)

modulos <- list(
  list(n = "01", t = "Radar Preventivo",         c = COLOR_ACCENT,
       d = c("Mapa interactivo de todas las haciendas.", "Colores: CRÍTICO/ALTO/OBS/BAJO.",
             "Capas activables: GOES-16, HYSPLIT, Restricciones CVC.",
             "Pop-ups con historial y fecha estimada próximo incendio.")),
  list(n = "02", t = "Red de Riesgo",             c = COLOR_PURPLE,
       d = c("Grafo Hacienda ↔ Ingenio interactivo.", "Concentración de riesgo por empresa.",
             "Identifica ingenios con mayor exposición.", "Gestión de compromisos legales.")),
  list(n = "03", t = "Analítica Histórica",        c = COLOR_GREEN,
       d = c("Series de tiempo 2019–2026.", "Filtros: año, mes, municipio, ingenio.",
             "Ranking Top 10 predios más afectados.", "Descarga PNG y CSV auditado.")),
  list(n = "04", t = "Base de Datos",              c = COLOR_ORANGE,
       d = c("Tabla completa exportable a Excel.", "Fechas, coordenadas, ciclos, riesgo.",
             "Historial de visitas técnicas.", "Anclas Operativas GPS integradas.")),
  list(n = "05", t = "Sin Georreferenciación",     c = COLOR_RED,
       d = c("Gestión del 'Punto Ciego' del sistema.", "Haciendas sin polígono catastral.",
             "Ingreso coordinadas GPS manualmente.", "Integración al análisis de riesgo."))
)

cxs <- c(0.18, 2.55, 4.92, 7.29, 9.66)

for (idx in seq_along(modulos)) {
  m  <- modulos[[idx]]
  Lm <- cxs[idx]

  prs <- ph_with(prs,
    location = ph_location(left = Lm, top = 0.9, width = 2.3, height = 0.78),
    value = block_list(
      fpar(ftext(m$n, mktxt("", m$c, 26, bold = TRUE)), fp_p = mkpar("center", 0.9, 0)),
      fpar(ftext(m$t, mktxt("", COLOR_WHITE, 10, bold = TRUE)), fp_p = mkpar("center", 1.0))
    )
  )

  dbd <- lapply(m$d, function(dd) {
    fpar(ftext(paste0("• ", dd), mktxt("", COLOR_SUBTITLE, 9)),
         fp_p = mkpar("left", 1.2, 4))
  })
  prs <- ph_with(prs,
    location = ph_location(left = Lm, top = 1.78, width = 2.3, height = 4.8),
    value = do.call(block_list, dbd)
  )
}

cat("  ✅ Slide 8 — Dashboard\n")

# ─────────────────────────────────────────────────────────────────────────────
# SLIDE 9 — AUTOMATIZACIÓN / CENTINELA
# ─────────────────────────────────────────────────────────────────────────────
prs <- add_slide(prs, layout = "Blank", master = "Office Theme")
prs <- bg_slide(prs)
prs <- add_title_block(prs,
  title    = "AUTOMATIZACIÓN TOTAL: EL CENTINELA",
  subtitle = "SATICA opera 24/7 sin que nadie esté frente a la pantalla — sistema serverless en la nube"
)

prs <- ph_with(prs,
  location = ph_location(left = 0.4, top = 1.15, width = 5.6, height = 5.4),
  value = block_list(
    fpar(ftext("Robot en la Nube (GitHub Actions)", mktxt("", COLOR_GREEN, 13, bold = TRUE)),
         fp_p = mkpar("left", 1.0, 6)),
    fpar(ftext("El archivo centinela.yml dispara un robot cada 15 minutos en servidores Ubuntu (GitHub):",
               mktxt("", COLOR_SUBTITLE, 11)),
         fp_p = mkpar("left", 1.3, 10)),
    fpar(ftext("•  Consulta NASA FIRMS (VIIRS + MODIS tiempo real).",
               mktxt("", COLOR_WHITE, 11)),
         fp_p = mkpar("left", 1.1, 4)),
    fpar(ftext("•  Cruza focos contra mapa catastral de suertes.",
               mktxt("", COLOR_WHITE, 11)),
         fp_p = mkpar("left", 1.1, 4)),
    fpar(ftext("•  Calcula nivel de riesgo con modelo IA V9.",
               mktxt("", COLOR_WHITE, 11)),
         fp_p = mkpar("left", 1.1, 4)),
    fpar(ftext("•  Envía Ficha de Acción Rápida a Telegram si hay confirmación.",
               mktxt("", COLOR_WHITE, 11)),
         fp_p = mkpar("left", 1.1, 10)),
    fpar(ftext("Las llaves API y tokens están cifrados como GitHub Secrets.",
               mktxt("", "#64748b", 10, ital = TRUE)),
         fp_p = mkpar("left", 1.0))
  )
)

prs <- ph_with(prs,
  location = ph_location(left = 6.25, top = 1.15, width = 5.55, height = 5.4),
  value = block_list(
    fpar(ftext("Ficha de Acción Rápida (Telegram)", mktxt("", COLOR_ORANGE, 13, bold = TRUE)),
         fp_p = mkpar("left", 1.0, 8)),
    fpar(ftext("🔥 ALERTA SATICA V2.0 · Certeza Factual 98.1%",
               mktxt("", COLOR_RED, 11, bold = TRUE)),
         fp_p = mkpar("left", 1.0, 4)),
    fpar(ftext("Hacienda:     EL PARAISO\nSuerte:         000123\nCoordenadas: 3.8841, -76.4712\nEstado:         Evento Térmico < 15 min\nRiesgo:         CRÍTICO 🔴",
               mktxt("", COLOR_SUBTITLE, 10.5)),
         fp_p = mkpar("left", 1.4, 10)),
    fpar(ftext("Smart Sync en el Dashboard", mktxt("", COLOR_ACCENT, 12, bold = TRUE)),
         fp_p = mkpar("left", 1.0, 5)),
    fpar(ftext("Al abrir el Dashboard, verifica si los datos tienen > 30 min. Si están viejos, actualiza automáticamente desde NASA antes de mostrar el mapa.",
               mktxt("", COLOR_SUBTITLE, 10.5)),
         fp_p = mkpar("left", 1.3, 10)),
    fpar(ftext("Deep Link: cada alerta abre el mapa directamente centrado en el predio afectado.",
               mktxt("", COLOR_GREEN, 10.5)),
         fp_p = mkpar("left", 1.3))
  )
)

cat("  ✅ Slide 9 — Automatización\n")

# ─────────────────────────────────────────────────────────────────────────────
# SLIDE 10 — BLINDAJE JURÍDICO
# ─────────────────────────────────────────────────────────────────────────────
prs <- add_slide(prs, layout = "Blank", master = "Office Theme")
prs <- bg_slide(prs)
prs <- add_title_block(prs,
  title    = "MÁS QUE UN MAPA: BLINDAJE JURÍDICO",
  subtitle = "SATICA no solo alerta — documenta, justifica y da respaldo legal a cada acción de la CVC"
)

pilares <- list(
  list(t = "Resolución 0741 de 2016",  c = COLOR_RED,
       d = "SATICA da sustento técnico concreto a las visitas preventivas que exige esta norma. Cada alerta incluye coordenadas, fecha y nivel de confianza certificado científicamente."),
  list(t = "Actas de Inspección",       c = COLOR_ORANGE,
       d = "El boletín ejecutivo descargable (PDF/DOCX) por cada hacienda describe su historial, índice de vulnerabilidad y justifica técnico-científicamente la necesidad de la visita de inspección."),
  list(t = "Métrica de Éxito Ambiental", c = COLOR_GREEN,
       d = "Si una hacienda en nivel CRÍTICO recibe visita preventiva y supera su ciclo predicho sin arder, SATICA lo registra como 'Incendio Evitado': indicador de gestión ambiental exitosa de la CVC."),
  list(t = "Exportación GIS Profesional", c = COLOR_ACCENT,
       d = "Las capas KML exportadas llevan estilos de riesgo embebidos (rojo=Crítico, naranja=Alto). Se cargan directamente en QGIS o ArcGIS Pro sin pasos de configuración adicionales.")
)

for (pi in seq_along(pilares)) {
  pl   <- pilares[[pi]]
  col_x <- if (pi <= 2) 0.4 else 6.4
  row_y <- if (pi %% 2 == 1) 1.15 else 3.9

  prs <- ph_with(prs,
    location = ph_location(left = col_x, top = row_y, width = 5.7, height = 2.5),
    value = block_list(
      fpar(ftext(pl$t, mktxt("", pl$c, 13.5, bold = TRUE)), fp_p = mkpar("left", 1.0, 6)),
      fpar(ftext(pl$d, mktxt("", COLOR_SUBTITLE, 11)),       fp_p = mkpar("left", 1.3))
    )
  )
}

cat("  ✅ Slide 10 — Blindaje Jurídico\n")

# ─────────────────────────────────────────────────────────────────────────────
# SLIDE 11 — ROADMAP / SIGUIENTES PASOS
# ─────────────────────────────────────────────────────────────────────────────
prs <- add_slide(prs, layout = "Blank", master = "Office Theme")
prs <- bg_slide(prs)
prs <- add_title_block(prs,
  title    = "SIGUIENTES PASOS: SATICA V3.0+",
  subtitle = "Los cimientos están listos. Estas expansiones multiplicarán el impacto del sistema."
)

prs <- ph_with(prs,
  location = ph_location(left = 0.3, top = 1.1, width = 11.4, height = 2.0),
  value = external_img(path_road, width = 11.4, height = 2.0)
)

sdets <- list(
  list(n = "01", t = "Georref. Total",    c = COLOR_GREEN,  e = "EN CURSO",
       d = "Completar polígonos de haciendas sin shapefile. Módulo 'Punto Ciego' activo para GPS manual. Meta: 100% cobertura espacial."),
  list(n = "02", t = "Portal Web CVC",    c = COLOR_GREEN,  e = "EN CURSO",
       d = "Despliegue en servidor dedicado con acceso autenticado para otros DAR y nivel central CVC."),
  list(n = "03", t = "App Móvil Campo",   c = COLOR_ACCENT, e = "PLANIFICADO",
       d = "Versión PWA para técnicos en terreno: recibir alertas, registrar visitas y capturar evidencia fotográfica."),
  list(n = "04", t = "Integración CDIAC", c = COLOR_ACCENT, e = "PLANIFICADO",
       d = "Conectar con datos climáticos IDEAM: sequía, ETP, déficit hídrico como features adicionales del modelo IA."),
  list(n = "05", t = "Régimen Quemas",    c = COLOR_PURPLE, e = "PLANIFICADO",
       d = "Módulo para solicitudes de quemas controladas: evalúa automáticamente restricciones CVC y condición del viento."),
  list(n = "06", t = "IA V10 Autolearn",  c = COLOR_PURPLE, e = "FUTURO",
       d = "Reentrenamiento automático del XGBoost cada trimestre con nuevos incendios confirmados. El sistema mejora solo.")
)

cxsd <- c(0.18, 2.2, 4.22, 6.45, 8.48, 10.5)

for (sd in seq_along(sdets)) {
  s2  <- sdets[[sd]]
  Ls  <- cxsd[sd]
  prs <- ph_with(prs,
    location = ph_location(left = Ls, top = 3.2, width = 2.05, height = 3.3),
    value = block_list(
      fpar(ftext(paste0(s2$n, " ", s2$t), mktxt("", s2$c, 9, bold = TRUE)),
           fp_p = mkpar("left", 1.0, 4)),
      fpar(ftext(paste0("[", s2$e, "]"), mktxt("", "#475569", 8, ital = TRUE)),
           fp_p = mkpar("left", 1.0, 5)),
      fpar(ftext(s2$d, mktxt("", COLOR_SUBTITLE, 8.5)),
           fp_p = mkpar("left", 1.2))
    )
  )
}

cat("  ✅ Slide 11 — Roadmap\n")

# ─────────────────────────────────────────────────────────────────────────────
# SLIDE 12 — CIERRE
# ─────────────────────────────────────────────────────────────────────────────
prs <- add_slide(prs, layout = "Blank", master = "Office Theme")
prs <- bg_slide(prs)

# Franjas
for (yf in c(0, 6.4)) {
  prs <- ph_with(prs,
    location = ph_location(left = 0, top = yf, width = W, height = 0.32),
    value = block_list(fpar(ftext(strrep("█", 120), mktxt("", COLOR_ACCENT, 10)),
                            fp_p = mkpar("left")))
  )
}

prs <- ph_with(prs,
  location = ph_location(left = 0.8, top = 1.1, width = 10.4, height = 1.6),
  value = block_list(
    fpar(ftext("SATICA", mktxt("", COLOR_WHITE, 72, bold = TRUE)), fp_p = mkpar("center", 0.9, 0)),
    fpar(ftext("protege el Valle del Cauca", mktxt("", COLOR_ACCENT, 24)), fp_p = mkpar("center", 1.0))
  )
)

prs <- ph_with(prs,
  location = ph_location(left = 1.5, top = 3.05, width = 9, height = 0.85),
  value = block_list(fpar(
    ftext("\"Con la tecnología correcta, un técnico de la CVC tiene hoy más poder de detección que una brigada completa de hace 10 años.\"",
          mktxt("", COLOR_SUBTITLE, 12.5, ital = TRUE)),
    fp_p = mkpar("center", 1.4)
  ))
)

prs <- ph_with(prs,
  location = ph_location(left = 1.5, top = 4.15, width = 9, height = 1.85),
  value = block_list(
    fpar(ftext("5 satélites  ·  98.1% de certeza  ·  15 min de respuesta",
               mktxt("", COLOR_WHITE, 14, bold = TRUE)),
         fp_p = mkpar("center", 1.0, 7)),
    fpar(ftext("Sistema de Alertas Tempranas de Incendios en Caña de Azúcar",
               mktxt("", COLOR_SUBTITLE, 11)),
         fp_p = mkpar("center", 1.0, 5)),
    fpar(ftext("CVC — DAR Suroriente  ·  Abril 2026",
               mktxt("", "#475569", 10)),
         fp_p = mkpar("center", 1.0))
  )
)

cat("  ✅ Slide 12 — Cierre\n")

# ==============================================================================
# 5. GUARDAR
# ==============================================================================
ruta_salida <- "PRESENTACION_SATICA_V2.pptx"
print(prs, target = ruta_salida)

cat("\n")
cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║  ✅  PRESENTACIÓN GENERADA EXITOSAMENTE                     ║\n")
cat(sprintf("║  📄  Archivo: %-46s ║\n", ruta_salida))
cat("║  📊  Diapositivas: 12                                        ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n")
cat("\nAbra el archivo con PowerPoint o LibreOffice Impress.\n")
