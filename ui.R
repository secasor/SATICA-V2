# ==============================================================================
# ARCHIVO: ui.R | SATICA V2.0 — Migrado a bs4Dash
# ==============================================================================
library(shiny)
library(bs4Dash)
library(leaflet)
library(DT)
library(shinyWidgets)
library(visNetwork)

# --- CSS personalizado con Estética Stitch/Google (Glassmorphism) ---
css_custom <- "
  @import url('https://fonts.googleapis.com/css2?family=Outfit:wght@300;400;600;900&display=swap');

  :root {
    --bg-dark: #0f172a;
    --sidebar-bg: rgba(15, 23, 42, 0.85);
    --accent-blue: #38bdf8;
    --accent-green: #22c55e;
    --accent-orange: #f59e0b;
    --accent-red: #ef4444;
    --glass-border: rgba(255, 255, 255, 0.1);
    --glass-shadow: 0 4px 30px rgba(0, 0, 0, 0.1);
  }

  /* Background principal */
  .content-wrapper, .wrapper { background-color: var(--bg-dark) !important; }
  body { font-family: 'Outfit', sans-serif !important; background-color: var(--bg-dark) !important; color: #f8fafc; }

  /* Sidebar Glassmorphism */
  .main-sidebar, .sidebar { 
    background: var(--sidebar-bg) !important; 
    backdrop-filter: blur(16px) !important;
    border-right: 1px solid var(--glass-border) !important;
  }
  .sidebar-dark-primary { background-color: transparent !important; }

  /* Header Premium */
  .main-header .navbar { 
    background: rgba(15, 23, 42, 0.8) !important; 
    backdrop-filter: blur(12px) !important;
    border-bottom: 1px solid var(--glass-border) !important; 
  }
  
  /* Brand Link */
  .brand-link { background: transparent !important; border: none !important; }
  .brand-link .brand-text { font-weight: 600 !important; color: #fff !important; }

  /* Nav Links */
  .nav-sidebar .nav-item .nav-link { 
    color: #94a3b8 !important; 
    border-radius: 12px !important;
    margin: 4px 10px !important;
    transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
  }
  .nav-sidebar .nav-item .nav-link:hover, .nav-sidebar .nav-item .nav-link.active {
    background: rgba(56, 189, 248, 0.15) !important;
    color: var(--accent-blue) !important;
    box-shadow: 0 4px 12px rgba(0,0,0,0.2);
  }

  /* Value Box Estilo 'Elevated Card' */
  .small-box { 
    border: 1px solid var(--glass-border) !important;
    border-radius: 16px !important;
    box-shadow: var(--glass-shadow) !important;
    overflow: hidden;
    transition: transform 0.3s ease;
    /* Soften the bg color natively provided by bs4Dash */
    background-image: linear-gradient(135deg, rgba(255,255,255,0.1) 0%, rgba(0,0,0,0.1) 100%);
    backdrop-filter: blur(8px);
  }
  .small-box:hover { transform: translateY(-5px); }
  .small-box .inner h3 { 
    font-size: 2.5rem !important; 
    font-weight: 900 !important; 
    letter-spacing: -1px;
    margin-bottom: 5px !important;
    color: #ffffff !important;
  }
  .small-box .inner p { color: #f1f5f9 !important; font-weight: 400 !important; }

  /* Inputs Premium */
  .main-sidebar .bootstrap-select > .dropdown-toggle,
  .main-sidebar .form-control {
    background: rgba(255, 255, 255, 0.05) !important;
    border: 1px solid var(--glass-border) !important;
    border-radius: 10px !important;
    color: #fff !important;
  }

  /* Botones Glass */
  .sidebar-btns .btn {
    border-radius: 12px !important;
    border: 1px solid var(--glass-border) !important;
    padding: 10px !important;
    font-size: 13px !important;
    margin-bottom: 8px !important;
    transition: all 0.2s ease;
    width: 100% !important;
    display: block !important;
    text-align: left !important;
  }
  .sidebar-btns .btn:hover { filter: brightness(1.2); transform: scale(1.02); }

  /* Leaflet full vision y Controles */
  #mapa { border-radius: 20px !important; overflow: hidden; box-shadow: 0 20px 50px rgba(0,0,0,0.5); }
  .leaflet-top { margin-top: 50px !important; }
  .leaflet-left { margin-left: 10px !important; }

  /* DataTables Dark Mode fix */
  table.dataTable, .dataTables_wrapper { color: #f8fafc !important; }
  .dataTable thead th, .dataTable thead td { border-bottom: 1px solid rgba(255,255,255,0.1) !important; color: #cbd5e1 !important; }
  .dataTable tbody tr { background-color: transparent !important; }
  .dataTable tbody td { border-top: 1px solid rgba(255,255,255,0.05) !important; }
  .dataTable tbody tr:hover { background-color: rgba(255,255,255,0.05) !important; color: #fff !important;}
  .dataTables_info, .dataTables_length, .dataTables_filter, .dataTables_paginate { color: #cbd5e1 !important; }


  /* Bullets de color en el menú */
  .menu-icon-dot { 
    display: inline-block; width: 14px; height: 14px;
    border-radius: 4px; margin-right: 8px; vertical-align: middle; 
  }
"

# Icono personalizado con cuadrado de color
menu_icon <- function(color) {
  tags$span(class = "menu-icon-dot", style = paste0("background-color:", color, ";"))
}

ui <- dashboardPage(
  dark = TRUE,
  scrollToTop = FALSE,
  title = "SATICA — Sistema de Alertas Tempranas",

  # ─── HEADER ───────────────────────────────────────────────────────────────
  dashboardHeader(
    title = dashboardBrand(
      title = "SATICA V2.0",
      color = "primary",
      opacity = 0.8
    ),
    rightUi = tagList(
      bs4DropdownMenu(
        type = "notifications",
        badgeStatus = "danger",
        icon = icon("fire-alt"),
        headerText = "Alertas Recientes"
      )
    )
  ),

  # ─── SIDEBAR ──────────────────────────────────────────────────────────────
  dashboardSidebar(
    skin = "dark",
    status = "primary",
    elevation = 0,

    # Sidebar User Panel simplificado
    div(
      style = "padding: 20px 15px; text-align: center;",
      tags$img(
        src   = "https://ui-avatars.com/api/?name=CVC&background=38bdf8&color=fff&rounded=true&size=64",
        style = "width:64px; height:64px; border-radius:16px; margin-bottom:10px; box-shadow: 0 10px 20px rgba(0,0,0,0.3);"
      ),
      tags$h5("SATICA", style = "color:#fff; font-weight:900; margin-bottom:0; letter-spacing:2px;"),
      tags$small("DAR SURORIENTE", style = "color:#64748b; font-weight:600;")
    ),

    # Menú principal con bullets de color
    sidebarMenu(
      id = "tabs",
      menuItem(
        tagList(menu_icon("#3498db"), "Radar Preventivo"),
        tabName = "dash", icon = NULL
      ),
      menuItem(
        tagList(menu_icon("#9b59b6"), "Red de Riesgo"),
        tabName = "grafo", icon = NULL
      ),
      menuItem(
        tagList(menu_icon("#27ae60"), "Analítica Histórica"),
        tabName = "historia", icon = NULL
      ),
      menuItem(
        tagList(menu_icon("#e67e22"), "Base de Datos"),
        tabName = "repo", icon = NULL
      ),
      menuItem(
        tagList(menu_icon("#e74c3c"), "Sin Georreferenciación"),
        tabName = "sin_georref", icon = NULL
      )
    ),
    hr(style = "border-color: #2e4a6a; margin: 5px 0;"),

    # ── Botones de acción ──
    div(
      class = "sidebar-btns", style = "padding: 0 12px;",
      downloadButton("descargar_plantilla",
        tagList(icon("file-alt"), " Plantilla Visitas"),
        style = "background-color:#e67e22; color:#fff; border:none;"
      ),
      downloadButton("descargar_reporte",
        tagList(icon("file-pdf"), " Generar PDF"),
        style = "background-color:#e74c3c; color:#fff; border:none;"
      ),
      downloadButton("descargar_excel",
        tagList(icon("file-excel"), " Excel Seguimiento"),
        style = "background-color:#27ae60; color:#fff; border:none;"
      ),
      downloadButton("descargar_shape",
        tagList(icon("map"), " Capas GIS"),
        style = "background-color:#2980b9; color:#fff; border:none;"
      )
    ),
    hr(style = "border-color: #2e4a6a; margin: 5px 0;"),

    # ── Filtros ──
    div(
      style = "padding: 0 12px;",
      pickerInput("f_ing", "Ingenio:",
        choices = sort(unique(DATOS_OFICIALES$INGENIO_FULL)),
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = "count > 99", # nunca habrá 100, siempre mostrará el texto
          `count-selected-text` = "Todos",
          `none-selected-text` = "Ninguno"
        ),
        selected = unique(DATOS_OFICIALES$INGENIO_FULL)
      ),
      pickerInput("f_mun", "Municipio:",
        choices = c("TODOS", sort(unique(DATOS_OFICIALES$MUNICIPIO))),
        multiple = FALSE,
        options = list(`live-search` = TRUE),
        selected = "TODOS"
      )
    ),
    hr(style = "border-color: #2e4a6a; margin: 5px 0;"),

    # ── Buscador de haciendas ──
    div(
      style = "padding: 0 12px 6px;",
      tags$label("Hacienda:",
        style = "font-size: 13px; color: #b8c7ce; margin-bottom: 4px;
                  display: block; font-weight: 400; padding-left: 0;"
      ),
      textInput("buscar_hda",
        label = NULL,
        placeholder = "Nombre o código...",
        width = "100%"
      ),
      uiOutput("ui_buscar_resultados")
    ),
    div(
      style = "padding: 0 12px 0 12px; width: 100%; box-sizing: border-box; margin-bottom: 10px;",
      actionButton("btn_actualizar", "Actualizar Telemetría",
        icon = icon("sync"),
        style = "background-color:#c0392b; color:#fff; border:none; width:100%; font-weight:bold; display:block; margin:0 auto; border-radius:12px; padding:10px;"
      )
    ),
    div(
      style = "padding: 0 12px 15px 12px; width: 100%; box-sizing: border-box;",
      actionButton("reset", "Vista General",
        icon = icon("globe-americas"),
        style = "background-color:#17a2b8; color:#fff; border:none; width:100%; font-weight:bold; display:block; margin:0 auto; border-radius:12px; padding:10px;"
      )
    )
  ),

  # ─── BODY ─────────────────────────────────────────────────────────────────
  dashboardBody(
    tags$head(tags$style(HTML(css_custom))),
    tags$script(HTML("
      $(document).on('shiny:value', function() {
        setTimeout(function(){
          $('.small-box .inner h3').css({
            'font-size': '2.2rem',
            'font-weight': '900',
            'line-height': '1'
          });
          $('.small-box .inner p b').css({
            'font-size': '1rem',
            'font-weight': '700'
          });
        }, 100);
      });
    ")),
    tabItems(
      # ── RADAR PREVENTIVO (BOLETÍN ESTRATÉGICO) ──────────────────────────
      tabItem(
        tabName = "dash",
        fluidRow(
          valueBoxOutput("box_red", width = 3),
          valueBoxOutput("box_orange", width = 3),
          valueBoxOutput("box_yellow", width = 3),
          valueBoxOutput("box_green", width = 3)
        ),
        fluidRow(
          valueBoxOutput("box_visitas_control", width = 4),
          valueBoxOutput("box_visitas_exito", width = 4),
          valueBoxOutput("box_firms_24h", width = 2),
          valueBoxOutput("box_goes_alert", width = 2)
        ),
        fluidRow(
          box(
            width = 12, title = tagList(icon("brain"), "Radar de Planificación y Riesgo Histórico"),
            status = "primary", borderStatus = "primary", solidHeader = FALSE, 
            footer = "Haciendas coloreadas por criticidad de su ciclo histórico. Use el control de capas para activar: Simulación HYSPLIT y Flagrancia Satelital (FIRMS/GOES-16).",
            leafletOutput("mapa", height = 650)
          )
        ),
        fluidRow(
          box(
            width = 12, title = "Haciendas Prioritarias (Ventana Crítica/Alta)",
            status = "warning", solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE,
            DTOutput("tabla_top")
          )
        )
      ),

      # ── ANALÍTICA HISTÓRICA ─────────────────────────────────────────────
      tabItem(
        tabName = "historia",
        h2("Auditoría Histórica"),
        fluidRow(valueBoxOutput("box_total_dar", width = 12)),
        fluidRow(
          box(
            width = 4, title = "Filtros y Controles",
            status = "primary", solidHeader = TRUE,
            pickerInput("h_anio", "Años:",
              choices = 2019:as.numeric(format(Sys.Date(), "%Y")),
              selected = 2019:as.numeric(format(Sys.Date(), "%Y")),
              multiple = TRUE,
              options = list(
                `actions-box` = TRUE,
                `selected-text-format` = "count > 7",
                `count-selected-text` = "Todos"
              )
            ),
            pickerInput("h_mes_num", "Meses:",
              choices = setNames(1:12, c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")),
              selected = 1:12, multiple = TRUE,
              options = list(
                `actions-box` = TRUE,
                `selected-text-format` = "count > 11",
                `count-selected-text` = "Todos"
              )
            ),
            hr(),
            pickerInput("h_ing", "Ingenio:",
              choices = c("TODOS", sort(unique(DATOS_OFICIALES$INGENIO_FULL))),
              selected = "TODOS"
            ),
            uiOutput("ui_h_mun"),
            hr(),
            radioButtons("h_agrupa", "Agrupar gráfica principal por:",
              choices = c("Año" = "ANIO", "Mes" = "MES", "Semana" = "SEMANA"),
              inline = TRUE
            ),
            hr(),
            div(
              style = "text-align: center;",
              downloadButton("descargar_historia_csv",
                "Descargar Datos Auditados (.csv)",
                style = "background-color:#27ae60; color:#fff; border:none; width:100%; font-weight:bold;"
              )
            )
          ),
          box(
            width = 8, title = "Tendencia de Incendios Detectados",
            status = "info", solidHeader = TRUE,
            plotOutput("plot_historia", height = "400px"),
            div(
              style = "text-align: right; margin-top: 15px;",
              downloadButton("dl_plot_historia", "Descargar Gráfico PNG",
                icon = icon("camera"),
                style = "font-size:12px;"
              )
            )
          )
        ),
        fluidRow(
          box(
            width = 12, title = "Ranking Histórico: Top 10 Predios Afectados",
            status = "warning", solidHeader = TRUE,
            plotOutput("plot_top10", height = "400px"),
            div(
              style = "text-align: right; margin-top: 15px;",
              downloadButton("dl_plot_top10", "Descargar Gráfico PNG",
                icon = icon("camera"),
                style = "font-size:12px;"
              )
            )
          )
        )
      ),

      # ── BASE DE DATOS ───────────────────────────────────────────────────
      tabItem(
        tabName = "repo",
        h3("Base de Datos Consolidada - DAR Suroriente"),
        DTOutput("tabla_completa")
      ),

      # ── RED DE RIESGO ───────────────────────────────────────────────────
      tabItem(
        tabName = "grafo",
        h3("Análisis de Redes: Conectividad Hacienda - Ingenio"),
        box(
          width = 12, status = "primary", solidHeader = TRUE,
          title = "Grafo Interactivo",
          visNetwork::visNetworkOutput("grafo_riesgo", height = "700px")
        )
      ),

      # ── SIN GEORREFERENCIACIÓN ──────────────────────────────────────────
      tabItem(
        tabName = "sin_georref",
        h3("Haciendas Sin Georreferenciación"),
        fluidRow(
          box(
            width = 12, status = "danger", solidHeader = TRUE,
            title = tagList(icon("exclamation-triangle"), "Punto Ciego del Sistema"),
            DTOutput("tabla_sin_georref")
          )
        )
      )
    )
  )
)
