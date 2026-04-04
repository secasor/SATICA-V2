# ==============================================================================
# ARCHIVO: ui.R | SATICA V2.0 — Migrado a bs4Dash
# ==============================================================================
library(shiny)
library(bs4Dash)
library(leaflet)
library(DT)
library(shinyWidgets)
library(visNetwork)

# --- CSS personalizado para replicar el diseño de referencia ---
css_custom <- "
  /* Sidebar fondo oscuro */
  .main-sidebar, .sidebar { background-color: #1a2d45 !important; }
  .sidebar-dark-primary { background-color: #1a2d45 !important; }
  .nav-sidebar .nav-item .nav-link { color: #c8d8e8 !important; }
  .nav-sidebar .nav-item .nav-link.active, .nav-sidebar .nav-item .nav-link:hover {
    background-color: #243d5a !important; color: #ffffff !important;
  }

  /* Header */
  .main-header .navbar { background-color: #1a2d45 !important; border: none; }
  .main-header .brand-link { background-color: #1a2d45 !important; border: none; }
  .brand-link .brand-text { color: #ffffff !important; font-size: 14px !important; }

  /* User panel */
  .user-panel { border-bottom: 1px solid #2e4a6a; padding: 10px 15px; }
  .user-panel .info { color: #c8d8e8; }
  .user-panel .info a { color: #ffffff; font-weight: 700; }

  /* Botón de haciendas buscador */
  #buscar_hda { background: rgba(255,255,255,.1) !important; color: white !important;
                border: 1px solid rgba(255,255,255,.2) !important; border-radius: 4px;
                font-size: 12px; }
  #buscar_hda::placeholder { color: rgba(255,255,255,.4) !important; }

  /* Botones de acción en sidebar */
  .sidebar-btns .btn { width: 100%; font-weight: bold; margin-bottom: 10px; }

  /* Bullets de color en el menú */
  .menu-icon-dot { display: inline-block; width: 14px; height: 14px;
                   border-radius: 3px; margin-right: 8px; vertical-align: middle; }

  /* Value boxes */
  .small-box { cursor: pointer !important; }

  /* Forzar leaflet al 100% de la caja */
  .leaflet { z-index: 0 !important; }

    /* Resultados del buscador */
  .buscar-resultado-item { padding: 5px 8px; border-radius: 4px; margin-bottom: 3px;
                            background: rgba(255,255,255,.08); cursor: pointer; }

  /* ── Botón Vista General: contener dentro del sidebar ── */
  #reset { width: 100% !important; box-sizing: border-box !important;
           display: block !important; margin: 0 !important; }

      /* ── Value boxes: números grandes (AdminLTE3 bs4Dash) ── */
  .small-box .inner h3,
  .small-box h3 {
    font-size: 4rem !important;
    font-weight: 900 !important;
    line-height: 1.1 !important;
    margin-bottom: 0 !important;
  }
  .small-box .inner p,
  .small-box p {
    font-size: 1.05rem !important;
    font-weight: 600 !important;
  }
  .small-box { min-height: 100px !important; }




    /* ── pickerInput en sidebar: fondo oscuro + texto blanco ── */
  .main-sidebar .bootstrap-select > .dropdown-toggle.btn-light,
  .main-sidebar .bootstrap-select > .dropdown-toggle,
  .main-sidebar .bootstrap-select .btn,
  .main-sidebar .selectpicker,
  .main-sidebar .SumoSelect > .CaptionCont {
    background-color: rgba(255,255,255,.10) !important;
    color: #ffffff !important;
    border: 1px solid rgba(255,255,255,.3) !important;
    border-radius: 4px !important;
  }
  .main-sidebar .filter-option-inner-inner,
  .main-sidebar .filter-option {
    color: #ffffff !important;
  }
  .main-sidebar .bootstrap-select .dropdown-menu.inner > li > a,
  .main-sidebar .bootstrap-select .dropdown-menu.inner > li > a span {
    color: #c8d8e8 !important;
  }
  .main-sidebar .bootstrap-select .dropdown-menu {
    background-color: #1a2d45 !important;
    border: 1px solid rgba(255,255,255,.15) !important;
  }
  .main-sidebar .bootstrap-select .dropdown-item:hover,
  .main-sidebar .bootstrap-select .dropdown-menu.inner > li.selected > a {
    background-color: #243d5a !important;
    color: #fff !important;
  }
  .main-sidebar .form-control,
  .main-sidebar input[type=text] {
    background: rgba(255,255,255,.10) !important;
    color: #fff !important;
    border: 1px solid rgba(255,255,255,.25) !important;
  }
  .main-sidebar label { color: #b8c7ce !important; font-size: 12px; }

"

# Icono personalizado con cuadrado de color
menu_icon <- function(color) {
  tags$span(class = "menu-icon-dot", style = paste0("background-color:", color, ";"))
}

ui <- dashboardPage(
  dark = NULL,
  scrollToTop = TRUE,
  title = "SATICA — Sistema de Alertas Tempranas",

  # ─── HEADER ───────────────────────────────────────────────────────────────
  dashboardHeader(
    title = dashboardBrand(
      title = "SATICA — Sistema de Alertas Tempranas",
      color = "primary"
    ),
    # Badges en el header usando la estructura correcta de bs4Dash
    rightUi = tagList(
      bs4DropdownMenu(
        type = "notifications",
        badgeStatus = "info",
        icon = icon("bell"),
        headerText = "DAR Suroriente"
      ),
      tags$li(
        class = "nav-item dropdown",
        tags$a(
          class = "nav-link dropdown-toggle",
          href = "#",
          `data-toggle` = "dropdown",
          tags$div(
            style = "display:inline-block; background:#0d1f5c; border-radius:8px;
                     padding:3px 10px 5px; border-bottom:4px solid #2d7a2d;
                     color:#fff; font-weight:900; font-style:italic;
                     font-size:18px; font-family:Arial,sans-serif; line-height:1.2;",
            "CVC"
          )
        ),
        tags$div(
          class = "dropdown-menu dropdown-menu-right",
          tags$span(
            class = "dropdown-item-text",
            "CVC — Corporación Autónoma Regional del Valle del Cauca"
          )
        )
      )
    )
  ),

  # ─── SIDEBAR ──────────────────────────────────────────────────────────────
  dashboardSidebar(
    skin = "dark",
    status = "primary",

    # Panel de usuario (div manual: bs4Dash sidebarUserPanel no acepta text=)
    div(
      class = "user-panel d-flex pb-3 mb-2",
      style = "border-bottom: 1px solid #2e4a6a; padding: 12px 15px;",
      div(
        class = "image",
        tags$img(
          src   = "https://ui-avatars.com/api/?name=SOR&background=1abc9c&color=fff&rounded=true&size=40",
          style = "width:40px; height:40px; border-radius:50%; margin-right:10px;"
        )
      ),
      div(
        class = "info",
        tags$a(
          href  = "#",
          style = "color:#ffffff; font-weight:700; font-size:14px; display:block;",
          "DAR Suroriente"
        ),
        tags$span(
          style = "color:#b8c7ce; font-size:12px;",
          "CVC \u2014 Incendios 2026"
        )
      )
    ),
    hr(style = "border-color: #2e4a6a; margin: 5px 0;"),

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
      style = "padding: 0 12px 12px; box-sizing: border-box; width: 100%;",
      actionButton("reset", "Vista General",
        icon = icon("globe-americas"),
        style = "background-color:#17a2b8; color:#fff; border:none;
                 width:100%; font-weight:bold; display:block; box-sizing:border-box;"
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
            status = "warning", solidHeader = FALSE, collapsible = TRUE,
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
