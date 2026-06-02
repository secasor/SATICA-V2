# ==============================================================================
# ARCHIVO: server.R | VERSIÓN: SATICA V1.30084.113_TIEMPO_DINAMICO
# ==============================================================================
# --- BACKUP POINT: server.R (Purga de Excel, Anti-Trituración y Matriz 7 Niveles) ---
# ==============================================================================

library(ggplot2)
library(lubridate)
library(janitor)
library(visNetwork)
library(dplyr)
library(sf)
# [BLINDAJE 2: Eliminda la dependencia de readxl en vivo]

server <- function(input, output, session) {
  
  update_trigger <- reactiveVal(0)

  # --- BUSCADOR DE HACIENDAS (bs4Dash fix: usa Shiny.setInputValue en lugar de actionLink) ---
  output$ui_buscar_resultados <- renderUI({
    query <- trimws(input$buscar_hda)
    if (is.null(query) || nchar(query) < 2) return(NULL)

    resultados <- DATOS_OFICIALES %>%
      st_drop_geometry() %>%
      filter(
        grepl(toupper(query), toupper(HDA_LABEL), fixed = TRUE) |
          grepl(toupper(query), toupper(COD_HDA_KEY), fixed = TRUE)
      ) %>%
      arrange(HDA_LABEL) %>%
      slice_head(n = 8) %>%
      select(COD_HDA_KEY, HDA_LABEL, MUNICIPIO, RIESGO, COL)

    if (nrow(resultados) == 0) {
      return(div(style = "font-size:11px; color:rgba(255,255,255,.4); padding: 4px 0;",
                 "Sin resultados"))
    }

    # Guardar resultados para el observer
    session$userData$buscar_resultados <- resultados

    # Construir botones usando tags$button + Shiny.setInputValue (compatible con bs4Dash)
    items <- lapply(seq_len(nrow(resultados)), function(i) {
      r <- resultados[i, ]
      tags$button(
        type = "button",
        style = paste0(
          "display:block; width:100%; text-align:left; background:rgba(255,255,255,.08); ",
          "border:none; border-left:3px solid ", r$COL, "; border-radius:4px; ",
          "padding:5px 8px; margin-bottom:3px; cursor:pointer; color:white;"
        ),
        onclick = paste0(
          "Shiny.setInputValue('buscar_sel', '", r$COD_HDA_KEY, "', {priority: 'event'});"
        ),
        tags$div(
          style = "font-size:11px; font-weight:600; white-space:nowrap; overflow:hidden; text-overflow:ellipsis;",
          r$HDA_LABEL
        ),
        tags$div(
          style = "font-size:10px; color:rgba(255,255,255,.5);",
          r$MUNICIPIO, " \u00b7 ",
          tags$span(style = paste0("color:", r$COL, "; font-weight:600;"), r$RIESGO)
        )
      )
    })

    tagList(items)
  })

  # Observer para centrar mapa al seleccionar resultado del buscador
  observeEvent(input$buscar_sel, {
    cod_key <- input$buscar_sel
    hda_sf  <- datos_r() %>% filter(COD_HDA_KEY == cod_key)
    if (nrow(hda_sf) > 0) {
      r      <- hda_sf %>% st_drop_geometry() %>% dplyr::slice(1)
      centro <- suppressWarnings(st_point_on_surface(hda_sf[1, ]))
      coords <- st_coordinates(centro)
      leafletProxy("mapa") %>%
        setView(lng = coords[1], lat = coords[2], zoom = 15) %>%
        clearPopups() %>%
        addPopups(
          lng = coords[1], lat = coords[2],
          popup = paste0(
            "<div style='font-family:\"Outfit\", sans-serif; padding:12px; min-width:280px; background:#0f172a; color:white; border-radius:12px; border:1px solid rgba(255,255,255,0.1); shadow:0 10px 30px rgba(0,0,0,0.5);'>",
            "<div style='border-bottom: 2px solid ", r$COL, "; padding-bottom:10px; margin-bottom:10px;'>",
            "<h4 style='margin:0; font-weight:900; letter-spacing:-0.5px;'>", r$HDA_LABEL, "</h4>",
            "<span style='font-size:11px; opacity:0.6;'>C\u00d3DIGO: ", r$COD_HDA_KEY, "</span></div>",
            "<div style='display:grid; grid-template-columns: 1fr 1fr; gap:8px; font-size:12px;'>",
            "<div><b style='color:#94a3b8;'>INGENIO</b><br>", r$INGENIO_FULL, "</div>",
            "<div><b style='color:#94a3b8;'>MUNICIPIO</b><br>", r$MUNICIPIO, "</div>",
            "<div><b style='color:#94a3b8;'>HISTORIAL</b><br>", r$N_EVENTOS, " incendios</div>",
            "<div><b style='color:#94a3b8;'>\u00daLTIMO</b><br>", r$TXT_ULTIMO, "</div>",
            "</div>",
            "<div style='margin-top:10px; padding:10px; background:rgba(255,255,255,0.05); border-radius:8px;'>",
            "<b>RIESGO:</b> <span style='color:", r$COL, "; font-weight:900;'>", r$RIESGO, "</span><br>",
            "<b>PR\u00d3XIMO ESTIMADO:</b> ", r$TXT_ESTIMADO, "<br>",
            "<b>ESTATUS BIOMASA:</b> ", r$ESTADO_BIOMASA, "</div>",
            "<div style='margin-top:10px; font-size:11px; font-style:italic; opacity:0.7;'>\ud83d\udee1\ufe0f CVC: ", r$ESTADO_CONTROL, "</div>",
            "</div>"
          )
        )
    }
  }, ignoreInit = TRUE)
  
  filtro_r <- reactiveVal(NULL)
  
  observeEvent(input$reset, { 
    filtro_r(NULL)
    updatePickerInput(session, "f_mun", selected = "TODOS") 
    leafletProxy("mapa") %>% setView(-76.3, 3.5, 10)
  })
  
  observeEvent(input$click_red, { filtro_r("CRITICO") })
  observeEvent(input$click_orange, { filtro_r("ALTO") })
  observeEvent(input$click_yellow, { filtro_r("OBSERVACION") })
  
  observeEvent(input$h_anio, {
    if (!is.null(input$h_anio) && length(input$h_anio) == 1 && !any(toupper(input$h_anio) == "TODOS")) {
      updateRadioButtons(session, "h_agrupa", selected = "MES")
    }
  }, ignoreInit = TRUE)
  
  # --- LECTURA DEL ARCHIVO DE VISITAS (\u00danico CSV Liviano) ---
  datos_visitas <- reactive({
    ruta_csv <- "visitas_cvc.csv"
    df_v <- NULL
    
    if (.ON_CLOUD) {
      df_v <- tryCatch({ read.csv(url("https://secasor.github.io/SATICA%20V2/visitas_cvc.csv"), stringsAsFactors = FALSE) %>% clean_names() }, error = function(e) NULL)
    } else {
      if (file.exists(ruta_csv)) {
        df_v <- tryCatch({ read.csv(ruta_csv, stringsAsFactors = FALSE) %>% clean_names() }, error = function(e) NULL)
      }
    }
    
    if(!is.null(df_v) && all(c("cod_hda_key", "fecha_visita") %in% names(df_v))) {
      df_procesado <- df_v %>%
        mutate(
          COD_HDA_KEY = toupper(trimws(as.character(cod_hda_key))),
          FECHA_VISITA = as.Date(fecha_visita)
        ) %>%
        group_by(COD_HDA_KEY) %>%
        summarise(
          HISTORIAL_VISITAS = paste(sort(format(unique(FECHA_VISITA), "%Y-%m-%d")), collapse = " | "),
          FECHA_VISITA = max(FECHA_VISITA, na.rm = TRUE),
          RADICADO = if("radicado" %in% names(df_v)) { val <- trimws(as.character(last(radicado))); ifelse(is.na(val) | val == "", "S/N", val) } else "S/N",
          .groups = "drop"
        )
      return(df_procesado)
    }
    
    return(data.frame(COD_HDA_KEY = character(), FECHA_VISITA = as.Date(character()), RADICADO = character()))
  })
  
  # --- PROCESAMIENTO ESTRAT\u00c9GICO Y MATRIZ TEMPORAL ---
  centros_global <- suppressWarnings(st_point_on_surface(DATOS_OFICIALES))
  coords_global <- as.data.frame(st_coordinates(centros_global))
  
  # --- DEEP LINKING DESDE TELEGRAM (SATICA V2.0) ---
  observeEvent(session$clientData$url_search, {
    query <- shiny::parseQueryString(session$clientData$url_search)
    
    if (!is.null(query$tab)) {
      # Si recibe tab=Satelital redirigir al Radar Preventivo ("dash")
      tab_target <- if(query$tab == "Satelital") "dash" else query$tab
      updateTabItems(session, "tabs", tab_target)
    }
    
    if (!is.null(query$lat) && !is.null(query$lon)) {
      lat <- as.numeric(query$lat)
      lon <- as.numeric(query$lon)
      
      if (!is.na(lat) && !is.na(lon)) {
        # Mover c\u00e1mara apenas se rendericen los datos
        shiny::observeEvent(datos_r(), {
          leafletProxy("mapa") %>%
            setView(lng = lon, lat = lat, zoom = 15)
        }, once = TRUE, ignoreInit = FALSE)
      }
    }
  }, once = TRUE)
  
  # Polling cada 30 segundos usando reactiveTimer en nube o reactivo local
  timer_goes <- reactiveTimer(30000)
  
  detect_goes_r <- reactive({
    timer_goes()
    if (.ON_CLOUD) {
      url_goes <- "https://secasor.github.io/SATICA%20V2/data_master/GOES16_Alertas.csv"
      tryCatch({
        read.csv(url(url_goes), stringsAsFactors = FALSE) %>%
          mutate(cod_unico = as.character(cod_unico)) %>%
          select(cod_unico, GOES_Fuego, Estado_GOES) %>%
          distinct(cod_unico, .keep_all = TRUE)
      }, error = function(e) data.frame(cod_unico=character(), GOES_Fuego=logical(), Estado_GOES=character()))
    } else {
      ruta <- "data_master/GOES16_Alertas.csv"
      if (file.exists(ruta)) {
        tryCatch({ 
          read.csv(ruta, stringsAsFactors = FALSE) %>% 
            mutate(cod_unico = as.character(cod_unico)) %>%
            select(cod_unico, GOES_Fuego, Estado_GOES) %>%
            distinct(cod_unico, .keep_all = TRUE)
        }, error = function(e) data.frame(cod_unico=character(), GOES_Fuego=logical(), Estado_GOES=character()))
      } else {
        data.frame(cod_unico=character(), GOES_Fuego=logical(), Estado_GOES=character())
      }
    }
  })

  # --- PROCESAMIENTO ESTRAT\u00c9GICO Y MATRIZ TEMPORAL ---
  datos_r <- reactive({
    req(detect_goes_r())
    update_trigger() # Trigger de actualización manual
    
    # 1. Preparar datos base con coordenadas
    centros_tmp <- suppressWarnings(st_point_on_surface(DATOS_OFICIALES))
    coords_tmp  <- as.data.frame(st_coordinates(centros_tmp))
    
    df <- DATOS_OFICIALES %>%
      mutate(LAT = coords_tmp$Y, LON = coords_tmp$X)
    
    # 2. Inyectar Alertas GOES Din\u00e1micas
    df_goes_dyn <- detect_goes_r()
    if (nrow(df_goes_dyn) > 0) {
      df <- df %>%
        left_join(df_goes_dyn %>% rename(GOES_FUEGO_NEW = GOES_Fuego, ESTADO_GOES_NEW = Estado_GOES), 
                  by = c("COD_HDA_KEY" = "cod_unico")) %>%
        mutate(
          GOES_FUEGO = coalesce(GOES_FUEGO_NEW, GOES_FUEGO),
          ESTADO_GOES = coalesce(ESTADO_GOES_NEW, ESTADO_GOES)
        ) %>%
        select(-ends_with("_NEW"))
    }
    
    # 3. Filtros de Usuario (Ingenio y Municipio)
    df <- df %>% filter(INGENIO_FULL %in% input$f_ing)
    if (!is.null(input$f_mun) && input$f_mun != "TODOS") {
      df <- df %>% filter(MUNICIPIO == input$f_mun)
    }
    
    # 4. C\u00e1lculo de Riesgo y Cruce con Visitas
    fecha_hoy <- Sys.Date()
    df <- df %>% 
      left_join(datos_visitas(), by = "COD_HDA_KEY")
      
    if (!"HISTORIAL_VISITAS" %in% names(df)) df$HISTORIAL_VISITAS <- NA_character_
    if (!"FECHA_VISITA" %in% names(df)) df$FECHA_VISITA <- as.Date(NA)
    if (!"RADICADO" %in% names(df)) df$RADICADO <- "S/N"
    
    df <- df %>%
      mutate(
        DIAS_DESDE_ULT = as.numeric(fecha_hoy - FECHA_ULT_I),
        ESTADO_RADAR = case_when(
          is.na(N_EVENTOS) | N_EVENTOS == 0 ~ "SIN_HISTORIAL",
          N_EVENTOS == 1 ~ "OCASIONAL",
          TRUE ~ "RECURRENTE"
        ),
        DIFF_MESES = if_else(ESTADO_RADAR == "RECURRENTE", (DIAS_DESDE_ULT - CICLO_DIAS) / 30.44, NA_real_),
        RIESGO = case_when(
          ESTADO_RADAR != "RECURRENTE" ~ "BAJO",
          DIFF_MESES < -3 ~ "BAJO",
          DIFF_MESES >= -3 & DIFF_MESES < -2 ~ "OBSERVACION",
          DIFF_MESES >= -2 & DIFF_MESES < -1 ~ "ALTO",
          DIFF_MESES >= -1 & DIFF_MESES <= 1 ~ "CRITICO",
          DIFF_MESES > 1 & DIFF_MESES <= 2 ~ "ALTO",
          DIFF_MESES > 2 & DIFF_MESES <= 3 ~ "OBSERVACION",
          DIFF_MESES > 3 ~ "MITIGADO",
          TRUE ~ "BAJO"
        ),
        COL = case_when(
          RIESGO == "BAJO" ~ "#27ae60",
          RIESGO == "OBSERVACION" ~ "#f1c40f",
          RIESGO == "ALTO" ~ "#e67e22",
          RIESGO == "CRITICO" ~ "#c0392b",
          RIESGO == "MITIGADO" ~ "#27ae60",
          TRUE ~ "#27ae60"
        ),
        TXT_CICLO = sapply(CICLO_DIAS, function(dias) {
          if (is.na(dias) || dias == 0) return("Sin Historial")
          if (dias < 60) return(paste(round(dias), "d\u00edas"))
          meses <- floor(dias / 30.44); sobra <- round(dias %% 30.44)
          txt_m <- ifelse(meses == 1, "1 mes", paste(meses, "meses")); txt_d <- ifelse(sobra > 0, paste(sobra, "d\u00edas"), "")
          return(trimws(paste(txt_m, txt_d)))
        }),
        TXT_ULTIMO = ifelse(!is.na(FECHA_ULT_I), as.character(FECHA_ULT_I), "Sin Incendios"),
        TXT_ESTIMADO = ifelse(!is.na(FECHA_ULT_I) & !is.na(CICLO_DIAS), as.character(as.Date(FECHA_ULT_I + CICLO_DIAS)), "N/A"),
        
        VISITA_VALIDA = case_when(
          is.na(FECHA_VISITA) ~ FALSE,
          is.na(FECHA_ULT_I) ~ TRUE, 
          FECHA_VISITA >= FECHA_ULT_I ~ TRUE,
          TRUE ~ FALSE
        ),
        ESTADO_CONTROL = case_when(
          !VISITA_VALIDA ~ "Sin Intervencion",
          VISITA_VALIDA & RIESGO %in% c("ALTO", "CRITICO") ~ "Visitado",
          VISITA_VALIDA & (RIESGO == "BAJO" | RIESGO == "MITIGADO") & DIFF_MESES > 3 ~ "Incendio Evitado (Exito)",
          TRUE ~ "Visita Preventiva"
        )
      )
    
    if (!is.null(filtro_r())) df <- df %>% filter(RIESGO == filtro_r())
    return(df)
  })
  
  # --- FILTRADO T\u00c1CTICO (SOLO EMERGENCIAS REALES) ---
  datos_tacticos <- reactive({
    req(datos_r())
    # Filtramos por las dos fuentes satelitales configuradas en global.R
    df_t <- datos_r() %>% 
      filter(SAT_FUEGO == TRUE | GOES_FUEGO == TRUE)
    return(df_t)
  })
  
  # --- DESCARGA DE PLANTILLA FOCALIZADA CON ETIQUETADO INTELIGENTE ---
  output$descargar_plantilla <- downloadHandler(
    filename = function() {
      paste0("Plantilla_Visitas_SATICA_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df_base <- datos_r() %>% st_drop_geometry() %>%
        # Sincronizaci\u00f3n Top 10
        mutate(PESO = case_when(RIESGO == "CRITICO" ~ 1, RIESGO == "ALTO" ~ 2, RIESGO == "OBSERVACION" ~ 3, TRUE ~ 4))
      
      # 1. Alerta Inminente
      df_inminentes <- df_base %>% 
        filter(RIESGO %in% c("CRITICO", "ALTO")) %>%
        mutate(es_inminente = TRUE) %>% select(COD_HDA_KEY, es_inminente)
      
      # 2. Focalizaci\u00f3n Operativa (Top 10 por Municipio)
      df_foc_mun <- df_base %>%
        group_by(MUNICIPIO) %>% arrange(PESO, desc(DIFF_MESES)) %>% slice_head(n = 10) %>% ungroup() %>%
        mutate(es_foc_mun = TRUE) %>% select(COD_HDA_KEY, es_foc_mun)
      
      # 3. Top 10 Regional
      df_top_reg <- df_base %>%
        arrange(PESO, desc(DIFF_MESES)) %>% slice_head(n = 10) %>%
        mutate(es_top_reg = TRUE) %>% select(COD_HDA_KEY, es_top_reg)
      
      df_keys <- bind_rows(df_inminentes %>% select(COD_HDA_KEY), df_foc_mun %>% select(COD_HDA_KEY), df_top_reg %>% select(COD_HDA_KEY)) %>% distinct()
      
      df_plantilla <- df_base %>%
        inner_join(df_keys, by = "COD_HDA_KEY") %>%
        left_join(df_inminentes, by = "COD_HDA_KEY") %>%
        left_join(df_foc_mun, by = "COD_HDA_KEY") %>%
        left_join(df_top_reg, by = "COD_HDA_KEY") %>%
        mutate(
          es_inminente = ifelse(is.na(es_inminente), FALSE, es_inminente),
          es_foc_mun = ifelse(is.na(es_foc_mun), FALSE, es_foc_mun),
          es_top_reg = ifelse(is.na(es_top_reg), FALSE, es_top_reg),
          cat_1 = ifelse(es_inminente, "Alerta Inminente", NA),
          cat_2 = ifelse(es_foc_mun, "Focalizaci\u00f3n Operativa", NA),
          cat_3 = ifelse(es_top_reg, "Top 10 Regional", NA)
        ) %>%
        rowwise() %>%
        mutate(CATEGORIA_ALERTA = paste(na.omit(c(cat_1, cat_2, cat_3)), collapse = " + ")) %>%
        ungroup() %>%
        select(COD_HDA_KEY, HDA_LABEL, INGENIO_FULL, MUNICIPIO, CORREGIMIENTO, CATEGORIA_ALERTA) %>%
        mutate(FECHA_VISITA = "", RADICADO = "")
      
      write.csv(df_plantilla, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  output$mapa <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter", group = "Modo Noche (Stitch)") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satelital (Real)") %>%
      addProviderTiles("CartoDB.PositronOnlyLabels", group = "Labels") %>%
      addTiles(
        urlTemplate = "https://mt1.google.com/vt/lyrs=y&x={x}&y={y}&z={z}",
        attribution = "© Google Maps",
        group = "H\u00edbrido Google"
      ) %>%
      setView(-76.3, 3.5, 10)
  })
  
  # --- CARGAR TRAYECTORIAS HYSPLIT ---
  hysplit_r <- reactive({
    if (.ON_CLOUD) {
      plumas <- tryCatch(readRDS(url("https://secasor.github.io/SATICA%20V2/data_master/HYSPLIT_plumas.rds")), error = function(e) NULL)
      return(plumas)
    } else {
      if (file.exists("data_master/HYSPLIT_plumas.rds")) {
        plumas <- tryCatch(readRDS("data_master/HYSPLIT_plumas.rds"), error = function(e) NULL)
        return(plumas)
      }
    }
    return(NULL)
  })
  
  # --- ACTUALIZACI\u00d3N DIN\u00c1MICA MAPA PREVENTIVO ---
  observe({
    req(datos_r())
    plumas <- hysplit_r()
    
    df_all   <- datos_r()
    df_poly  <- df_all %>% filter(!coalesce(ES_ANCLA, FALSE))
    df_ancla <- df_all %>% filter(coalesce(ES_ANCLA, FALSE))
    
    proxy <- leafletProxy("mapa") %>% 
      clearGroup("Haciendas de Riesgo") %>%
      clearGroup("Simulaci\u00f3n Humo (HYSPLIT)") %>%
      clearGroup("Flagrancia Satelital (FIRMS/GOES)") %>%
      clearGroup("Ancla Operativa (GPS)") %>%
      clearGroup("Zonas Restringidas")

    # Capas de Restricci\u00f3n Ambiental (CVC)
    if (!is.null(SHP_REST_RAMSAR))   proxy %>% addPolygons(data = SHP_REST_RAMSAR,   color = "#00bcd4", weight = 1, fillOpacity = 0.15, group = "Zonas Restringidas", options = pathOptions(interactive = FALSE))
    if (!is.null(SHP_REST_AP))       proxy %>% addPolygons(data = SHP_REST_AP,       color = "#f39c12", weight = 1, fillOpacity = 0.15, group = "Zonas Restringidas", options = pathOptions(interactive = FALSE))
    if (!is.null(SHP_REST_FORESTAL)) proxy %>% addPolygons(data = SHP_REST_FORESTAL, color = "#2ecc71", weight = 1, fillOpacity = 0.2, group = "Zonas Restringidas", options = pathOptions(interactive = FALSE))
    if (!is.null(SHP_REST_RECARGA))  proxy %>% addPolygons(data = SHP_REST_RECARGA,  color = "#3498db", weight = 1, fillOpacity = 0.15, group = "Zonas Restringidas", options = pathOptions(interactive = FALSE))
    if (!is.null(SHP_REST_CAUCA))    proxy %>% addPolygons(data = SHP_REST_CAUCA,    color = "#2980b9", weight = 1, fillOpacity = 0.2, group = "Zonas Restringidas", options = pathOptions(interactive = FALSE))
    if (!is.null(SHP_REST_POBLADOS)) proxy %>% addPolygons(data = SHP_REST_POBLADOS, color = "#e67e22", weight = 1, fillOpacity = 0.2, group = "Zonas Restringidas", options = pathOptions(interactive = FALSE))
    if (!is.null(SHP_REST_POZOS))    proxy %>% addPolygons(data = SHP_REST_POZOS,    color = "#9b59b6", weight = 1, fillOpacity = 0.2, group = "Zonas Restringidas", options = pathOptions(interactive = FALSE))
    
    # Pol\u00edgonos (haciendas con geometr\u00eda SIG completa)
    if (nrow(df_poly) > 0) {
      proxy %>%
        addPolygons(
          data = df_poly,
          fillColor = ~COL, fillOpacity = 0.8, weight = 1.5, color = "white", layerId = ~COD_HDA_KEY,
          group = "Haciendas de Riesgo",
          popup = ~paste0(
            "<div style='font-family:sans-serif; font-size:13px; min-width:230px;'>",
            "<div style='background-color:#ecf0f1; padding:6px; border-bottom:2px solid ", COL, ";'>",
            "<b>", HDA_LABEL, "</b> <span style='font-size:10px;'>[Cod: ", COD_HDA_KEY, "]</span></div>",
            "<div style='padding:8px;'>",
            "<b>Ingenio:</b> ", INGENIO_FULL, "<br>",
            "<b>Municipio:</b> ", MUNICIPIO, "<br>",
            "<b>Corregimiento:</b> ", CORREGIMIENTO, "<br>",
            "<b>Historial:</b> ", N_EVENTOS, " eventos registrados<br>",
            "<b>\u00daLTIMO INCENDIO:</b> ", TXT_ULTIMO, "<br>",
            "<b>CICLO ESTIMADO:</b> ", TXT_ESTIMADO, " <i>(", TXT_CICLO, ")</i><br>",
            "<b>Nivel Riesgo:</b> <b style='color:", COL, ";'>", RIESGO, "</b><br>",
            "<hr style='margin:5px 0;'>",
            "<b>\ud83c\udf3f Estatus Biomasa (Sentinel-2):</b> ", ESTADO_BIOMASA, "<br>",
            "<hr style='margin:5px 0;'>",
            "<b>Gesti\u00f3n CVC:</b> ", ESTADO_CONTROL,
            ifelse(!is.na(FECHA_VISITA), paste0("<br><b>\u00daLTIMA VISITA:</b> ", FECHA_VISITA, " <i>(Radicado: ", RADICADO, ")</i>"), ""),
            ifelse(is.na(HISTORIAL_VISITAS) | HISTORIAL_VISITAS == as.character(FECHA_VISITA) | HISTORIAL_VISITAS=="", "", paste0("<br><b>\ud83e\uddfe Trazabilidad Visitas:</b> ", HISTORIAL_VISITAS)),
            "<hr style='margin:5px 0;'>",
            "<b>\ud83d\udee1\ufe0f Restricciones Ambientales:</b> ", RESTRICCIONES_CVC,
            "</div></div>"
          ),
          highlightOptions = highlightOptions(weight = 4, color = "black", bringToFront = TRUE)
        )
    }
    
    # Puntos GPS (Ancla Operativa)
    if (nrow(df_ancla) > 0) {
      proxy %>%
        addCircleMarkers(
          data = df_ancla,
          lng = ~LON, lat = ~LAT,
          radius = 10,
          color = "white", fillColor = ~COL, fillOpacity = 0.9,
          weight = 2, stroke = TRUE,
          layerId = ~paste0("ancla_", COD_HDA_KEY),
          group = "Ancla Operativa (GPS)",
          popup = ~paste0(
            "<div style='font-family:sans-serif; font-size:13px; min-width:230px;'>",
            "<div style='background-color:#ecf0f1; padding:6px; border-bottom:2px solid ", COL, ";'>",
            "\ud83d\udccd <b>", HDA_LABEL, "</b> <span style='font-size:10px;'>[Cod: ", COD_HDA_KEY, "]</span>",
            "<br><span style='font-size:10px; color:#e74c3c;'>Punto GPS (Sin Pol\u00edgono Catastral)</span></div>",
            "<div style='padding:8px;'>",
            "<b>Ingenio:</b> ", INGENIO_FULL, "<br>",
            "<b>Municipio:</b> ", MUNICIPIO, "<br>",
            "<b>Corregimiento:</b> ", CORREGIMIENTO, "<br>",
            "<b>Historial:</b> ", N_EVENTOS, " eventos registrados<br>",
            "<b>\u00daLTIMO INCENDIO:</b> ", TXT_ULTIMO, "<br>",
            "<b>CICLO ESTIMADO:</b> ", TXT_ESTIMADO, " <i>(", TXT_CICLO, ")</i><br>",
            "<b>Nivel Riesgo:</b> <b style='color:", COL, ";'>", RIESGO, "</b><br>",
            "<hr style='margin:5px 0;'>",
            "<b>Gesti\u00f3n CVC:</b> ", ESTADO_CONTROL,
            ifelse(!is.na(FECHA_VISITA), paste0("<br><b>\u00daLTIMA VISITA:</b> ", FECHA_VISITA, " <i>(Radicado: ", RADICADO, ")</i>"), ""),
            ifelse(is.na(HISTORIAL_VISITAS) | HISTORIAL_VISITAS == as.character(FECHA_VISITA) | HISTORIAL_VISITAS=="", "", paste0("<br><b>\ud83e\uddfe Trazabilidad Visitas:</b> ", HISTORIAL_VISITAS)),
            "<hr style='margin:5px 0;'>",
            "<b>\ud83d\udee1\ufe0f Restricciones Ambientales:</b> ", RESTRICCIONES_CVC,
            "</div></div>"
          )
        )
    }
      
    # A\u00f1adir plumas HYSPLIT si existen (Como grupo controlable)
    if (!is.null(plumas) && inherits(plumas, "sf") && nrow(plumas) > 0) {
      proxy %>%
        addPolylines(
          data = plumas,
          color = "#9b59b6", 
          weight = 3,
          dashArray = "5, 5", 
          opacity = 0.8,
          group = "Simulaci\u00f3n Humo (HYSPLIT)",
          layerId = ~paste0("hysplit_", cod_hda_key, "_", height),
          popup = ~paste0("<b>\ud83e\udded Simulaci\u00f3n Preventiva de Humo (Predominancia de Vientos)</b><br>Proyecci\u00f3n de alcance a 6 horas<br>Hacienda Origen: ", hda_nombre)
        )
    }
    
    # Agregar capa de Flagrancia Satelital si hay detecciones
    df_fuego <- datos_r() %>% filter(SAT_FUEGO == TRUE | GOES_FUEGO == TRUE)
    if (nrow(df_fuego) > 0) {
      proxy %>%
        addPolygons(
          data = df_fuego,
          fillColor = "#e74c3c", fillOpacity = 0.55, weight = 3, color = "#ff0000",
          group = "Flagrancia Satelital (FIRMS/GOES)",
          popup = ~paste0(
            "<div style='font-family:sans-serif; padding:8px;'>",
            "<h4 style='color:#e74c3c; margin:0;'>\ud83d\udea8 ALERTA DE INCENDIO</h4>",
            "<hr>",
            "<b>Hacienda:</b> ", HDA_LABEL, "<br>",
            "<b>Ingenio:</b> ", INGENIO_FULL, "<br>",
            "<b>GOES-16:</b> ", ifelse(GOES_FUEGO, ESTADO_GOES, "No Detectado"), "<br>",
            "<b>FIRMS 24h:</b> ", ifelse(SAT_FUEGO, "FUEGO CONFIRMADO", "No Detectado"),
            "</div>"
          )
        ) %>%
        addCircleMarkers(
          data = df_fuego,
          lng = ~LON, lat = ~LAT,
          radius = 14, color = "#ff0000", fillColor = "#e74c3c", fillOpacity = 0.35,
          weight = 2, group = "Flagrancia Satelital (FIRMS/GOES)"
        )
    }

    # Agregar Control de Capas para evitar saturaci\u00f3n
    proxy %>%
      addLayersControl(
        baseGroups = c("Sat\u00e9lite (Esri)", "Google Hybrid", "Mapa Base (OSM)"),
        overlayGroups = c("Haciendas de Riesgo", "Ancla Operativa (GPS)", "Simulaci\u00f3n Humo (HYSPLIT)", "Flagrancia Satelital (FIRMS/GOES)", "Zonas Restringidas"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      hideGroup("Flagrancia Satelital (FIRMS/GOES)") %>%
      hideGroup("Zonas Restringidas")
  })
  
  observeEvent(input$tabla_top_rows_selected, {
    row_idx <- input$tabla_top_rows_selected
    data_view <- datos_r() %>% st_drop_geometry()
    
    if (is.null(input$f_mun) || input$f_mun == "TODOS") {
      df_sorted <- data_view %>% 
        mutate(PESO = case_when(RIESGO == "CRITICO" ~ 1, RIESGO == "ALTO" ~ 2, RIESGO == "OBSERVACION" ~ 3, TRUE ~ 4)) %>%
        filter(MUNICIPIO %in% c("EL CERRITO", "PALMIRA", "PRADERA", "CANDELARIA", "FLORIDA")) %>%
        group_by(MUNICIPIO) %>% arrange(PESO, desc(DIFF_MESES)) %>% slice_head(n = 2) %>% ungroup() %>% arrange(MUNICIPIO, PESO)
    } else {
      df_sorted <- data_view %>% mutate(PESO = case_when(RIESGO == "CRITICO" ~ 1, RIESGO == "ALTO" ~ 2, RIESGO == "OBSERVACION" ~ 3, TRUE ~ 4)) %>%
        arrange(PESO, desc(DIFF_MESES)) %>% slice_head(n = 10)
    }
    
    selected_hda <- df_sorted[row_idx, ]
    if (nrow(selected_hda) > 0) {
      popup_html <- paste0(
        "<div style='font-family:sans-serif; font-size:13px; min-width:230px;'>",
        "<div style='background-color:#ecf0f1; padding:6px; border-bottom:2px solid ", selected_hda$COL, ";'>",
        "<b>", selected_hda$HDA_LABEL, "</b> <span style='font-size:10px;'>[Cod: ", selected_hda$COD_HDA_KEY, "]</span></div>",
        "<div style='padding:8px;'>",
        "<b>Ingenio:</b> ", selected_hda$INGENIO_FULL, "<br>",
        "<b>Municipio:</b> ", selected_hda$MUNICIPIO, "<br>",
        "<b>Historial:</b> ", selected_hda$N_EVENTOS, " eventos registrados<br>",
        "<b>\u00daLTIMO INCENDIO:</b> ", selected_hda$TXT_ULTIMO, "<br>",
        "<b>CICLO ESTIMADO:</b> ", selected_hda$TXT_ESTIMADO, " <i>(", selected_hda$TXT_CICLO, ")</i><br>",
        "<b>Nivel Riesgo:</b> <b style='color:", selected_hda$COL, ";'>", selected_hda$RIESGO, "</b><br>",
        "<hr style='margin:5px 0;'>",
        "<b>\ud83d\udef0\ufe0f NASA FIRMS:</b> ", ifelse(selected_hda$SAT_FUEGO, "<b style='color:#c0392b;'>\ud83d\ude80 FUEGO DETECTADO (24H)</b>", "Cielo Limpio"), "<br>",
        "<b>\ud83d\udce1 GOES-16:</b> ", ifelse(selected_hda$GOES_FUEGO, paste0("<b style='color:#e74c3c;'>", selected_hda$ESTADO_GOES, "</b>"), "Sin Fuego Din\u00e1mico"), "<br>",
        "<b>\ud83c\udf3f Sentinel-2:</b> ", selected_hda$ESTADO_BIOMASA, "<br>",
        "<hr style='margin:5px 0;'>",
        "<b>Gesti\u00f3n CVC:</b> ", selected_hda$ESTADO_CONTROL,
        ifelse(!is.na(selected_hda$FECHA_VISITA), paste0("<br><b>\u00daLTIMA VISITA:</b> ", selected_hda$FECHA_VISITA, " <i>(Radicado: ", selected_hda$RADICADO, ")</i>"), ""),
        ifelse(is.na(selected_hda$HISTORIAL_VISITAS) | selected_hda$HISTORIAL_VISITAS == as.character(selected_hda$FECHA_VISITA) | selected_hda$HISTORIAL_VISITAS=="", "", paste0("<br><b>\ud83e\uddfe Trazabilidad Visitas:</b> ", selected_hda$HISTORIAL_VISITAS)),
        "<hr style='margin:5px 0;'>",
        "<b>\ud83d\udee1\ufe0f Restricciones Ambientales:</b> ", selected_hda$RESTRICCIONES_CVC,
        "</div></div>"
      )
      
      leafletProxy("mapa") %>% 
        setView(lng = selected_hda$LON, lat = selected_hda$LAT, zoom = 14) %>%
        clearPopups() %>%
        addPopups(lng = selected_hda$LON, lat = selected_hda$LAT, popup = popup_html)
    }
  })
  
  output$tabla_top <- renderDT({
    req(datos_r())
    df_tbl <- datos_r() %>% st_drop_geometry() %>%
      mutate(PESO = case_when(RIESGO == "CRITICO" ~ 1, RIESGO == "ALTO" ~ 2, RIESGO == "OBSERVACION" ~ 3, TRUE ~ 4))
    
    if (is.null(input$f_mun) || input$f_mun == "TODOS") {
      target_munis <- c("EL CERRITO", "PALMIRA", "PRADERA", "CANDELARIA", "FLORIDA")
      final_tbl <- df_tbl %>% filter(MUNICIPIO %in% target_munis) %>% group_by(MUNICIPIO) %>%
        arrange(PESO, desc(DIFF_MESES)) %>% slice_head(n = 2) %>% ungroup() %>% arrange(MUNICIPIO, PESO)
    } else {
      final_tbl <- df_tbl %>% arrange(PESO, desc(DIFF_MESES)) %>% slice_head(n = 10)
    }
    
    final_tbl %>%
      mutate(COORDS = paste(round(LAT,4), round(LON,4), sep=", ")) %>%
      select(HDA_LABEL, MUNICIPIO, RIESGO, ESTADO_CONTROL) %>%
      datatable(selection = 'single', rownames = FALSE, options = list(pageLength = 10, dom = 't'))
  })
  
  output$tabla_completa <- renderDT({
    req(datos_r())
    datos_r() %>% st_drop_geometry() %>%
      select(COD_HDA_KEY, HDA_LABEL, INGENIO_FULL, MUNICIPIO, RIESGO, SAT_FUEGO, GOES_FUEGO, ESTADO_BIOMASA, FECHA_ULT_I, TXT_ESTIMADO, ESTADO_CONTROL) %>%
      datatable(extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = c('excel', 'csv')), rownames=FALSE)
  })
  
  output$descargar_reporte <- downloadHandler(
    filename = function() { paste0("Boletin_SATICA_", Sys.Date(), ".pdf") },
    content = function(file) {
      temp_rmd <- file.path(tempdir(), "reporte.Rmd")
      file.copy("reporte.Rmd", temp_rmd, overwrite = TRUE)
      out_html <- rmarkdown::render(temp_rmd, params = list(datos = datos_r()), quiet = TRUE)
      pagedown::chrome_print(out_html, output = file)
    }
  )
  
  output$descargar_shape <- downloadHandler(
    filename = function() {
      prefijo <- if(input$f_mun == "TODOS") "REGIONAL" else input$f_mun
      paste0("SATICA_GIS_", prefijo, "_", format(Sys.Date(), "%Y%m%d"), ".kml")
    },
    content = function(file) {
      
      # \u2500\u2500 Datos: solo CRITICO y ALTO \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
      poligonos_gis <- datos_r() %>%
        filter(RIESGO %in% c("CRITICO", "ALTO")) %>%
        filter(!coalesce(ES_ANCLA, FALSE)) %>%
        mutate(
          RIESGO_GIS = as.character(RIESGO),    # Campo expl\u00edcito sin ambig\u00fcedad
          NOM_PRED   = substr(HDA_LABEL, 1, 100),
          INGENIO    = INGENIO_FULL,
          MUN        = MUNICIPIO,
          TIPO       = ifelse(RIESGO == "CRITICO", "INMINENTE", "ALTO"),
          FECHA_EST  = as.character(suppressWarnings(as.Date(FECHA_ULT_I + CICLO_DIAS)))
        ) %>%
        select(NOM_PRED, INGENIO, MUN, RIESGO_GIS, TIPO, FECHA_EST)
      
      # \u2500\u2500 Paso 1: sf escribe el KML base (sin colores) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
      kml_tmp <- tempfile(fileext = ".kml")
      sf::st_write(poligonos_gis, dsn = kml_tmp, driver = "KML",
                   delete_dsn = TRUE, quiet = TRUE)
      
      # \u2500\u2500 Paso 2: Inyectar estilos de color en el KML \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
      # KML usa color en formato AABBGGRR (alpha-blue-green-red, invertido al RGB)
      # CRITICO #c0392b \u2192 alpha=d9, B=2b, G=39, R=c0 \u2192 "d92b39c0"
      # ALTO    #e67e22 \u2192 alpha=d9, B=22, G=7e, R=e6 \u2192 "d9227ee6"
      kml_lines <- readLines(kml_tmp, warn = FALSE, encoding = "UTF-8")
      
      style_block <- c(
        '  <Style id="CRITICO">',
        '    <LineStyle><color>ff0a143c</color><width>1.5</width></LineStyle>',
        '    <PolyStyle><color>d92b39c0</color><fill>1</fill><outline>1</outline></PolyStyle>',
        '  </Style>',
        '  <Style id="ALTO">',
        '    <LineStyle><color>ff0032c8</color><width>1.5</width></LineStyle>',
        '    <PolyStyle><color>d9227ee6</color><fill>1</fill><outline>1</outline></PolyStyle>',
        '  </Style>'
      )
      
      new_lines   <- character(0)
      cur_riesgo  <- NA_character_
      styles_done <- FALSE
      
      for (ln in kml_lines) {
        
        # Insertar bloque de estilos justo antes del primer <Folder>
        # (los Style deben ser hijos directos de <Document>, antes de los Placemarks)
        if (!styles_done && grepl("<Folder>", ln, fixed = TRUE)) {
          new_lines   <- c(new_lines, style_block, ln)
          styles_done <- TRUE
          next
        }
        
        # Inicio de Placemark \u2192 resetear el riesgo capturado
        if (grepl("<Placemark>", ln, fixed = TRUE)) cur_riesgo <- NA_character_
        
        # Detectar el valor de RIESGO_GIS en el SimpleData de este Placemark
        if (grepl('name="RIESGO_GIS"', ln, fixed = TRUE)) {
          m <- regmatches(ln, regexpr('(?<=<SimpleData name="RIESGO_GIS">)[^<]+', ln, perl = TRUE))
          if (length(m) > 0) cur_riesgo <- trimws(m[1])
        }
        
        # Insertar <styleUrl> ANTES del primer elemento geom\u00e9trico del Placemark
        # (ExtendedData viene antes de la geometr\u00eda en el output de GDAL/KML)
        if (!is.na(cur_riesgo) && grepl("<Polygon>|<MultiGeometry>", ln)) {
          sid       <- if (cur_riesgo == "CRITICO") "CRITICO" else "ALTO"
          new_lines <- c(new_lines, paste0("    <styleUrl>#", sid, "</styleUrl>"))
          cur_riesgo <- NA_character_   # Reset: solo una vez por Placemark
        }
        
        new_lines <- c(new_lines, ln)
      }
      
      writeLines(new_lines, file, useBytes = FALSE)
    }
  )



  # --- CAJAS DE RIESGO ---
  output$box_red <- renderValueBox({ 
    valueBox(
      value = tags$p(sum(datos_r()$RIESGO=="CRITICO"), style="font-size:3rem; margin-bottom:0; line-height:1;"), 
      subtitle = HTML("<b>CRÍTICO (±1 Mes)</b><br><span style='font-size:12px;'>Alto Riesgo</span>"),
      icon = icon("fire-alt"), 
      color = "danger"
    ) 
  })
  output$box_orange <- renderValueBox({ 
    valueBox(
      value = tags$p(sum(datos_r()$RIESGO=="ALTO"), style="font-size:3rem; margin-bottom:0; line-height:1;"), 
      subtitle = HTML("<b>ALTO (±2 Meses)</b><br><span style='font-size:12px;'>Riesgo Preventivo</span>"),
      icon = icon("exclamation-triangle"), 
      color = "orange"
    ) 
  })
  output$box_yellow <- renderValueBox({ 
    valueBox(
      value = tags$p(sum(datos_r()$RIESGO=="OBSERVACION"), style="font-size:3rem; margin-bottom:0; line-height:1;"), 
      subtitle = HTML("<b>OBSERVACIÓN (±3 Meses)</b><br><span style='font-size:12px;'>Riesgo Medio</span>"),
      icon = icon("eye"), 
      color = "warning"
    ) 
  })
  output$box_green <- renderValueBox({ 
    valueBox(
      value = tags$p(sum(datos_r()$RIESGO %in% c("BAJO", "MITIGADO")), style="font-size:3rem; margin-bottom:0; line-height:1;"), 
      subtitle = HTML("<b>BAJO (> 3 Meses)</b><br><span style='font-size:12px;'>Riesgo Controlado</span>"),
      icon = icon("check-circle"), 
      color = "success"
    ) 
  })
  
  # --- CAJAS OPERATIVAS (VISITAS) ---
  output$box_visitas_control <- renderValueBox({
    total_visitadas <- sum(datos_r()$ESTADO_CONTROL == "🛡️ Visitado", na.rm = TRUE)
    valueBox(total_visitadas, "Haciendas Visitadas en Riesgo Alto/Cr\u00edtico", icon = icon("shield-alt"), color = "purple")
  })
  
  output$box_visitas_exito <- renderValueBox({
    total_exitos <- sum(datos_r()$ESTADO_CONTROL == "✅ Incendio Evitado (\u00c9xito)", na.rm = TRUE)
    valueBox(total_exitos, "Incendios Evitados (Ciclo Superado con Visita)", icon = icon("award"), color = "olive")
  })

  # --- CAJAS T\u00c1CTICAS (VIGILANCIA) ---
  output$box_firms_24h <- renderValueBox({
    total <- sum(datos_r()$SAT_FUEGO == TRUE, na.rm = TRUE)
    valueBox(total, "Fuegos Detectados (NASA FIRMS 24h)", icon = icon("fire"), color = "danger")
  })

  output$box_goes_alert <- renderValueBox({
    total <- sum(datos_r()$GOES_FUEGO == TRUE, na.rm = TRUE)
    valueBox(total, "Alertas Din\u00e1micas (GOES-16 1h)", icon = icon("satellite-dish"), color = "warning")
  })
  
  output$grafo_riesgo <- visNetwork::renderVisNetwork({
    req(datos_r())
    datos_grafos <- datos_r() %>% sf::st_drop_geometry()
    
    nodos_hda <- datos_grafos %>%
      mutate(
        id = COD_HDA_KEY,
        label = HDA_LABEL,
        group = INGENIO_FULL,
        title = paste0("<b>Hacienda:</b> ", HDA_LABEL, "<br><b>Riesgo:</b> ", RIESGO),
        color = case_when(
          RIESGO == "CRITICO" ~ "#c0392b",
          RIESGO == "ALTO" ~ "#e67e22",
          RIESGO == "OBSERVACION" ~ "#f1c40f",
          TRUE ~ "#27ae60"
        ),
        shape = "dot", size = 20
      ) %>% select(id, label, group, title, color, shape, size)
    
    nodos_ing <- datos_grafos %>%
      distinct(INGENIO_FULL) %>%
      mutate(
        id = INGENIO_FULL, label = INGENIO_FULL,
        group = "INGENIO", title = "Ingenio Azucarero",
        color = "#2c3e50", shape = "square", size = 40
      ) %>% select(id, label, group, title, color, shape, size)
    
    todos_nodos <- bind_rows(nodos_hda, nodos_ing)
    enlaces <- datos_grafos %>% mutate(from = INGENIO_FULL, to = COD_HDA_KEY) %>% select(from, to)
    
    visNetwork::visNetwork(todos_nodos, enlaces) %>%
      visNetwork::visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE), nodesIdSelection = TRUE) %>%
      visNetwork::visPhysics(stabilization = FALSE)
  })
  
  # [BLINDAJE 1 Y 2 APLICADOS AL HISTORIAL DIN\u00c1MICO]
  # Extirpada la Bomba de Rendimiento: Ya no se lee 'reportes_cosecha/*.xlsx' en caliente
  # La base hist\u00f3rica bebe del SATICA_HISTORIAL precalculado recuperando la cardinalidad temporal total.
  base_historica_real <- reactive({
    
    # 1. Cargamos el mini-RDS (Milisegundos) que guarda cada incendio individual (Carga Híbrida)
    historial_crudo <- NULL
    if (file.exists("data_master/SATICA_HISTORIAL_v2.2.rds")) {
      historial_crudo <- tryCatch(readRDS("data_master/SATICA_HISTORIAL_v2.2.rds"), error = function(e) NULL)
    } else if (.ON_CLOUD) {
      historial_crudo <- tryCatch(readRDS(url("https://secasor.github.io/SATICA%20V2/data_master/SATICA_HISTORIAL_v2.2.rds")), error = function(e) NULL)
    }
    if (is.null(historial_crudo)) {
      historial_crudo <- data.frame(COD_UNICO_14 = character(), FECHA = as.Date(character()), stringsAsFactors = FALSE)
    }
    
    # 2. Rescatamos el mapa blindado para obtener Nombres y Municipios
    df_nombres <- DATOS_OFICIALES %>% 
      st_drop_geometry() %>% 
      select(COD_HDA_KEY, HDA_LABEL, INGENIO_FULL, MUNICIPIO) %>%
      distinct(COD_HDA_KEY, .keep_all = TRUE)
    
    # 3. Cruzamos y formateamos para inyecci\u00f3n r\u00e1pida en las gr\u00e1ficas
    df_final <- historial_crudo %>%
      mutate(
        ING = substr(COD_UNICO_14, 1, 2),
        HDA_PAD = substr(COD_UNICO_14, 3, 8),
        COD_HDA_KEY = paste(ING, HDA_PAD, sep = "_")
      ) %>%
      inner_join(df_nombres, by = "COD_HDA_KEY") %>%
      mutate(
        fecha_dt = as.Date(FECHA),
        hda_f = HDA_LABEL,
        ingenio_f = INGENIO_FULL,
        municipio_f = MUNICIPIO,
        ANIO = factor(year(fecha_dt), levels = 2019:as.numeric(format(Sys.Date(), "%Y"))), 
        MES_NUM = month(fecha_dt), 
        MES = factor(c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")[MES_NUM], levels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")),
        SEMANA = factor(week(fecha_dt), levels = 1:53)
      ) %>%
      select(fecha_dt, COD_HDA_KEY, ingenio_f, municipio_f, hda_f, ANIO, MES_NUM, MES, SEMANA)
    
    return(df_final)
  })
  
  output$ui_h_mun <- renderUI({
    req(input$h_ing)
    opciones <- if(input$h_ing == "TODOS") sort(unique(DATOS_OFICIALES$MUNICIPIO)) else sort(unique(DATOS_OFICIALES$MUNICIPIO[DATOS_OFICIALES$INGENIO_FULL == input$h_ing]))
    pickerInput("h_mun", "Municipio:", choices = c("TODOS", opciones), selected = "TODOS")
  })
  
  datos_h <- reactive({
    req(base_historica_real())
    df <- base_historica_real()
    if(!is.null(input$h_anio) && !any(toupper(input$h_anio) == "TODOS")) df <- df %>% filter(as.character(ANIO) %in% input$h_anio)
    if(!is.null(input$h_mes_num) && !any(toupper(input$h_mes_num) == "TODOS")) df <- df %>% filter(MES_NUM %in% as.numeric(input$h_mes_num))
    if(!is.null(input$h_ing) && toupper(input$h_ing) != "TODOS") df <- df %>% filter(toupper(ingenio_f) == toupper(input$h_ing))
    if(!is.null(input$h_mun) && toupper(input$h_mun) != "TODOS" && toupper(input$h_mun) != "N/A") df <- df %>% filter(toupper(municipio_f) == toupper(input$h_mun))
    return(df)
  })
  
  output$box_total_dar <- renderValueBox({
    req(datos_h())
    n_registros <- nrow(datos_h())
    n_predios   <- n_distinct(datos_h()$hda_f)
    valueBox(
      value    = formatC(n_registros, format="d", big.mark="."),
      subtitle = HTML(paste0(
        "<b>Total registros de incendio</b><br>",
        "<span style='font-size:12px;'>",
        formatC(n_predios, format="d", big.mark="."),
        " haciendas \u00fanicas afectadas</span>"
      )),
      icon  = icon("fire"),
      color = "navy"  # 'navy' es v\u00e1lido en bs4Dash
    )
  })

  # --- TABLA SIN GEORREFERENCIACI\u00d3N ---
  output$tabla_sin_georref <- renderDT(server = FALSE, {
    req(SIN_GEORREF)
    n_total <- nrow(SIN_GEORREF)
    SIN_GEORREF %>%
      select(COD_HDA_8, Nombre_Reporte, Ingenio, Municipio_Excel,
             Correg_Excel, n_registros, Prioridad, RIESGO) %>%
      datatable(
        rownames   = FALSE,
        caption    = htmltools::tags$caption(
          style = "caption-side: top; text-align: left; font-size: 14px; font-weight: bold; color:#c0392b; padding: 6px 0;",
          paste0("\u26a0 Total haciendas sin georreferenciaci\u00f3n: ", n_total,
                 " \u2014 Los botones exportan la totalidad del registro, no solo la p\u00e1gina visible.")
        ),
        extensions = 'Buttons',
        filter     = 'top',
        options    = list(
          pageLength = 25,
          lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'Todos')),
          dom        = 'Blfrtip',
          buttons    = list(
            list(
              extend  = 'csv',
              text    = '\U0001F4E5 Descargar CSV',
              filename = paste0('SinGeorref_SATICA_', Sys.Date()),
              exportOptions = list(
                modifier = list(page = 'all')
              )
            ),
            list(
              extend  = 'excel',
              text    = '\U0001F4CA Descargar Excel',
              filename = paste0('SinGeorref_SATICA_', Sys.Date()),
              title   = paste0('SATICA \u2014 Sin Georreferenciaci\u00f3n \u2014 ', Sys.Date()),
              exportOptions = list(
                modifier = list(page = 'all')
              )
            ),
            list(
              extend  = 'print',
              text    = '\U0001F5A8 Imprimir Todo',
              title   = paste0('SATICA \u2014 Sin Georreferenciaci\u00f3n \u2014 ', Sys.Date()),
              exportOptions = list(
                modifier = list(page = 'all')
              )
            )
          )
        )
      )
  })

  # --- DESCARGA EXCEL SEGUIMIENTO ---
  output$descargar_excel <- downloadHandler(
    filename = function() { paste0("Seguimiento_Boletin_SATICA_", Sys.Date(), ".xlsx") },
    content  = function(file) {
      df_base <- datos_r() %>% st_drop_geometry() %>%
        mutate(
          MUN_VAL = MUNICIPIO,
          HDA_NOM = HDA_LABEL,
          CORREG_VAL = CORREGIMIENTO,
          INGENIO_VAL = INGENIO_FULL,
          TOTAL_HISTORICO = ifelse(is.na(N_EVENTOS), 0, as.numeric(N_EVENTOS)),
          TXT_ULTIMO_INCENDIO = ifelse(!is.na(FECHA_ULT_I), as.character(FECHA_ULT_I), "Sin Eventos"),
          TXT_AUDITORIA = case_when(
            ESTADO_CONTROL == "Visitado" & !is.na(RADICADO) & RADICADO != "S/N" ~ paste("Visitado (Rad:", RADICADO, ")"),
            TRUE ~ ESTADO_CONTROL
          ),
          PESO_RIESGO = case_when(RIESGO == "CRITICO" ~ 1, RIESGO == "ALTO" ~ 2, RIESGO == "OBSERVACION" ~ 3, TRUE ~ 4)
        )
      
      # Pestana 1: Cronograma Inminentes (+- 15 dias)
      datos_alerta <- df_base %>% filter(RIESGO %in% c("CRITICO", "ALTO")) %>% arrange(PESO_RIESGO, desc(DIFF_MESES))
      datos_quincena <- datos_alerta %>%
        mutate(
          FECH_EST = as.Date(FECHA_ULT_I) + CICLO_DIAS,
          DIAS_FALTANTES = as.numeric(FECH_EST - Sys.Date()),
          TXT_ESTIMADO = ifelse(!is.na(FECH_EST), as.character(FECH_EST), "N/A")
        ) %>%
        filter(DIAS_FALTANTES >= -15 & DIAS_FALTANTES <= 15) %>%
        arrange(MUN_VAL) %>%
        select(Municipio = MUN_VAL, Hacienda = HDA_NOM, Corregimiento = CORREG_VAL, Ingenio = INGENIO_VAL, `Ultimo Evento` = TXT_ULTIMO_INCENDIO, `Ciclo Estimado` = TXT_ESTIMADO, `Historico` = TOTAL_HISTORICO, `Gestion CVC` = TXT_AUDITORIA) %>%
        mutate(`Funcionario Responsable` = "", Radicado = "")

      # Pestana 2: Focalizacion Top 10 Municipio
      top_municipios <- df_base %>%
        group_by(MUN_VAL) %>%
        slice_max(order_by = TOTAL_HISTORICO, n = 10, with_ties = FALSE) %>%
        arrange(MUN_VAL, desc(TOTAL_HISTORICO)) %>%
        ungroup() %>%
        select(Municipio = MUN_VAL, Hacienda = HDA_NOM, Corregimiento = CORREG_VAL, Ingenio = INGENIO_VAL, Riesgo = RIESGO, `Ultimo Evento` = TXT_ULTIMO_INCENDIO, Historico = TOTAL_HISTORICO, `Gestion CVC` = TXT_AUDITORIA) %>%
        mutate(`Funcionario Responsable` = "", Radicado = "")

      # Pestana 3: Ficha Tecnica Regional
      ficha_maestra <- df_base %>%
        slice_max(order_by = TOTAL_HISTORICO, n = 10, with_ties = FALSE) %>%
        arrange(desc(TOTAL_HISTORICO)) %>%
        select(Municipio = MUN_VAL, Hacienda = HDA_NOM, Corregimiento = CORREG_VAL, Ingenio = INGENIO_VAL, Nivel = RIESGO, `Ultimo Evento` = TXT_ULTIMO_INCENDIO, `Historico` = TOTAL_HISTORICO, `Gestion CVC` = TXT_AUDITORIA) %>%
        mutate(`Funcionario Responsable` = "", Radicado = "")

      # Generar y escribir multiples hojas
      openxlsx::write.xlsx(list(
        "Cronograma Inminentes" = datos_quincena,
        "Top 10 Municipio" = top_municipios,
        "Ficha Tecnica DAR" = ficha_maestra
      ), file)
    }
  )
  
  grafico_tendencia <- reactive({
    req(datos_h())
    if(nrow(datos_h()) == 0) return(ggplot() + theme_void() + geom_text(aes(x = 1, y = 1, label = "Sin registros para los filtros actuales"), size = 6, color="gray"))
    df_plot <- datos_h() %>% group_by(GRUPO = !!sym(input$h_agrupa)) %>% summarise(TOTAL = n(), .groups = "drop")
    ggplot(df_plot, aes(x = GRUPO, y = TOTAL, fill = GRUPO)) +
      geom_col(color = "black", alpha = 0.8) + geom_text(aes(label = ifelse(TOTAL > 0, TOTAL, "")), vjust = -0.5, size = 5, fontface = "bold") +
      scale_fill_viridis_d(drop = FALSE) + scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + scale_x_discrete(drop = FALSE) + 
      labs(x = "Temporalidad", y = "Haciendas Incidentes") + theme_minimal() +
      theme(legend.position = "none", axis.text.x = element_text(size = 12, angle = 45, hjust = 1, face = "bold"), axis.title = element_text(size = 14, face = "bold"), plot.background = element_rect(fill = "white", color = NA), panel.background = element_rect(fill = "white", color = NA))
  })
  
  output$plot_historia <- renderPlot({ grafico_tendencia() })
  output$dl_plot_historia <- downloadHandler(filename = function() { paste0("SATICA_Tendencia_", Sys.Date(), ".png") }, content = function(file) { ggsave(file, plot = grafico_tendencia(), width = 10, height = 6, dpi = 300, bg = "white") })
  
  grafico_top10 <- reactive({
    req(datos_h())
    if(nrow(datos_h()) == 0) return(ggplot() + theme_void())
    df_top <- datos_h() %>% group_by(hda_f, ingenio_f) %>% summarise(INCENDIOS = n(), .groups = "drop") %>% arrange(desc(INCENDIOS)) %>% slice_head(n = 10)
    ggplot(df_top, aes(x = reorder(hda_f, INCENDIOS), y = INCENDIOS, fill = INCENDIOS)) +
      geom_col(color = "black") + coord_flip() + geom_text(aes(label = INCENDIOS), hjust = -0.2, size = 5, fontface = "bold") +
      scale_fill_gradient(low = "#f39c12", high = "#c0392b") + scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = "Hacienda Afectada", y = "Historial Registrado", title = "") + theme_minimal() +
      theme(legend.position = "none", axis.text.y = element_text(size = 12, face = "bold"), axis.text.x = element_text(size = 12), axis.title = element_text(size = 14), plot.background = element_rect(fill = "white", color = NA), panel.background = element_rect(fill = "white", color = NA))
  })
  
  output$plot_top10 <- renderPlot({ grafico_top10() })
  output$dl_plot_top10 <- downloadHandler(filename = function() { paste0("SATICA_Top10_", Sys.Date(), ".png") }, content = function(file) { ggsave(file, plot = grafico_top10(), width = 10, height = 6, dpi = 300, bg = "white") })
  output$descargar_historia_csv <- downloadHandler(filename = function() { paste0("Auditoria_Historica_SATICA_", Sys.Date(), ".csv") }, content = function(file) { write.csv(datos_h(), file, row.names = FALSE) })
  
  # --- OBSERVER PARA ACTUALIZACIÓN MANUAL ---
  observeEvent(input$btn_actualizar, {
    showModal(modalDialog(
      title = "Actualizando Telemetría",
      div(style = "text-align: center; padding: 20px;",
          icon("sync", class = "fa-spin fa-3x", style = "color: #c0392b; margin-bottom: 15px;"),
          p("Descargando focos de calor recientes de la NASA e iniciando predicciones del motor..."),
          p("Por favor espere. Este proceso puede tardar unos segundos.")),
      footer = NULL,
      easyClose = FALSE
    ))
    
    tryCatch({
      # Ejecutar scripts de actualización (nasa firms y engine)
      if (file.exists("R/api_nasa_firms.R")) {
        source("R/api_nasa_firms.R", local = TRUE, encoding = "UTF-8")
      }
      if (file.exists("satica_engine.R")) {
        source("satica_engine.R", local = TRUE, encoding = "UTF-8")
      }
      
      # Recargar master_rds localmente
      master_rds <- readRDS("data_master/SATICA_MASTER_v2.2.rds")
      
      formatear_tiempo_g <- function(dias) {
        if (is.na(dias) || dias == 0) return("Sin Historial")
        if (dias < 60) return(paste(round(dias), "días"))
        meses <- floor(dias / 30.44); sobra <- round(dias %% 30.44)
        txt_m <- ifelse(meses == 1, "1 mes", paste(meses, "meses"))
        txt_d <- ifelse(sobra > 0, paste(sobra, "días"), "")
        return(trimws(paste(txt_m, txt_d)))
      }
      
      DB_RIESGO <- master_rds %>%
        group_by(cod_hda_key) %>%
        summarise(
          TOTAL_HISTORICO = max(N_EVENTOS_HDA,      na.rm = TRUE),
          N_EVENTOS       = max(N_EVENTOS_HDA,      na.rm = TRUE),
          FECHA_ULT_I     = max(FECHA_ULT_I_HDA,    na.rm = TRUE),
          CICLO_DIAS      = mean(FRECUENCIA_HDA_DIAS,na.rm = TRUE),
          TXT_CICLO       = sapply(mean(FRECUENCIA_HDA_DIAS, na.rm = TRUE), formatear_tiempo_g),
          DIFF_MESES      = min(DIFF_HDA_MESES,      na.rm = TRUE),
          RIESGO = { idx <- which.min(DIFF_HDA_MESES); if (length(idx) > 0) riesgo[idx] else NA_character_ },
          COL    = { idx <- which.min(DIFF_HDA_MESES); if (length(idx) > 0) col[idx]    else NA_character_ },
          FECHA_ESTIMADA  = as.Date(max(FECHA_ULT_I_HDA, na.rm = TRUE) +
                                      mean(FRECUENCIA_HDA_DIAS, na.rm = TRUE)),
          SAT_FUEGO       = if("Satelite_Fuego" %in% names(.)) any(Satelite_Fuego == TRUE, na.rm = TRUE) else FALSE,
          GOES_FUEGO      = if("GOES_Fuego" %in% names(.)) any(GOES_Fuego == TRUE, na.rm = TRUE) else FALSE,
          ESTADO_GOES     = if("Estado_GOES" %in% names(.)) first(na.omit(Estado_GOES)) else "Normal",
          MAX_NDVI        = if("NDVI" %in% names(.)) max(NDVI, na.rm = TRUE) else 0.5,
          ESTADO_BIOMASA  = if("Alerta_Combustion" %in% names(.)) first(na.omit(Alerta_Combustion)) else "Sin Datos",
          .groups = "drop"
        ) %>%
        mutate(
          FECHA_ULT_I = if_else(is.infinite(FECHA_ULT_I), as.Date(NA), as.Date(FECHA_ULT_I)),
          RIESGO      = coalesce(RIESGO, "BAJO"),
          COL         = coalesce(COL,    "#7f8c8d"),
          COD_HDA_KEY = cod_hda_key
        )
      
      # Reconstruir DATOS_OFICIALES
      DATOS_OFICIALES <<- DATOS_ESPACIALES_BASE %>%
        left_join(DB_RIESGO, by = "COD_HDA_KEY") %>%
        mutate(
          TOTAL_HISTORICO = coalesce(TOTAL_HISTORICO, 0),
          N_EVENTOS = coalesce(N_EVENTOS, 0),
          FECHA_ULT_I = as.Date(FECHA_ULT_I, origin = "1970-01-01"),
          SAT_FUEGO   = coalesce(SAT_FUEGO, FALSE),
          GOES_FUEGO  = coalesce(GOES_FUEGO, FALSE),
          ESTADO_GOES = coalesce(ESTADO_GOES, "Normal"),
          ESTADO_BIOMASA = coalesce(ESTADO_BIOMASA, "Sin Datos")
        )
      
      # Invalidar trigger reactivo para recargar todo
      update_trigger(update_trigger() + 1)
      
      removeModal()
      showNotification("¡Telemetría y predicciones actualizadas exitosamente!", type = "message")
    }, error = function(e) {
      removeModal()
      showNotification(paste("Error en la actualización:", e$message), type = "error")
    })
  })
}