# ----------------------------------------------------
# Archivo: R/train_v9_model.R - Entrenamiento Inteligente V9 (Inteligencia Geoespacial)
# ----------------------------------------------------

library(dplyr)
library(lubridate)
library(xgboost)
library(Matrix)
library(tidyr)
library(stringr)

# 1. Cargar dependencias
source("aux_functions.R", encoding = "UTF-8")
source("R/data_prep.R", encoding = "UTF-8")

message("🔄 Preparando Pipeline de Datos V9 (Inyectando Distancias Ambientales y Antrópicas)...")
df_target <- leer_y_preparar_datos("reportes_cosecha")

# =========================================================================
# CROSS-JOIN GEOESPACIAL: Inyectar Distancias Físicas desde master_data
# =========================================================================
matriz_distancias <- readRDS("data_master/matriz_distancias_cana.rds")

# Reconstruir COD_UNICO_14 (padded) para que el join con el Shapefile sea perfecto
df_target <- df_target %>%
  mutate(
    ing_clean = substr(COD_UNICO, 1, 2),
    hda_pad = stringr::str_pad(gsub("[^0-9A-Za-z]", "", cod_hacienda), 6, pad = "0"),
    ste_pad = stringr::str_pad(gsub("[^0-9A-Za-z]", "", cod_suerte), 6, pad = "0"),
    cod_unico_14 = paste0(ing_clean, hda_pad, ste_pad)
  ) %>%
  left_join(matriz_distancias, by = c("cod_unico_14" = "cod_unico"))

# Imputar valores extremos (por defecto 15km) si falló el join por un código huérfano
df_target <- df_target %>%
  tidyr::replace_na(list(dist_vias_m = 15000, dist_poblados_m = 15000, dist_bosques_m = 15000))

# Ingestar Municipio desde diccionario de Corregimientos
correg_shp <- sf::st_read("capas/Corregimientos.shp", quiet = TRUE) %>% sf::st_transform(4326) %>% janitor::clean_names()

correg_diccionario <- correg_shp %>% 
  sf::st_drop_geometry() %>% 
  select(nom_div_po, nom_munici) %>% 
  mutate(feature_A_upper = toupper(stringi::stri_trans_general(nom_div_po, "Latin-ASCII"))) %>%
  group_by(feature_A_upper) %>%
  dplyr::slice(1) %>%
  ungroup() %>%
  distinct()

df_target <- df_target %>% 
  mutate(feature_A_upper = toupper(stringi::stri_trans_general(feature_A, "Latin-ASCII"))) %>%
  left_join(correg_diccionario, by = "feature_A_upper")



# Seleccionar predictores básicos e importantes autorizados en el plan de implementación
df_features <- df_target %>%
  filter(!is.na(dias_hasta_siguiente_incendio)) %>%
  mutate(
    Mes = as.numeric(month(fecha_dato)),
    Ingenio = as.factor(nombre_ingenio_completo),
    Corregimiento = as.factor(feature_A),
    Municipio = as.character(nom_munici),
    Recurrencia = as.numeric(recurrencia_incendios_acumulada),
    Dist_Vias = as.numeric(dist_vias_m),
    Dist_Pueblos = as.numeric(dist_poblados_m),
    Dist_Bosques = as.numeric(dist_bosques_m)
  ) %>%
  tidyr::drop_na(Mes, Ingenio, Corregimiento, Recurrencia) %>%
  tidyr::replace_na(list(Municipio = "UNKN")) %>%
  mutate(Municipio = as.factor(Municipio))

message(sprintf("   -> Muestras válidas de entrenamiento V9: %d", nrow(df_features)))

if(nrow(df_features) == 0){
  stop("El proceso de limpieza devolvió 0 filas. Revisa data_prep.R.")
}

# 2. Ingeniería Matricial (One-Hot Encoding automático a través de dgCMatrix)
# Generamos la matriz exparsa eliminando el término de intercepción (-1)
dummy_matrix <- sparse.model.matrix(
  dias_hasta_siguiente_incendio ~ Ingenio + Corregimiento + Municipio + Recurrencia + Mes + Dist_Vias + Dist_Pueblos + Dist_Bosques - 1, 
  data = df_features
)
labels <- df_features$dias_hasta_siguiente_incendio

# 3. Entrenamiento (Train / Test Split)
set.seed(2026)
train_idx <- sample(seq_len(nrow(dummy_matrix)), size = 0.8 * nrow(dummy_matrix))

dtrain <- xgb.DMatrix(data = dummy_matrix[train_idx, ], label = labels[train_idx])
dtest <- xgb.DMatrix(data = dummy_matrix[-train_idx, ], label = labels[-train_idx])

message("🤖 Entrenando Modelo V9 Geoespacial (Buscando optimizar el MAE base de 203)...")
# Aumentamos agresividad y profundidad para capturar el conocimiento de las 3 distancias nuevas
params <- list(
  objective = "reg:squarederror",
  eta = 0.03,
  max_depth = 8,
  eval_metric = "rmse"
)

xgb_model_V9 <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 350,
  watchlist = list(train = dtrain, test = dtest),
  print_every_n = 50,
  early_stopping_rounds = 15
)

# 4. Evaluación base (Rápida validación en consola)
preds <- predict(xgb_model_V9, dtest)
mae <- mean(abs(labels[-train_idx] - preds))
rmse <- sqrt(mean((labels[-train_idx] - preds)^2))
message(sprintf("✅ Entrenamiento Completo V9."))
message(sprintf("   -> NUEVO MAE V9: %.1f días de error promedio", mae))
message(sprintf("   -> RMSE Global: %.1f días", rmse))

# 5. Exportación Segura del Cerebro y el Diccionario
if(!dir.exists("modelo_rds")) dir.create("modelo_rds")

# Persistir modelo predictivo
xgb.save(xgb_model_V9, "modelo_rds/xgb_model_V9_regresion_geoespacial.ubj")

# ¡CRÍTICO! Persistir el "ADN" de las columnas 
diccionario_columnas <- colnames(dummy_matrix)
saveRDS(diccionario_columnas, "modelo_rds/v9_features_geoespacial.rds")

message("🚀 Módulos Exportados: Modelo (V9) y su Genoma Espacial almacenados en `modelo_rds`.")
