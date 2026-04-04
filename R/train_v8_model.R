# ----------------------------------------------------
# Archivo: R/train_v8_model.R - Entrenamiento Inteligente V8
# ----------------------------------------------------

library(dplyr)
library(lubridate)
library(xgboost)
library(Matrix)
library(tidyr)

# 1. Cargar dependencias
source("aux_functions.R", encoding = "UTF-8")
source("R/data_prep.R", encoding = "UTF-8")

message("🔄 Preparando Pipeline de Datos V8.1 (Inyectando Municipio Geográfico)...")
df_target <- leer_y_preparar_datos("reportes_cosecha")

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
    Recurrencia = as.numeric(recurrencia_incendios_acumulada)
  ) %>%
  tidyr::drop_na(Mes, Ingenio, Corregimiento, Recurrencia) %>%
  tidyr::replace_na(list(Municipio = "UNKN")) %>%
  mutate(Municipio = as.factor(Municipio))

message(sprintf("   -> Muestras válidas de entrenamiento: %d", nrow(df_features)))

if(nrow(df_features) == 0){
  stop("El proceso de limpieza devolvió 0 filas. Revisa data_prep.R.")
}

# 2. Ingeniería Matricial (One-Hot Encoding automático a través de dgCMatrix)
# Generamos la matriz exparsa eliminando el término de intercepción (-1)
dummy_matrix <- sparse.model.matrix(
  dias_hasta_siguiente_incendio ~ Ingenio + Corregimiento + Municipio + Recurrencia + Mes - 1, 
  data = df_features
)
labels <- df_features$dias_hasta_siguiente_incendio

# 3. Entrenamiento (Train / Test Split)
set.seed(2026)
train_idx <- sample(seq_len(nrow(dummy_matrix)), size = 0.8 * nrow(dummy_matrix))

dtrain <- xgb.DMatrix(data = dummy_matrix[train_idx, ], label = labels[train_idx])
dtest <- xgb.DMatrix(data = dummy_matrix[-train_idx, ], label = labels[-train_idx])

message("🤖 Entrenando Modelo V8 Regresión Días (Optimizando RMSE)...")
params <- list(
  objective = "reg:squarederror",
  eta = 0.05,
  max_depth = 6,
  eval_metric = "rmse"
)

xgb_model_V8 <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 200,
  watchlist = list(train = dtrain, test = dtest),
  print_every_n = 50,
  early_stopping_rounds = 10
)

# 4. Evaluación base (Rápida validación en consola)
preds <- predict(xgb_model_V8, dtest)
mae <- mean(abs(labels[-train_idx] - preds))
rmse <- sqrt(mean((labels[-train_idx] - preds)^2))
message(sprintf("✅ Entrenamiento Completo V8."))
message(sprintf("   -> MAE en Datos Reales (Test): %.1f días de error promedio", mae))
message(sprintf("   -> RMSE Global: %.1f días", rmse))

# 5. Exportación Segura del Cerebro y el Diccionario
if(!dir.exists("modelo_rds")) dir.create("modelo_rds")

# Persistir modelo predictivo
saveRDS(xgb_model_V8, "modelo_rds/xgb_model_V8_regresion_dias.rds")

# ¡CRÍTICO! Persistir el "ADN" de las columnas 
# Esto evitará la amnesia ocurrida en V7 para futuras predicciones en vivo.
diccionario_columnas <- colnames(dummy_matrix)
saveRDS(diccionario_columnas, "modelo_rds/v8_features.rds")

# También guardamos el objeto dummyVars puramente si hiciera falta.
message("🚀 Módulos Exportados: Modelo (V8) y su diccionario matricial almacenados en `modelo_rds`.")
