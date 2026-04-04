# aux_functions.R (VERSIÓN CORREGIDA)
library(dplyr)
library(stringr)

# Normaliza texto (acentos, mayúsculas, espacios)
normalize_name <- function(texto) {
  texto <- as.character(texto)
  texto[is.na(texto)] <- ""
  texto <- iconv(texto, from = "UTF-8", to = "ASCII//TRANSLIT")
  texto <- toupper(texto)
  texto <- trimws(gsub("\\s+", " ", texto))
  texto <- ifelse(texto == "", NA_character_, texto)
  return(texto)
}

# Normaliza códigos (solo A-Z0-9)
normalize_code <- function(codigo) {
  codigo <- as.character(codigo)
  codigo[is.na(codigo)] <- ""
  codigo <- toupper(codigo)
  codigo <- gsub("[^A-Z0-9]", "", codigo)
  codigo <- ifelse(codigo == "", NA_character_, codigo)
  return(codigo)
}

# Mapeo de ingenios (mantener actualizado)
MAPEO_INGENIOS <- tibble::tribble(
  ~nombre_ingenio_completo, ~abreviatura,
  "PROVIDENCIA", "PR",
  "MANUELITA", "MN",
  "INCAUCA", "IN",
  "MAYAGUEZ", "MY",
  "PICHICHI", "PI",
  "RISARALDA", "RI",
  "CARMELITA", "CM",
  "MARIA LUISA", "ML",
  "SANCARLOS", "SC",
  "SAN CARLOS", "SC",
  "TUMACO", "TU",
  "OCCIDENTE", "OC",
  "LA CABANA", "CB",
  "CABANA", "CB",
  "CASTILLA", "CC",
  "RIOPAILA", "RP"
) %>%
  mutate(nombre_ingenio_completo = normalize_name(nombre_ingenio_completo))

# Vectorized translate_ingenio: devuelve abreviatura o NA y registra si faltan
translate_ingenio <- function(nombres_vector) {
  nombres_norm <- normalize_name(nombres_vector)
  df_map <- tibble::tibble(nombre_ingenio_completo = nombres_norm) %>%
    left_join(MAPEO_INGENIOS, by = "nombre_ingenio_completo")
  # Produces NA where not matched
  missing <- which(is.na(df_map$abreviatura) & !is.na(df_map$nombre_ingenio_completo))
  if(length(missing) > 0) {
    warning("translate_ingenio: Ingenios no mapeados (usar 'UNKN' o ampliar MAPEO_INGENIOS): ",
            paste(unique(df_map$nombre_ingenio_completo[missing]), collapse = ", "))
    df_map$abreviatura[missing] <- "UNKN"
  }
  df_map$abreviatura[is.na(df_map$abreviatura)] <- "UNKN"
  return(df_map$abreviatura)
}

# create_cod_unico: vectorizado, admite nombres comunes
create_cod_unico <- function(df,
                             col_ing = c("ING", "nombre_ingenio_completo", "RAW_ING"),
                             col_hda = c("NOMBRE_HDA", "HACIENDA", "cod_hacienda", "RAW_HDA"),
                             col_ste = c("STE", "SUERTE", "cod_suerte", "RAW_STE")) {
  # Detect columns present
  cols <- names(df)
  c_ing <- intersect(col_ing, cols)[1]
  c_hda <- intersect(col_hda, cols)[1]
  c_ste <- intersect(col_ste, cols)[1]
  if(is.null(c_ing) || is.null(c_hda) || is.null(c_ste)) {
    stop("create_cod_unico: faltan columnas requeridas. Se requieren columnas de Ingenio, Hacienda y Suerte.")
  }
  df2 <- df %>%
    mutate(
      tmp_ing = normalize_name(.data[[c_ing]]),
      tmp_hda = normalize_code(.data[[c_hda]]),
      tmp_ste = normalize_code(.data[[c_ste]])
    )
  # Obtener abreviaturas vectorizadas
  df2 <- df2 %>%
    mutate(abv = translate_ingenio(tmp_ing))
  # Construir COD_UNICO y blindear
  df2 <- df2 %>%
    mutate(
      COD_UNICO = toupper(gsub("[^[:alnum:]]", "", paste0(abv, tmp_hda, tmp_ste))),
      COD_UNICO = ifelse(is.na(COD_UNICO) | COD_UNICO == "", NA_character_, COD_UNICO)
    ) %>%
    select(-tmp_ing, -tmp_hda, -tmp_ste, -abv)
  return(df2)
}
