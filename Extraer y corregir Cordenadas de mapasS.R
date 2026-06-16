# ============================================================
# 0. Librerías
# ============================================================

library(dplyr)
library(stringr)
library(readr)
library(purrr)
library(tibble)
library(readxl)
library(janitor)
library(writexl)


# ============================================================
# 1. Parámetros
# ============================================================

ruta_entrada <- "C:/Users/brittany.pereo/Downloads/CASAS_SALUD.xlsx"
hoja_entrada <- "Sin clues"
out_dir <- "C:/Users/brittany.pereo/Downloads"

lat_min <- 14
lat_max <- 33
lon_min <- -119
lon_max <- -86


# ============================================================
# 2. Funciones
# ============================================================

es_numerico <- function(x) {
  !is.na(suppressWarnings(as.numeric(x)))
}

dms_a_decimal <- function(x) {
  
  x <- str_squish(as.character(x))
  
  patron <- "(\\d+(?:\\.\\d+)?)°\\s*(\\d+(?:\\.\\d+)?)?'?\\s*(\\d+(?:\\.\\d+)?)?\"?\\s*([NSEW])?"
  m <- str_match(x, patron)
  
  if (is.na(m[1, 1])) return(NA_real_)
  
  grados   <- as.numeric(m[1, 2])
  minutos  <- as.numeric(m[1, 3])
  segundos <- as.numeric(m[1, 4])
  dir      <- m[1, 5]
  
  minutos  <- coalesce(minutos, 0)
  segundos <- coalesce(segundos, 0)
  
  decimal <- grados + minutos / 60 + segundos / 3600
  
  if (!is.na(dir) && dir %in% c("S", "W")) {
    decimal <- -decimal
  }
  
  decimal
}

extraer_coord <- function(latitud, longitud) {
  
  lat_txt <- str_squish(as.character(latitud))
  lon_txt <- str_squish(as.character(longitud))
  
  par_lon <- str_match(
    lon_txt,
    "^(-?\\d+(?:\\.\\d+)?)\\s*,\\s*(-?\\d+(?:\\.\\d+)?)$"
  )
  
  par_dms_lat <- str_match(
    lat_txt,
    "(.+?[NS])\\s+(.+?[EW])"
  )
  
  lat_num <- case_when(
    !is.na(par_lon[, 1]) ~ as.numeric(par_lon[, 2]),
    !is.na(par_dms_lat[, 1]) ~ map_dbl(par_dms_lat[, 2], dms_a_decimal),
    str_detect(lat_txt, "°|'|\"|[NSEW]") ~ map_dbl(lat_txt, dms_a_decimal),
    TRUE ~ parse_number(lat_txt)
  )
  
  lon_num <- case_when(
    !is.na(par_lon[, 1]) ~ as.numeric(par_lon[, 3]),
    !is.na(par_dms_lat[, 1]) ~ map_dbl(par_dms_lat[, 3], dms_a_decimal),
    str_detect(lon_txt, "°|'|\"|[NSEW]") ~ map_dbl(lon_txt, dms_a_decimal),
    TRUE ~ parse_number(lon_txt)
  )
  
  tibble(
    latitud_num = lat_num,
    longitud_num = lon_num
  )
}

coord_valida <- function(lat, lon) {
  between(lat, lat_min, lat_max) & between(lon, lon_min, lon_max)
}

causa_perdida <- function(lat, lon) {
  case_when(
    is.na(lat) & is.na(lon) ~ "Latitud y longitud no convertidas",
    is.na(lat) ~ "Latitud no convertida",
    is.na(lon) ~ "Longitud no convertida",
    !between(lat, lat_min, lat_max) & !between(lon, lon_min, lon_max) ~ "Latitud y longitud fuera de rango México",
    !between(lat, lat_min, lat_max) ~ "Latitud fuera de rango México",
    !between(lon, lon_min, lon_max) ~ "Longitud fuera de rango México",
    TRUE ~ "Sin causa identificada"
  )
}


# ============================================================
# 3. Leer base original con ID
# ============================================================

df <- read_xlsx(
  ruta_entrada,
  sheet = hoja_entrada
) %>% 
  clean_names() %>% 
  mutate(id_registro = row_number())


# ============================================================
# 4. Separar numéricos y texto
# ============================================================

df_numerico <- df %>% 
  filter(
    es_numerico(latitud),
    es_numerico(longitud)
  ) %>% 
  mutate(
    latitud_original = latitud,
    longitud_original = longitud,
    latitud = as.numeric(latitud),
    longitud = as.numeric(longitud),
    origen_coord = "numérico"
  )

df_texto <- df %>% 
  filter(
    !(es_numerico(latitud) & es_numerico(longitud))
  ) %>% 
  mutate(
    latitud_original = latitud,
    longitud_original = longitud,
    origen_coord = "texto"
  )


# ============================================================
# 5. Limpieza de numéricos
# ============================================================

df_numerico_limpio <- df_numerico %>% 
  mutate(
    longitud = ifelse(longitud > 0, -longitud, longitud),
    
    lat_tmp = case_when(
      between(latitud, lon_min, lon_max) & between(longitud, lon_min, lon_max) ~ NA_real_,
      between(latitud, lon_min, lon_max) ~ longitud,
      TRUE ~ latitud
    ),
    
    lon_tmp = case_when(
      between(latitud, lon_min, lon_max) & between(longitud, lon_min, lon_max) ~ NA_real_,
      between(latitud, lon_min, lon_max) ~ latitud,
      TRUE ~ longitud
    ),
    
    latitud = lat_tmp,
    longitud = lon_tmp
  ) %>% 
  select(-lat_tmp, -lon_tmp) %>% 
  mutate(
    longitud = case_when(
      latitud == 24.15034 & round(longitud, 5) == -10.56551 ~ -110.56551,
      latitud == 15.60042 & longitud < -100000 ~ -93.1443414,
      TRUE ~ longitud
    ),
    latitud_num = latitud,
    longitud_num = longitud,
    coordenada_valida = coord_valida(latitud_num, longitud_num),
    causa_revision = causa_perdida(latitud_num, longitud_num)
  )

df_numerico_mapa <- df_numerico_limpio %>% 
  filter(coordenada_valida)

df_numerico_revision <- df_numerico_limpio %>% 
  filter(!coordenada_valida)


# ============================================================
# 6. Limpieza de texto
# ============================================================

df_texto_convertido <- df_texto %>% 
  bind_cols(
    extraer_coord(.$latitud, .$longitud)
  ) %>% 
  mutate(
    latitud_num = case_when(
      longitud == "-92.4453267,13.92" ~ 13.92,
      latitud == "164403" ~ 16.4403,
      latitud == "165143" ~ 16.5143,
      latitud == "14°47'" ~ 14 + 47 / 60,
      latitud == "15.63065°" ~ 15.63065,
      latitud == "16.359982299999999" ~ 16.3599823,
      TRUE ~ latitud_num
    ),
    
    longitud_num = case_when(
      longitud == "110..26522" ~ -110.26522,
      longitud == "930138 1" ~ -93.01381,
      longitud == "925626 2" ~ -92.56262,
      longitud == "-92.02061°" ~ -92.02061,
      longitud == "-92.34611°" ~ -92.34611,
      longitud == "-92.4453267,13.92" ~ -92.4453267,
      TRUE ~ longitud_num
    ),
    coordenada_valida = coord_valida(latitud_num, longitud_num),
    causa_revision = causa_perdida(latitud_num, longitud_num)
  )

df_texto_mapa <- df_texto_convertido %>% 
  filter(coordenada_valida)

df_texto_revision <- df_texto_convertido %>% 
  filter(!coordenada_valida)

# ============================================================
# 7. Base final
# ============================================================

df_final <- bind_rows(
  
  df_numerico_mapa %>% 
    transmute(
      id_registro,
      entidad,
      municipio,
      localidad,
      tipo_de_asentamiento,
      codigo_postal,
      insertar_liga_de_georreferencia_google_maps,
      numero_de_consultorios,
      origen_coord,
      latitud_original = as.character(latitud_original),
      longitud_original = as.character(longitud_original),
      latitud = latitud_num,
      longitud = longitud_num
    ),
  
  df_texto_mapa %>% 
    transmute(
      id_registro,
      entidad,
      municipio,
      localidad,
      tipo_de_asentamiento,
      codigo_postal,
      insertar_liga_de_georreferencia_google_maps,
      numero_de_consultorios,
      origen_coord,
      latitud_original = as.character(latitud_original),
      longitud_original = as.character(longitud_original),
      latitud = latitud_num,
      longitud = longitud_num
    )
)

df_final_corregido <- df_final %>% 
  mutate(
    lat_tmp = case_when(
      between(latitud, lon_min, lon_max) & between(longitud, lat_min, lat_max) ~ longitud,
      TRUE ~ latitud
    ),
    lon_tmp = case_when(
      between(latitud, lon_min, lon_max) & between(longitud, lat_min, lat_max) ~ latitud,
      TRUE ~ longitud
    ),
    latitud = lat_tmp,
    longitud = lon_tmp,
    coordenada_valida_final = coord_valida(latitud, longitud),
    causa_revision_final = causa_perdida(latitud, longitud)
  ) %>% 
  select(-lat_tmp, -lon_tmp)

df_final_mapa <- df_final_corregido %>% 
  filter(coordenada_valida_final)

df_final_revision <- df_final_corregido %>% 
  filter(!coordenada_valida_final)

# ============================================================
# 8. Validar cuáles se perdieron y por qué
# ============================================================

perdidos <- df %>% 
  anti_join(
    df_final_mapa %>% select(id_registro),
    by = "id_registro"
  )

revision_total <- bind_rows(
  
  df_numerico_revision %>% 
    transmute(
      id_registro,
      origen_coord,
      latitud_original = as.character(latitud_original),
      longitud_original = as.character(longitud_original),
      latitud_num,
      longitud_num,
      causa_revision
    ),
  
  df_texto_revision %>% 
    transmute(
      id_registro,
      origen_coord,
      latitud_original = as.character(latitud_original),
      longitud_original = as.character(longitud_original),
      latitud_num,
      longitud_num,
      causa_revision
    ),
  
  df_final_revision %>% 
    transmute(
      id_registro,
      origen_coord,
      latitud_original = as.character(latitud_original),
      longitud_original = as.character(longitud_original),
      latitud_num = latitud,
      longitud_num = longitud,
      causa_revision = causa_revision_final
    )
) %>% 
  distinct(id_registro, .keep_all = TRUE)

perdidos_con_causa <- perdidos %>% 
  left_join(
    revision_total,
    by = "id_registro"
  ) %>% 
  mutate(
    causa_revision = coalesce(
      causa_revision,
      "Se perdió en alguna transformación intermedia"
    )
  )
# ============================================================
# 9. Resumen de validación
# ============================================================

resumen_validacion <- tibble(
  etapa = c(
    "Base original",
    "Numéricos válidos",
    "Numéricos revisión",
    "Texto válido",
    "Texto revisión",
    "Final válido",
    "Final revisión",
    "Perdidos"
  ),
  registros = c(
    nrow(df),
    nrow(df_numerico_mapa),
    nrow(df_numerico_revision),
    nrow(df_texto_mapa),
    nrow(df_texto_revision),
    nrow(df_final_mapa),
    nrow(df_final_revision),
    nrow(perdidos_con_causa)
  )
)

resumen_causas <- perdidos_con_causa %>% 
  count(origen_coord, causa_revision, sort = TRUE)


# ============================================================
# 10. Ver resultados
# ============================================================

resumen_validacion
resumen_causas

perdidos_con_causa %>% 
  select(
    id_registro,
    entidad,
    municipio,
    localidad,
    latitud_original,
    longitud_original,
    latitud_num,
    longitud_num,
    causa_revision
  )


# ============================================================
# 11. Exportar revisión
# ============================================================

write_xlsx(
  list(
    resumen_validacion = resumen_validacion,
    resumen_causas = resumen_causas,
    perdidos_con_causa = perdidos_con_causa,
    numerico_revision = df_numerico_revision,
    texto_revision = df_texto_revision,
    final_revision = df_final_revision,
    final_mapa = df_final_mapa
  ),
  file.path(out_dir, "validacion_coordenadas_perdidas.xlsx")
)
