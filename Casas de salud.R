# ============================================================
# 0. Librerías
# ============================================================

library(dplyr)
library(stringr)
library(readr)
library(purrr)
library(tibble)
library(leaflet)
library(readxl)
library(janitor)
library(writexl)


# ============================================================
# 1. Parámetros generales
# ============================================================

ruta_entrada <- "C:/Users/brittany.pereo/Downloads/CASAS_SALUD.xlsx"
hoja_entrada <- "Sin clues"

out_dir <- "C:/Users/brittany.pereo/Downloads"

lat_min <- 14
lat_max <- 33
lon_min <- -119
lon_max <- -86


# ============================================================
# 2. Funciones auxiliares
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

crear_mapa <- function(df, lat_col = "latitud", lon_col = "longitud", popup_cols = NULL) {
  
  lat <- df[[lat_col]]
  lon <- df[[lon_col]]
  
  mapa <- leaflet(df) %>% 
    addTiles() %>% 
    fitBounds(
      lng1 = min(lon, na.rm = TRUE),
      lat1 = min(lat, na.rm = TRUE),
      lng2 = max(lon, na.rm = TRUE),
      lat2 = max(lat, na.rm = TRUE)
    )
  
  if (is.null(popup_cols)) {
    mapa <- mapa %>% 
      addCircleMarkers(
        lng = lon,
        lat = lat,
        radius = 4,
        stroke = FALSE,
        fillOpacity = 0.7
      )
  } else {
    popup_txt <- df %>% 
      select(any_of(popup_cols)) %>% 
      mutate(across(everything(), as.character)) %>% 
      pmap_chr(~ paste(c(...), collapse = "<br>"))
    
    mapa <- mapa %>% 
      addCircleMarkers(
        lng = lon,
        lat = lat,
        radius = 4,
        stroke = FALSE,
        fillOpacity = 0.7,
        popup = popup_txt
      )
  }
  
  mapa
}


# ============================================================
# 3. Lectura de base
# ============================================================

df <- read_xlsx(
  ruta_entrada,
  sheet = hoja_entrada
) %>% 
  clean_names()


# ============================================================
# 4. Separar registros numéricos y de texto
# ============================================================

df_numerico <- df %>% 
  filter(
    es_numerico(latitud),
    es_numerico(longitud)
  ) %>% 
  mutate(
    latitud = as.numeric(latitud),
    longitud = as.numeric(longitud)
  )

df_texto <- df %>% 
  filter(
    !(es_numerico(latitud) & es_numerico(longitud))
  )


# ============================================================
# 5. Limpieza de coordenadas numéricas
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
    )
  )

df_numerico_mapa <- df_numerico_limpio %>% 
  filter(coord_valida(latitud, longitud))

df_numerico_revision <- df_numerico_limpio %>% 
  filter(!coord_valida(latitud, longitud))


# Mapa numérico limpio
mapa_numerico <- crear_mapa(
  df_numerico_mapa,
  lat_col = "latitud",
  lon_col = "longitud",
  popup_cols = c("entidad", "municipio", "latitud", "longitud")
)

mapa_numerico


# ============================================================
# 6. Limpieza de coordenadas en texto
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
    )
  )

df_texto_mapa <- df_texto_convertido %>% 
  filter(
    !is.na(latitud_num),
    !is.na(longitud_num),
    coord_valida(latitud_num, longitud_num)
  )

df_texto_revision <- df_texto_convertido %>% 
  filter(
    is.na(latitud_num) |
      is.na(longitud_num) |
      !coord_valida(latitud_num, longitud_num)
  )


# Mapa texto convertido
mapa_texto <- crear_mapa(
  df_texto_mapa,
  lat_col = "latitud_num",
  lon_col = "longitud_num",
  popup_cols = c("entidad", "municipio", "latitud", "longitud", "latitud_num", "longitud_num")
)

mapa_texto


# ============================================================
# 7. Base final integrada
# ============================================================

df_final <- bind_rows(
  
  df_numerico_mapa %>% 
    mutate(
      latitud_num = latitud,
      longitud_num = longitud
    ) %>% 
    select(-latitud, -longitud),
  
  df_texto_mapa %>% 
    select(-latitud, -longitud)
  
) %>% 
  rename(
    latitud = latitud_num,
    longitud = longitud_num
  )


# Corrección final por si quedaron lat/lon invertidas
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
    longitud = lon_tmp
  ) %>% 
  select(-lat_tmp, -lon_tmp)

df_final_mapa <- df_final_corregido %>% 
  filter(coord_valida(latitud, longitud))

df_final_revision <- df_final_corregido %>% 
  filter(!coord_valida(latitud, longitud))


# Mapa final
mapa_final <- crear_mapa(
  df_final_mapa,
  lat_col = "latitud",
  lon_col = "longitud",
  popup_cols = c("entidad", "municipio", "latitud", "longitud")
)

mapa_final


# ============================================================
# 8. Exportaciones
# ============================================================

write_xlsx(
  list(
    numerico_mapa = df_numerico_mapa,
    numerico_revision = df_numerico_revision,
    texto_mapa = df_texto_mapa,
    texto_revision = df_texto_revision,
    final_mapa = df_final_mapa,
    final_revision = df_final_revision
  ),
  file.path(out_dir, "casas_salud_coordenadas_limpias.xlsx")
)

write.csv(
  df_final_mapa,
  "casas_salud.csv",
  row.names = FALSE
)

