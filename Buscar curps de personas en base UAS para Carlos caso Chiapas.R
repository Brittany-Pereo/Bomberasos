library(googlesheets4)
library(readxl)
library(janitor)
library(dplyr)
library(tidyr)
library(purrr)
library(sf)
library(data.table)
library(writexl)
library(httr)
library(reticulate)
library(stringr)

# Google Sheets -----------------------------------------------------------
gs4_deauth()
gs4_auth(
  email = "lia.pereo@ciencias.unam.mx",
  scopes = "https://www.googleapis.com/auth/spreadsheets.readonly")

url <- "https://docs.google.com/spreadsheets/d/1Axj6z0U1odyFcdDz7Rjdv2jOZgnh9yIxKAXiWzvXKW4/edit?gid=0#gid=0"

df <- read_sheet(ss = url,
                 sheet = "Registros_completos") %>% 
  clean_names() %>% 
  mutate(across(where(is.character),
                ~ .x %>% 
                  str_trim() %>% 
                  str_to_upper()))

chiapas <- readxl::read_excel(
  "C:/Users/brittany.pereo/Downloads/CLUSTERS 190626 chiapas.xlsx", 
  sheet = "ITINIERANTES", 
  skip = 1
) %>% 
  janitor::clean_names() %>% 
  mutate(
    clues_ancla = str_trim(str_to_upper(clues_ancla)),
    clues = str_extract(clues_ancla, "[^-]+$"))


base_alex_original <- st_read(
  "C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/80_Basico comunitarios dificil acceso/bases/cluster_19_rutas_geo.gpkg"
) %>% 
  clean_names() %>% 
  st_drop_geometry() %>% 
  data.table() %>% 
  mutate(clues_ancla = str_remove(nombre_cluster, "_\\d+$")) %>% 
  distinct(clues_ancla, nombre_cluster, ancla_entidad,
           ancla_nombre) %>% 
  filter(!is.na(clues_ancla))

revision <- chiapas %>% 
  mutate(
    curp = str_trim(str_to_upper(curp)),
    clues_ancla = str_trim(str_to_upper(clues))
  ) %>% 
  left_join(
    df %>% 
      transmute(
        curp = str_trim(str_to_upper(curp)),
        clues_df = str_trim(str_to_upper(clues)),
        cnpm,
        puesto = clave_puesto,
        clave_del_puesto = cnpm,
        estatus_uas = revision_uas,
        enlace_a_carpeta = link_carpeta
      ),
    by = "curp"
  ) %>% 
  mutate(
    clave_del_puesto = ifelse(puesto == "OP057 CHOFER PROMOTOR POLIVALENTE", "POLIVALENTE", puesto),
    coincide_clues = case_when(
      is.na(clues_df) ~ "NO EN DF",
      clues_ancla == clues_df ~ "SI",
      TRUE ~ "CURP SI, CLUES DIFERENTE"
    ),
    puesto_arm = case_when(
      cnpm == "MG001" ~ "Medicina General",
      cnpm == "ME001" ~ "Anestesiologia",
      cnpm == "ME002" ~ "Cirugia",
      cnpm %in% c("EN002", "EN005") ~ "Enfermeria quirurgica",
      perfil == "POLIVALENTE" ~ "Chofer",
      TRUE ~ NA_character_
    )
  ) %>% 
  filter(!is.na(puesto_arm)) %>% 
  left_join(base_alex_original, by = "clues_ancla") %>% 
  mutate(
    ancla_entidad = coalesce(ancla_entidad, "SIN COINCIDENCIA CON CLUSTER"),
    nombre_cluster = coalesce(nombre_cluster, "SIN COINCIDENCIA CON CLUSTER"),
    ancla_nombre = coalesce(ancla_nombre, "SIN COINCIDENCIA CON CLUSTER")
  ) %>% 
  group_by(
    ancla_entidad,
    nombre_cluster,
    clues_ancla,
    ancla_nombre
  ) %>% 
  mutate(
    equipo_itinerante = pmin(
      sum(puesto_arm == "Anestesiologia", na.rm = TRUE),
      sum(puesto_arm == "Cirugia", na.rm = TRUE),
      sum(puesto_arm == "Medicina General", na.rm = TRUE),
      sum(puesto_arm == "Enfermeria quirurgica", na.rm = TRUE)
    )
  ) %>% 
  ungroup() %>% 
  transmute(
    ancla_entidad,
    cnpm,
    nombre_cluster,
    estado_ancla = ancla_entidad,
    clues_ancla,
    nombre_del_ancla = ancla_nombre,
    nombre,
    curp,
    puesto,
    clave_del_puesto,
    estatus_uas,
    cluster_id = nombre_cluster,
    enlace_a_carpeta,
    puesto_arm,
    equipo_itinerante
  )

write_xlsx(
  revision,
  "C:/Users/brittany.pereo/Downloads/casos_nuevos_chiapas.xlsx"
)
