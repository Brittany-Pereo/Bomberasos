library(googlesheets4)
library(tidyr)
library(dplyr)

gs4_deauth()
gs4_auth(
  email = "lia.pereo@ciencias.unam.mx",
  scopes = "https://www.googleapis.com/auth/spreadsheets.readonly"
)

url <- "https://docs.google.com/spreadsheets/d/1Axj6z0U1odyFcdDz7Rjdv2jOZgnh9yIxKAXiWzvXKW4/edit"

df <- read_sheet(
  ss = url,
  sheet = "Registros_completos")

curps_duplicadas <- df %>% 
  janitor::clean_names() %>% 
  filter(
    turno %in% c("Equipo itinerante", "EQUIPO ITINERANTE")
  ) %>% 
  transmute(estado, clues, curp, clave_puesto, cnpm) %>% 
  filter(!is.na(curp)) %>% 
  count(curp, sort = TRUE) %>% 
  filter(n > 1)

base_alex <- sf::st_read(
"C:/Users/brittany.pereo/Downloads/cluster_19_rutas_geo.gpkg"
  ) %>% 
  janitor::clean_names() %>% 
  data.table::data.table()

clues_no_pertenecen <- df %>% 
  janitor::clean_names() %>% 
  filter(
    turno %in% c("Equipo itinerante", "EQUIPO ITINERANTE")
  ) %>% 
  distinct(clues) %>% 
  anti_join(base_alex %>% 
              filter(tipo_cluster == "ancla"),
            by = c("clues" = "clues_imb_ancla"))

base_limpia <- df %>% 
  janitor::clean_names() %>% 
  filter(
    turno %in% c("Equipo itinerante", "EQUIPO ITINERANTE"),
    clues %in% ( base_alex %>% 
        filter(tipo_cluster == "ancla") %>% 
        pull(clues_imb_ancla))) %>% 
  transmute(estado, clues, curp, clave_puesto, cnpm) %>% 
  mutate(puesto = case_when(
    cnpm == "ME001" ~ "Anestesiologia",
    cnpm == "ME002" ~ "Cirugia",
    cnpm == "MG001" ~ "Medicina General",
    cnpm == "EN005" ~ "Enfermeria quirurgica",
    cnpm %in% c("PA022", "PA020") ~ "Chofer",
    
    clave_puesto == "MG001" ~ "Medicina General",
    clave_puesto == "MEDICINA GENERAL" ~ "Medicina General",
    
    clave_puesto == "ME002" ~ "Cirugia",
    clave_puesto == "CIRUGIA GENERAL" ~ "Cirugia",
    clave_puesto == "ME002 CIRUGIA GENERAL" ~ "Cirugia",
    
    clave_puesto == "EN005" ~ "Enfermeria quirurgica",
    clave_puesto == "ENFERMERA ESPECIALISTA CIRUGIA" ~ "Enfermeria quirurgica",
    
    clave_puesto == "ME001" ~ "Anestesiologia",
    clave_puesto == "ANESTESIOLOGIA" ~ "Anestesiologia",
    
    clave_puesto == "AUXILIAR ADMINISTRATIVO (CHOFER)" ~ "Chofer",
    clave_puesto == "CHOFER PROMOTOR POLIVALENTE" ~ "Chofer",
    clave_puesto == "CHOFER POLIVALENTE" ~ "Chofer",
    clave_puesto == "OP057 CHOFER PROMOTOR POLIVALENTE" ~ "Chofer",
    clave_puesto == "PA022" ~ "Chofer")) %>% 
  filter(!is.na(puesto)) %>% 
  group_by(entidad = estado, puesto) %>% 
  summarise(personas = n_distinct(curp), .groups = "drop") %>% 
  tidyr::pivot_wider(
    names_from = puesto,
    values_from = personas,
    values_fill = 0) %>% 
  rowwise() %>% 
  mutate(
    equipo_itinerante = min(c_across(where(is.numeric)), na.rm = TRUE)
  ) %>% 
  ungroup()


anclas <- base_alex %>% 
  select(clues_imb_ancla, ancla_entidad,
         numero_ancla, nombre_cluster) %>% 
  group_by(entidad = ancla_entidad) %>% 
  summarise(id = n_distinct(numero_ancla),
            id_val = n_distinct(nombre_cluster)) %>% 
  ungroup() %>% 
  mutate(entidad = case_when(
    entidad == "VERACRUZ DE IGNACIO DE LA LLAVE" ~ "Veracruz",
    entidad == "MICHOACAN DE OCAMPO" ~ "Michoacan",
    entidad == "MEXICO"  ~ "Estado de Mexico",
    TRUE ~ stringr::str_to_title(entidad)
  )) %>% 
  select(entidad, numero_cluster = id_val)

tabla_final <- inner_join(
  base_limpia, anclas
) %>% 
  mutate(equipo_qx_faltantes = numero_cluster - equipo_itinerante)


writexl::write_xlsx(tabla_final,
                    "C:/Users/brittany.pereo/Downloads/tabla.xlsx")
