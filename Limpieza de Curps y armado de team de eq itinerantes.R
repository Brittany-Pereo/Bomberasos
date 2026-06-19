library(googlesheets4)
library(tidyr)
library(dplyr)

catalogo_puestos <- readxl::read_xlsx(
  "C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/Plantilla/catalogos/Catalogo_CNPM_2026_F.xlsx"
) %>% 
  janitor::clean_names() %>% 
  select(cnpm = codigo_cnpm_26, denominacion_de_puesto)

gs4_deauth()
gs4_auth(
  email = "lia.pereo@ciencias.unam.mx",
  scopes = "https://www.googleapis.com/auth/spreadsheets.readonly"
)

url <- "https://docs.google.com/spreadsheets/d/1Axj6z0U1odyFcdDz7Rjdv2jOZgnh9yIxKAXiWzvXKW4/edit?gid=0#gid=0"

df <- read_sheet(
  ss = url,
  sheet = "Registros_completos") %>% 
  janitor::clean_names()

base_Carlos <- df %>% 
  filter(
    turno %in% c("Equipo itinerante", "EQUIPO ITINERANTE")
  ) %>% 
  transmute(estado, clues_ancla = clues, nombre_del_ancla= unidad_medica,
            curp, puesto = clave_puesto, cnpm, estatus_uas = revision_uas,
            link_carpeta) %>% 
  mutate(
    cnpm = case_when(
      puesto == "ME002 CIRUGIA GENERAL" ~ "ME002",
      puesto == "OP057 CHOFER PROMOTOR POLIVALENTE" ~ "PA020",
      puesto == "MG001 MEDICINA GENERAL" ~ "MG001",
      puesto == "OP065 AUXILIAR ADMINISTRATIVO (CHOFER)" ~ "PA022",
      puesto == "EN005 ENFERMERA ESPECIALISTA CIRUGIA" ~ "EN005",
      puesto == "CHOFER PROMOTOR POLIVALENTE" ~ "PA020",
      puesto == "CHOFER POLIVALENTE" ~ "PA022",
      puesto == "AUXILIAR ADMINISTRATIVO (CHOFER)" ~ "PA022",
      puesto == "CIRUGIA GENERAL" ~ "ME002",
      puesto == "ANESTESIOLOGIA" ~ "ME001",
      puesto == "ENFERMERA ESPECIALISTA CIRUGIA" ~ "EN005",
      puesto == "MEDICINA GENERAL" ~ "MG001",
      cnpm == "OP057" ~ "PA020",
      cnpm == "OP065" ~ "PA022", 
      TRUE ~ cnpm),
    cnpm = ifelse(is.na(cnpm), puesto, cnpm)
  ) %>% 
  left_join(catalogo_puestos, by = "cnpm") %>% 
  select(estado, clues_ancla, nombre_del_ancla,curp,
         puesto = denominacion_de_puesto, cnpm, 
         estatus_uas, link_carpeta)

#Nombres a partir de la clues
ruta <- "C:/Users/brittany.pereo/Downloads/curps_equipos_qx"
archivos <- list.files(
  path = ruta,
  pattern = "\\.xlsx$|\\.xls$",
  full.names = TRUE)

curps_limpias <- archivos %>% 
  map(readxl::read_excel) %>% 
  bind_rows() %>% 
  filter(is.na(error))
  
curps_limpio <- curps %>% 
  janitor::clean_names() %>% 
  transmute(
    curp,
    nombre = paste(
      ape_pat,
      ape_mat,
      nombres
    )
  ) %>% 
  distinct(curp, .keep_all = TRUE)

base_alex <- sf::st_read(
  "C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/80_Basico comunitarios dificil acceso/bases/cluster_19_rutas_geo_long.gpkg"
) %>% 
  janitor::clean_names() %>% 
  data.table::data.table() %>% 
  filter(tipo_cluster == "ancla") %>% 
  distinct(clues_imb, nombre_cluster, ancla_entidad)

clues_no_pertenecen <- base_Carlos %>% 
  janitor::clean_names() %>% 
  distinct(clues_ancla) %>% 
  anti_join(base_alex,
            by = c("clues_ancla" = "clues_imb")) 

base_Carlos_1 <- base_Carlos %>% 
  left_join(base_alex %>% distinct(clues_imb, .keep_all = TRUE),
            by = c("clues_ancla" = "clues_imb")) %>%
  left_join(curps_limpio, by = "curp" ) %>% 
  filter(!is.na(ancla_entidad)) %>% 
  group_by(ancla_entidad, clues_ancla, nombre_del_ancla,
           nombre, curp, puesto, cnpm, estatus_uas, nombre_cluster) %>% 
  mutate(n_duplicado = n()) %>% 
  ungroup() %>% 
  mutate(
    id_temp = interaction(
      ancla_entidad,
      clues_ancla,
      nombre_del_ancla,
      nombre,
      curp,
      puesto,
      cnpm,
      estatus_uas,
      nombre_cluster,
      drop = TRUE
    )
  ) %>% 
  
  # solo rankear duplicados reales
  mutate(
    duplicados = 0
  ) %>% 
  
  mutate(
    duplicados = if_else(
      n_duplicado > 1,
      match(
        id_temp,
        unique(id_temp[n_duplicado > 1])
      ),
      0L
    )
  ) %>% 
  
  transmute(
    estado_ancla = ancla_entidad,
    clues_ancla,
    nombre_del_ancla,
    nombre,
    curp,
    puesto,
    clave_del_puesto = cnpm,
    estatus_uas,
    cluster_id = nombre_cluster,
    enlace_a_carpeta = link_carpeta,
    duplicados
  )

writexl::write_xlsx(base_Carlos_1,
                    "C:/Users/brittany.pereo/Downloads/base carlos.xlsx")

base_limpia <- base_Carlos_1 %>% 
  mutate(
    puesto_arm = case_when(
      clave_del_puesto == "MG001" ~ "Medicina General",
      clave_del_puesto == "ME001" ~ "Anestesiologia",
      clave_del_puesto == "ME002" ~ "Cirugia",
      clave_del_puesto %in% c("EN002","EN005") ~ "Enfermeria quirurgica",
      clave_del_puesto %in% c("PA022", "PA020") ~ "Chofer",
            TRUE ~ NA_character_)
  ) %>% 
  filter(!is.na(puesto_arm)) %>% 
  group_by(estado_ancla, cluster_id, puesto_arm) %>% 
  summarise(personas = n_distinct(curp), .groups = "drop") %>% 
  tidyr::pivot_wider(
    names_from = puesto_arm,
    values_from = personas,
    values_fill = 0
  ) %>% 
  mutate(
    equipo_itinerante = pmin(
      `Anestesiologia`,
      Cirugia,
      `Medicina General`,
      `Enfermeria quirurgica`,
      Chofer,
      na.rm = TRUE
    ),
    
    anestesiologia_sobrante = `Anestesiologia` - equipo_itinerante,
    cirugia_sobrante = Cirugia - equipo_itinerante,
    medicina_general_sobrante = `Medicina General` - equipo_itinerante,
    enfermeria_quirurgica_sobrante = `Enfermeria quirurgica` - equipo_itinerante,
    chofer_sobrante = Chofer - equipo_itinerante,
    
    equipo_itinerante_incompleto = if_else(
      anestesiologia_sobrante +
        cirugia_sobrante +
        medicina_general_sobrante +
        enfermeria_quirurgica_sobrante +
        chofer_sobrante > 0,
      1L,
      0L
    ),
    
    puestos_faltantes = purrr::pmap_chr(
      list(
        anestesiologia_sobrante,
        cirugia_sobrante,
        medicina_general_sobrante,
        enfermeria_quirurgica_sobrante,
        chofer_sobrante
      ),
      function(anest, cir, med, enf, chof) {
        faltan <- c(
          if (anest < 1) "Anestesiologia",
          if (cir < 1) "Cirugia",
          if (med < 1) "Medicina General",
          if (enf < 1) "Enfermeria quirurgica",
          if (chof < 1) "Chofer"
        )
        
        if (length(faltan) == 5) {
          return("")
        }
        
        if (length(faltan) == 0) {
          return("")
        }
        
        paste(faltan, collapse = ", ")
      }
    )
  ) %>% 
select(
  -ends_with("_sobrante")
)

writexl::write_xlsx(base_limpia,
                    "C:/Users/brittany.pereo/Downloads/equipo itinerantes.xlsx")

resumen_team_qx <- base_limpia %>% 
  select(
    estado_ancla,
    cluster_id,
    Anestesiologia,
    Cirugia,
    `Medicina General`,
    `Enfermeria quirurgica`,
    Chofer,
    equipo_itinerante,
    equipo_itinerante_incompleto,
    puestos_faltantes
  ) %>% 
  mutate(
    team_qx = if_else(equipo_itinerante >= 1, 1L, 0L)
  )

base_Carlos_3 <- base_Carlos_1 %>% 
  left_join(
    resumen_team_qx,
    by = c("estado_ancla", "cluster_id")
  ) %>% 
  mutate(
    across(
      c(
        Anestesiologia,
        Cirugia,
        `Medicina General`,
        `Enfermeria quirurgica`,
        Chofer,
        equipo_itinerante,
        equipo_itinerante_incompleto,
        team_qx
      ),
      ~ tidyr::replace_na(.x, 0)
    ),
    puestos_faltantes = tidyr::replace_na(puestos_faltantes, "")
  ) %>% 
  select(-"Anestesiologia",-"Cirugia", -"Medicina General",            
         -"Enfermeria quirurgica", -"Chofer")

writexl::write_xlsx(base_Carlos_3,
                    "C:/Users/brittany.pereo/Downloads/equipo itinerantes com.xlsx")
