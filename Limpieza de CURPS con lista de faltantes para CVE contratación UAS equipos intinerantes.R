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

# FUNCIONES ---------------------------------------------------------------
# CURPs limpias 
py_run_string("
import requests
import json
import pandas as pd

requests.packages.urllib3.disable_warnings()

def consultar_curp(curp):
    headers = {
        'user-agent': 'Mozilla/5.0',
        'content-type': 'application/json; charset=utf-8'
    }

    payload = {'curp': curp.strip()}

    try:
        r = requests.post(
            'https://us-central1-os-gobierno-de-nuevo-leon.cloudfunctions.net/nuevoLeon-checkCurp',
            data=json.dumps(payload),
            headers=headers,
            verify=False
        )

        if r.status_code == 200:
            out = r.json()
            out['curp'] = curp.strip()
            return out

        return {'curp': curp.strip(), 'error': 'No encontrada'}

    except Exception as e:
        return {'curp': curp.strip(), 'error': str(e)}


def consultar_curps(curps):
    resultados = [consultar_curp(x) for x in curps]
    return pd.DataFrame(resultados)
")
regex_curp <- "^[A-Z]{4}[0-9]{6}[HM][A-Z]{5}[A-Z0-9]{2}$"

probar_link <- function(url) {
  tryCatch({
    resp <- HEAD(url, timeout(10))
    
    tibble(
      url = url,
      status = status_code(resp),
      funciona = status_code(resp) %in% c(200, 302)
    )
    
  }, error = function(e) {
    tibble(
      url = url,
      status = NA_integer_,
      funciona = FALSE
    )
  })
}
# Catálogo de puestos -----------------------------------------------------
catalogo_puestos <- read_xlsx(
  "C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/Plantilla/catalogos/Catalogo_CNPM_2026_F.xlsx"
) %>% 
  clean_names() %>% 
  select(cnpm = codigo_cnpm_26,
         denominacion_de_puesto)

hbc <- read_xlsx("C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/80_Basico comunitarios dificil acceso/bases/bases_clusters_viejas/cluster_19_carlos_long_simple.xlsx"
)

vector_ancla_cluster <- c(hbc$clues_imb, substr(hbc$nombre_cluster,1,11)) |> unique()
vector_ancla_cluster <- vector_ancla_cluster[!is.na(vector_ancla_cluster)]

# Google Sheets -----------------------------------------------------------
gs4_deauth()
gs4_auth(
  email = "lia.pereo@ciencias.unam.mx",
  scopes = "https://www.googleapis.com/auth/spreadsheets.readonly")

url <- "https://docs.google.com/spreadsheets/d/1Axj6z0U1odyFcdDz7Rjdv2jOZgnh9yIxKAXiWzvXKW4/edit?gid=0#gid=0"

df <- read_sheet(ss = url,
                 sheet = "Registros_completos") %>% 
  clean_names()
# Base madre --------------------------------------------------------------
base_online <- df %>% 
  filter(turno %like%"(itinerante)|(ITINERANTE)" | 
           (fase==3 &  clues%in%vector_ancla_cluster)) %>%
  transmute(estado, fase, turno, clues_ancla = clues,
            nombre_del_ancla = unidad_medica, curp, 
            puesto = clave_puesto,cnpm, estatus_uas = revision_uas,
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
    cnpm = if_else(is.na(cnpm), puesto, cnpm)) %>% 
  left_join(catalogo_puestos, by = "cnpm") %>% 
  select(estado, clues_ancla, nombre_del_ancla, fase, turno,link_carpeta,
         curp, puesto = denominacion_de_puesto,cnpm, estatus_uas) %>% 
  filter(cnpm %in% c("PA020", "PA022", "ME001", "ME002", 
                     "MG001", "EN005") | turno %like%"(itinerante)|(ITINERANTE)")


base_alex_original <- st_read(
  "C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/80_Basico comunitarios dificil acceso/bases/cluster_19_rutas_geo_long.gpkg"
) 

base_alex <-base_alex_original%>% 
  clean_names() %>% 
  st_drop_geometry() %>% 
  data.table() %>% 
  mutate(clues_ancla = str_remove(nombre_cluster, "_\\d+$")) %>% 
  distinct(clues_ancla, nombre_cluster, ancla_entidad) %>% 
  filter(!is.na(clues_ancla))

clues_no_pertenecen <- base_online %>% 
  clean_names() %>% 
  distinct(clues_ancla) %>% 
  anti_join(
    base_alex,
    by = c("clues_ancla" = "clues_ancla")
  )
table(clues_no_pertenecen$clues_ancla%in%base_alex_original$clues_imb)

base_alex_original %>% 
  filter(clues_imb%in%clues_no_pertenecen$clues_ancla) %>% 
  transmute(clues_imb, clues_ancla=substr(nombre_cluster,1,11)) %>%  
  st_drop_geometry()

base_online <- base_online  %>% 
  mutate(clues_ancla = case_when(
    clues_ancla == "PLIMB003706" ~ "PLIMB002516",
    clues_ancla == "SLIMB001950" ~ "SLIMB002930",
    clues_ancla == "SLIMB000195" ~ "SLIMB002930",
    clues_ancla == "SLIMB001554" ~ "SLIMB002930",
    clues_ancla == "BSIMB000503" ~ "BSIMB000754",
    clues_ancla == "TSIMB003483" ~ "TSIMB001260",
    TRUE ~ clues_ancla  # Mantiene el valor original si no coincide
  ))

clues_no_pertenecen <- base_online %>% 
  clean_names() %>% 
  distinct(clues_ancla) %>% 
  anti_join(base_alex,
            by = "clues_ancla")

base_online_1 <- base_online %>% 
  left_join(
    base_alex %>% distinct(clues_ancla, .keep_all = TRUE),
    by = "clues_ancla"
  ) %>%
  mutate(
    ancla_entidad = if_else(
      is.na(ancla_entidad),
      estado,
      ancla_entidad
    ),
    nombre_cluster = if_else(
      is.na(nombre_cluster),
      "Sin match en cluster",
      nombre_cluster
    )
  ) %>% 
  group_by(
    ancla_entidad, clues_ancla, nombre_del_ancla, fase, 
    turno, curp, puesto, cnpm, estatus_uas, nombre_cluster
  ) %>% 
  mutate(n_duplicado = n()) %>% 
  ungroup() %>% 
  mutate(
    id_temp = interaction(
      ancla_entidad, clues_ancla, nombre_del_ancla, curp, fase, turno,
      puesto, cnpm, estatus_uas, nombre_cluster,
      drop = TRUE
    ),
    duplicados = if_else(
      n_duplicado > 1,
      match(id_temp, unique(id_temp[n_duplicado > 1])),
      0L
    )
  )

sin_match_cluster <- base_online_1 %>% 
  filter(nombre_cluster == "Sin match en cluster")

base_online_1 <- base_online_1 %>% 
  mutate(
    curp_original = curp,
    curp_limpia = curp %>% 
      as.character() %>% 
      str_replace_all("['\"`´“”‘’]", "") %>% 
      str_replace_all("\\s+", "") %>% 
      str_replace_all("[[:cntrl:]]", "") %>% 
      str_trim() %>% 
      str_to_upper(),
    cambio_limpieza = curp_original != curp_limpia,
    curp_vacia = is.na(curp_limpia) | curp_limpia == "",
    longitud_curp = nchar(curp_limpia),
    formato_curp_valido = str_detect(curp_limpia, regex_curp),
    estatus_validacion_curp = case_when(
      curp_vacia ~ "CURP vacía",
      longitud_curp != 18 ~ "Longitud distinta de 18",
      !formato_curp_valido ~ "Formato inválido",
      cambio_limpieza ~ "CURP corregida por limpieza",
      TRUE ~ "CURP válida sin cambios")) %>% 
  select(curp_original, curp_limpia, cambio_limpieza,
         curp_vacia, longitud_curp,formato_curp_valido,
         estatus_validacion_curp,everything())

# Base equipos itinerantes ------------------------------------------------
vector_itinerantes <- base_online_1 %>% 
  filter(formato_curp_valido,
         turno %in% c("Equipo itinerante", "EQUIPO ITINERANTE")) %>% 
  distinct(curp_limpia) %>% 
  pull(curp_limpia)

resultado_curps_py_it <- py$consultar_curps(vector_itinerantes)

base_itinerantes <- resultado_curps_py_it$to_csv(index = FALSE) %>% 
  readr::read_csv(show_col_types = FALSE) %>% 
  select(curp_limpia = curp, apePat, apeMat, nombres) %>% 
  mutate(across(everything(), as.character))

base_itinerantes <- base_online_1 %>% 
  left_join(base_itinerantes, by = "curp_limpia")%>% 
  mutate(nombre = str_squish(
    paste(nombres, apePat, apeMat)),
    consulta_endpoint_exitosa = !is.na(nombres) | !is.na(apePat) | !is.na(apeMat),
    estatus_consulta_curp = case_when(
      !formato_curp_valido ~ estatus_validacion_curp,
      consulta_endpoint_exitosa ~ "CURP encontrada",
      TRUE ~ "CURP válida en formato, no encontrada en endpoint"))

# Validacion de curps --
tabla_validaciones <- base_itinerantes %>% 
  count(estatus_consulta_curp, sort = TRUE)

curps_invalidas <- base_itinerantes %>% 
  filter(!formato_curp_valido)

curps_corregidas <- base_itinerantes %>% 
  filter(cambio_limpieza)

# CURPs válidas que no regresaron nombre/apellidos en el endpoint
sin_datos_curp <- base_itinerantes %>% 
  filter(
    formato_curp_valido,
    is.na(nombres),
    is.na(apePat),
    is.na(apeMat)
  ) %>% 
  mutate(
    motivo_eliminacion = "CURP válida pero sin nombre/apellidos en endpoint"
  )

# Quitar de la base principal las que no regresaron datos
base_itinerantes <- base_itinerantes %>% 
  filter(
    !(
      formato_curp_valido &
        is.na(nombres) &
        is.na(apePat) &
        is.na(apeMat)
    )
  )

base_itinerantes <- base_itinerantes %>% 
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

write_xlsx(
  list(
    base_con_nombres = base_itinerantes,
    resumen_validaciones = tabla_validaciones,
    curps_invalidas = curps_invalidas,
    curps_corregidas = curps_corregidas,
    sin_datos_curp = sin_datos_curp,
    sin_match_cluster = sin_match_cluster
  ),
  "C:/Users/brittany.pereo/Downloads/base_eq_itinerantes.xlsx"
)

# Base equipos fase 3 -----------------------------------------------------
vector_fase3 <- base_online_1 %>% 
  filter(formato_curp_valido,
         fase == 3) %>% 
  distinct(curp_limpia) %>% 
  pull(curp_limpia)

resultado_curps_py <- py$consultar_curps(vector_fase3)

base_fase3 <- resultado_curps_py$to_csv(index = FALSE) %>% 
  readr::read_csv(show_col_types = FALSE) %>% 
  select(curp_limpia = curp, apePat, apeMat, nombres) %>% 
  mutate(across(everything(), as.character))

base_fase3 <- base_online_1 %>% 
  left_join(base_fase3, by = "curp_limpia") %>% 
  mutate(nombre = str_squish(
    paste(nombres, apePat, apeMat)),
    consulta_endpoint_exitosa = !is.na(nombres) | !is.na(apePat) | !is.na(apeMat),
    estatus_consulta_curp = case_when(
      !formato_curp_valido ~ estatus_validacion_curp,
      consulta_endpoint_exitosa ~ "CURP encontrada",
      TRUE ~ "CURP válida en formato, no encontrada en endpoint"))

# Validacion de curps --
tabla_validaciones <- base_fase3 %>% 
  count(estatus_consulta_curp, sort = TRUE)

curps_invalidas <- base_fase3 %>% 
  filter(!formato_curp_valido)

curps_corregidas <- base_fase3 %>% 
  filter(cambio_limpieza)

# CURPs válidas que no regresaron nombre/apellidos
sin_datos_curp <- base_fase3 %>% 
  filter(
    formato_curp_valido,
    is.na(nombres),
    is.na(apePat),
    is.na(apeMat)
  ) %>% 
  mutate(
    motivo_eliminacion = "CURP válida pero sin nombre/apellidos en endpoint"
  )

# Removerlas de la base principal
base_fase3 <- base_fase3 %>% 
  filter(
    !(
      formato_curp_valido &
        is.na(nombres) &
        is.na(apePat) &
        is.na(apeMat)
    )
  )

base_fase3 <- base_fase3 %>% 
  transmute(estado_ancla = ancla_entidad, clues_ancla, nombre_del_ancla,
            nombre, curp, puesto, clave_del_puesto = cnpm, estatus_uas,
            cluster_id = nombre_cluster, enlace_a_carpeta = link_carpeta,
            duplicados)

write_xlsx(
  list(
    base_con_nombres = base_fase3,
    resumen_validaciones = tabla_validaciones,
    curps_invalidas = curps_invalidas,
    curps_corregidas = curps_corregidas,
    sin_datos_curp = sin_datos_curp,
    sin_match_cluster = sin_match_cluster
  ),
  "C:/Users/brittany.pereo/Downloads/base_fase3.xlsx"
)
# -------------------------------------------------------------------------
#EXTRAS
# -------------------------------------------------------------------------
# Resumen de equipos itinerantes -----------------------------------------
base_limpia_itinerantes <- base_itinerantes %>% 
  mutate(
    puesto_arm = case_when(
      clave_del_puesto == "MG001" ~ "Medicina General",
      clave_del_puesto == "ME001" ~ "Anestesiologia",
      clave_del_puesto == "ME002" ~ "Cirugia",
      clave_del_puesto %in% c("EN002", "EN005") ~ "Enfermeria quirurgica",
      clave_del_puesto %in% c("PA022", "PA020") ~ "Chofer",
      TRUE ~ NA_character_
    )
  ) %>% 
  filter(!is.na(puesto_arm)) %>% 
  group_by(estado_ancla, cluster_id, puesto_arm) %>% 
  summarise(
    personas = n_distinct(curp),
    .groups = "drop"
  ) %>% 
  pivot_wider(
    names_from = puesto_arm,
    values_from = personas,
    values_fill = 0
  ) %>% 
  mutate(
    equipo_itinerante = pmin(
      Anestesiologia,
      Cirugia,
      `Medicina General`,
      `Enfermeria quirurgica`,
      Chofer,
      na.rm = TRUE
    ),
    anestesiologia_sobrante = Anestesiologia - equipo_itinerante,
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
    
    puestos_faltantes = pmap_chr(
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
        
        if (length(faltan) == 5) return("")
        if (length(faltan) == 0) return("")
        
        paste(faltan, collapse = ", ")
      }
    )
  ) %>% 
  select(-ends_with("_sobrante"))


resumen_team_qx <- base_limpia_itinerantes %>% 
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
base_itinerantes_final <- base_itinerantes%>% 
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
      ~ replace_na(.x, 0)
    ),
    puestos_faltantes = replace_na(puestos_faltantes, "")
  ) %>% 
  select(
    -Anestesiologia,
    -Cirugia,
    -`Medicina General`,
    -`Enfermeria quirurgica`,
    -Chofer
  )

revision_links <- base_itinerantes_final %>% 
  filter(duplicados > 0) %>% 
  distinct(duplicados, enlace_a_carpeta) %>% 
  mutate(
    revision = map(enlace_a_carpeta, probar_link)
  ) %>% 
  unnest(revision)

links_buenos <- revision_links %>% 
  filter(funciona) %>% 
  group_by(duplicados) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(duplicados, enlace_a_carpeta)

base_corregida <- base_itinerantes_final%>% 
  left_join(
    links_buenos %>% 
      mutate(link_bueno = TRUE),
    by = c("duplicados", "enlace_a_carpeta")
  ) %>% 
  filter(
    duplicados == 0 | link_bueno == TRUE
  )

duplicados_link_feo <- base_itinerantes_final %>% 
  left_join(
    links_buenos %>% 
      mutate(link_bueno = TRUE),
    by = c("duplicados", "enlace_a_carpeta")
  ) %>% 
  filter(
    duplicados > 0,
    is.na(link_bueno)
  ) %>% 
  mutate(
    motivo_eliminacion = "Duplicado eliminado porque el link no abre"
  ) %>% 
  select(-link_bueno)

observaciones_eliminadas_filtros <- base_online %>% 
  anti_join(
    base_corregida %>% 
      distinct(curp, clues_ancla, clave_del_puesto),
    by = c(
      "curp",
      "clues_ancla",
      "cnpm" = "clave_del_puesto"
    )
  ) %>% 
  mutate(
    motivo_eliminacion = case_when(
      !clues_ancla %in% base_alex$clues_imb ~ "CLUES no pertenece a base_alex / no es ancla",
      TRUE ~ "Se eliminó en filtros posteriores"
    )
  )

observaciones_eliminadas <- bind_rows(
  observaciones_eliminadas_filtros,
  duplicados_link_feo)

base_corregida <- base_corregida %>% 
  select(-link_bueno, -duplicados, -team_qx,
         -puestos_faltantes, -equipo_itinerante_incompleto)

write_xlsx(
  list(
    base_corregida = base_corregida,
    observaciones_eliminadas = observaciones_eliminadas
  ),
  "C:/Users/brittany.pereo/Downloads/equipo itinerantes completos.xlsx"
)

# Resumen de fase 3 -----------------------------------------
base_limpia_fase3 <- base_fase3 %>% 
  mutate(
    puesto_arm = case_when(
      clave_del_puesto == "MG001" ~ "Medicina General",
      clave_del_puesto == "ME001" ~ "Anestesiologia",
      clave_del_puesto == "ME002" ~ "Cirugia",
      clave_del_puesto %in% c("EN002", "EN005") ~ "Enfermeria quirurgica",
      clave_del_puesto %in% c("PA022", "PA020") ~ "Chofer",
      TRUE ~ NA_character_
    )
  ) %>% 
  filter(!is.na(puesto_arm)) %>% 
  group_by(estado_ancla, cluster_id, puesto_arm) %>% 
  summarise(
    personas = n_distinct(curp),
    .groups = "drop"
  ) %>% 
  pivot_wider(
    names_from = puesto_arm,
    values_from = personas,
    values_fill = 0
  ) %>% 
  mutate(
    equipo_itinerante = pmin(
      Anestesiologia,
      Cirugia,
      `Medicina General`,
      `Enfermeria quirurgica`,
      Chofer,
      na.rm = TRUE
    ),
    anestesiologia_sobrante = Anestesiologia - equipo_itinerante,
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
    
    puestos_faltantes = pmap_chr(
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
        
        if (length(faltan) == 5) return("")
        if (length(faltan) == 0) return("")
        
        paste(faltan, collapse = ", ")
      }
    )
  ) %>% 
  select(-ends_with("_sobrante"))

resumen_team_qx <- base_limpia_fase3 %>% 
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
base_fase3_final <- base_fase3%>% 
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
      ~ replace_na(.x, 0)
    ),
    puestos_faltantes = replace_na(puestos_faltantes, "")
  ) %>% 
  select(
    -Anestesiologia,
    -Cirugia,
    -`Medicina General`,
    -`Enfermeria quirurgica`,
    -Chofer
  )

revision_links <- base_fase3_final %>% 
  filter(duplicados > 0) %>% 
  distinct(duplicados, enlace_a_carpeta) %>% 
  mutate(
    revision = map(enlace_a_carpeta, probar_link)
  ) %>% 
  unnest(revision)

links_buenos <- revision_links %>% 
  filter(funciona) %>% 
  group_by(duplicados) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(duplicados, enlace_a_carpeta)

base_corregida <- base_fase3_final%>% 
  left_join(
    links_buenos %>% 
      mutate(link_bueno = TRUE),
    by = c("duplicados", "enlace_a_carpeta")
  ) %>% 
  filter(
    duplicados == 0 | link_bueno == TRUE
  )

duplicados_link_feo <- base_fase3_final %>% 
  left_join(
    links_buenos %>% 
      mutate(link_bueno = TRUE),
    by = c("duplicados", "enlace_a_carpeta")
  ) %>% 
  filter(
    duplicados > 0,
    is.na(link_bueno)
  ) %>% 
  mutate(
    motivo_eliminacion = "Duplicado eliminado porque el link no abre"
  ) %>% 
  select(-link_bueno)

observaciones_eliminadas_filtros <- base_online %>% 
  anti_join(
    base_corregida %>% 
      distinct(curp, clues_ancla, clave_del_puesto),
    by = c(
      "curp",
      "clues_ancla",
      "cnpm" = "clave_del_puesto"
    )
  ) %>% 
  mutate(
    motivo_eliminacion = case_when(
      !clues_ancla %in% base_alex$clues_imb ~ "CLUES no pertenece a base_alex / no es ancla",
      TRUE ~ "Se eliminó en filtros posteriores"
    )
  )

observaciones_eliminadas <- bind_rows(
  observaciones_eliminadas_filtros,
  duplicados_link_feo)

base_corregida <- base_corregida %>% 
  select(-link_bueno, -duplicados, -team_qx,
         -puestos_faltantes, -equipo_itinerante_incompleto)

write_xlsx(
  list(
    base_corregida = base_corregida,
    observaciones_eliminadas = observaciones_eliminadas
  ),
  "C:/Users/brittany.pereo/Downloads/equipo fase 3 completos.xlsx"
)

# Resumen por entidad y cluster -------------------------------------------
catalogo_clues <- arrow::read_parquet(
  "C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/CLUES/clues.parquet"
)
resumen_por_cluster <- base_limpia %>% 
  mutate(
    arma_team_qx = if_else(
      equipo_itinerante >= 1,
      "Sí arma team QX",
      "No arma team QX"
    ),
    clues_ancla = stringr::str_remove(cluster_id, "_\\d+$")
  ) %>% 
  left_join(
    catalogo_clues%>% 
      select(
        clues_ancla = clues_imb,
        nombre_del_ancla = nombre_comercial
      ) %>% 
      distinct(),
    by = "clues_ancla") %>% 
  filter(arma_team_qx == "No arma team QX") %>% 
  
  select(
    entidad = estado_ancla,
    nombre_cluster = cluster_id,
    clues_ancla,
    hospital_ancla = nombre_del_ancla,
    anestesiologia = Anestesiologia,
    cirugia = Cirugia,
    medicina_general = `Medicina General`,
    enfermeria_quirurgica = `Enfermeria quirurgica`,
    chofer = Chofer,
    equipos_qx_armados = equipo_itinerante,
    arma_team_qx,
    puestos_faltantes
  ) %>% 
  
  distinct() %>% 
  
  arrange(
    entidad,
    nombre_cluster
  )

write_xlsx(resumen_por_cluster,
           "C:/Users/brittany.pereo/Downloads/faltantes.xlsx"
)
