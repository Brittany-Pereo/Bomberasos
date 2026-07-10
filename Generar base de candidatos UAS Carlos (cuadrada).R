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
reticulate::py_run_string("
import requests
import json
from concurrent.futures import ThreadPoolExecutor, as_completed

requests.packages.urllib3.disable_warnings()

URL = 'https://us-central1-os-gobierno-de-nuevo-leon.cloudfunctions.net/nuevoLeon-checkCurp'

headers = {
    'user-agent': 'Mozilla/5.0',
    'content-type': 'application/json; charset=utf-8'}
session = requests.Session()

def consultar_curp(curp):
    curp = str(curp).strip()

    if curp == '' or curp.lower() == 'nan':
        return {'curp': curp, 'error': 'CURP vacía'}

    payload = {'curp': curp}

    try:
        r = session.post(
            URL,
            data=json.dumps(payload),
            headers=headers,
            verify=False,
            timeout=15
        )

        if r.status_code == 200:
            out = r.json()
            out['curp'] = curp
            return out

        return {'curp': curp, 'error': 'No encontrada'}

    except Exception as e:
        return {'curp': curp, 'error': str(e)}


def consultar_curps(curps, max_workers=10):
    curps_limpias = [
        str(x).strip()
        for x in curps
        if str(x).strip() != '' and str(x).strip().lower() != 'nan'
    ]

    curps_limpias = list(dict.fromkeys(curps_limpias))

    resultados = []

    with ThreadPoolExecutor(max_workers=max_workers) as executor:
        futuros = {
            executor.submit(consultar_curp, curp): curp
            for curp in curps_limpias
        }

        for futuro in as_completed(futuros):
            resultados.append(futuro.result())

    return resultados
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

limpiar_curp <- function(x) {
  x %>% 
    as.character() %>% 
    str_replace_all("['\"`´“”‘’]", "") %>% 
    str_replace_all("\\s+", "") %>% 
    str_replace_all("[[:cntrl:]]", "") %>% 
    str_trim() %>% 
    str_to_upper()
}

homologar_cnpm <- function(cnpm, clave_puesto) {
  case_when(
    cnpm == "OP057" ~ "PA020",
    cnpm == "OP065" ~ "PA022",
    clave_puesto %in% c(
      "OP057 CHOFER PROMOTOR POLIVALENTE",
      "CHOFER PROMOTOR POLIVALENTE"
    ) ~ "PA020",
    clave_puesto %in% c(
      "OP065 AUXILIAR ADMINISTRATIVO (CHOFER)",
      "CHOFER POLIVALENTE",
      "AUXILIAR ADMINISTRATIVO (CHOFER)"
    ) ~ "PA022",
    clave_puesto %in% c(
      "ME002 CIRUGIA GENERAL",
      "CIRUGIA GENERAL"
    ) ~ "ME002",
    clave_puesto %in% c(
      "ANESTESIOLOGIA"
    ) ~ "ME001",
    clave_puesto %in% c(
      "MG001 MEDICINA GENERAL",
      "MEDICINA GENERAL"
    ) ~ "MG001",
    clave_puesto %in% c(
      "EN005 ENFERMERA ESPECIALISTA CIRUGIA",
      "ENFERMERA ESPECIALISTA CIRUGIA"
    ) ~ "EN005",
    is.na(cnpm) ~ clave_puesto,
    TRUE ~ cnpm
  )
}

corregir_clues <- function(x) {
  recode(
    x,
    "PLIMB003706" = "PLIMB002516",
    "SLIMB001950" = "SLIMB002930",
    "SLIMB000195" = "SLIMB002930",
    "SLIMB001554" = "SLIMB002930",
    "BSIMB000503" = "BSIMB000754",
    "TSIMB003483" = "TSIMB001260",
    .default = x
  )
}
# Catálogo -----------------------------------------------------
catalogo_puestos <- read_xlsx(
  "C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/Plantilla/catalogos/Catalogo_CNPM_2026_F.xlsx"
) %>% 
  clean_names() %>% 
  select(cnpm = codigo_cnpm_26,
         denominacion_de_puesto)

hbc <- read_xlsx(
  "C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/80_Basico comunitarios dificil acceso/bases/bases_clusters_viejas/cluster_19_carlos_long_simple.xlsx"
)

vector_ancla_cluster <- c(hbc$clues_imb, substr(hbc$nombre_cluster,1,11)) |> unique()
vector_ancla_cluster <- vector_ancla_cluster[!is.na(vector_ancla_cluster)]

team_completos <- base_eq_completos <- readxl::read_xlsx(
  "C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/87_ Plan de contratacion/Data/equipo itinerantes 23062026.xlsx"
) %>% 
  clean_names() %>% 
  mutate(cluster_id = ifelse(cluster_id == "CSIMB005500_09", "CSIMB005500_8", 
                             cluster_id))

base_alex_original <- st_read(
  "C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/80_Basico comunitarios dificil acceso/bases/cluster_19_rutas_geo.gpkg"
) 

base_alex <-base_alex_original%>% 
  clean_names() %>% 
  st_drop_geometry() %>% 
  data.table() %>% 
  mutate(clues_ancla = str_remove(nombre_cluster, "_\\d+$")) %>% 
  distinct(clues_ancla, nombre_cluster, ancla_entidad,
           ancla_nombre) %>% 
  filter(!is.na(clues_ancla)) %>% 
  anti_join(team_completos %>% select(cluster_id),
            by = c("nombre_cluster"= "cluster_id"))

team_completos_oficiales <- team_completos %>% 
  mutate(curp_limpia = curp %>% 
           as.character() %>% 
           str_replace_all("['\"`´“”‘’]", "") %>% 
           str_replace_all("\\s+", "") %>% 
           str_trim() %>% 
           str_to_upper()) %>% 
  distinct(curp_limpia, .keep_all = TRUE) %>% 
  transmute(curp_limpia,
            clues = clues_ancla)

vector_team_completos <- unique(team_completos_oficiales$clues)

base_anterior_curps <- readxl::read_xlsx(
  "C:/Users/brittany.pereo/Downloads/casos nuevos.xlsx"
) %>%
  select(curp, nombre) %>%
  filter(!is.na(nombre))
# Google Sheets -----------------------------------------------------------
gs4_deauth()
gs4_auth(
  email = "lia.pereo@ciencias.unam.mx",
  scopes = "https://www.googleapis.com/auth/spreadsheets.readonly")

url <- "https://docs.google.com/spreadsheets/d/1Axj6z0U1odyFcdDz7Rjdv2jOZgnh9yIxKAXiWzvXKW4/edit?gid=0#gid=0"


df <- read_sheet(ss = url,
                 sheet = "Registros_completos") %>% 
  clean_names() %>% 
  mutate(across(
    where(is.character) & !matches("link_carpeta"),
    ~ str_to_upper(str_trim(.x))),
    link_carpeta = str_trim(link_carpeta))
#  Base online limpia y priorizada ----------------------------------------
puestos_validos <- c("PA020", "PA022", "ME001", "ME002", "MG001", "EN005", "EN002")

base_online <- df %>% 
  mutate(curp_limpia = limpiar_curp(curp),
         cnpm = homologar_cnpm(cnpm, clave_puesto)) %>% 
  anti_join( team_completos_oficiales %>% distinct(curp_limpia),
             by = "curp_limpia") %>% 
  filter(str_detect(turno, regex("itinerante", ignore_case = TRUE)) |
           (fase == 3 & clues %in% vector_ancla_cluster)) %>% 
  left_join(catalogo_puestos, by = "cnpm") %>% 
  transmute(estado, clues_ancla = clues, fase, turno, link_carpeta,
            curp, puesto = denominacion_de_puesto, cnpm,
            estatus_uas = revision_uas) %>% 
  filter(cnpm %in% puestos_validos,
         is.na(estatus_uas) | estatus_uas == "APROBADO") %>% 
  distinct(across(-link_carpeta), .keep_all = TRUE)

links_validos <- base_online %>%
  distinct(link_carpeta) %>%
  mutate(info = purrr::map(link_carpeta, probar_link)) %>%
  tidyr::unnest(info) %>%
  select(link_carpeta = url, funciona)

base_online <- base_online %>%
  left_join(links_validos, by = "link_carpeta") %>% 
  mutate(prioridad_estatus = case_when(
    estatus_uas == "APROBADO" ~ 1L,
    is.na(estatus_uas) | str_trim(estatus_uas) == "" ~ 2L,
    TRUE ~ 3L),
    prioridad_link = if_else(funciona == TRUE, 1L, 2L)) %>% 
  arrange(curp, clues_ancla, prioridad_estatus, prioridad_link) %>% 
  group_by(curp, clues_ancla) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-prioridad_estatus, -prioridad_link) %>% 
  mutate(clues_ancla = corregir_clues(clues_ancla))

clues_no_pertenecen <- base_online %>% 
  distinct(clues_ancla) %>% 
  anti_join(base_alex, by = "clues_ancla")

base_online_1 <- base_online %>% 
  left_join(base_alex %>% distinct(clues_ancla, .keep_all = TRUE),
            by = "clues_ancla") %>% 
  filter(!is.na(nombre_cluster)) %>% 
  mutate(ancla_entidad = coalesce(ancla_entidad, estado),
         nombre_cluster = coalesce(nombre_cluster, "Sin match en cluster")) %>% 
  group_by(ancla_entidad, clues_ancla, nombre_del_ancla = ancla_nombre,
           curp, puesto, cnpm, estatus_uas, nombre_cluster) %>% 
  mutate(n_duplicado = n()) %>% 
  ungroup()

sin_match_cluster <- base_online_1 %>% 
  filter(nombre_cluster == "Sin match en cluster")

base_anterior_curps_join <- base_anterior_curps %>% 
  mutate(curp_limpia = limpiar_curp(curp)) %>% 
  filter(!is.na(curp_limpia), curp_limpia != "") %>% 
  distinct(curp_limpia, .keep_all = TRUE) %>% 
  transmute(curp_limpia, nombre_base = nombre)

base_online_1 <- base_online_1 %>% 
  mutate(curp_original = curp,
         curp_limpia = limpiar_curp(curp),
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
  left_join(base_anterior_curps_join, by = "curp_limpia") %>% 
  mutate(nombre = nombre_base,
         nombre_recuperado_base_previa = !is.na(nombre_base) & nombre_base != "") %>% 
  select(curp_original, curp_limpia, nombre, nombre_recuperado_base_previa,
         cambio_limpieza, curp_vacia, longitud_curp,
         formato_curp_valido, estatus_validacion_curp,
         everything(),  -nombre_base)

vector_curps <- base_online_1 %>% 
  filter(is.na(nombre) | nombre == "" | nombre == "NA NA NA") %>% 
  filter(formato_curp_valido) %>% 
  distinct(curp_limpia) %>% 
  pull(curp_limpia)

resultado_curps_py_it <- py$consultar_curps(vector_curps)

base_limpia_endpoint <- purrr::map_dfr(
  resultado_curps_py_it,
  ~ tibble::as_tibble(.x)) %>%
  janitor::clean_names() %>%
  rename(curp_limpia = curp)

for (col in c("ape_pat", "ape_mat", "nombres", "error")) {
  if (!col %in% names(base_limpia_endpoint)) {
    base_limpia_endpoint[[col]] <- NA_character_
  }
}

base_limpia_endpoint <- base_limpia_endpoint %>%
  transmute(
    curp_limpia = as.character(curp_limpia),
    ape_pat = as.character(ape_pat),
    ape_mat = as.character(ape_mat),
    nombres = as.character(nombres),
    nombre_endpoint = str_squish(paste(nombres, ape_pat, ape_mat)),
    nombre_endpoint = na_if(nombre_endpoint, "NA NA NA"),
    error_endpoint = as.character(error))


base_limpia <- base_online_1 %>% 
  left_join(base_limpia_endpoint, by = "curp_limpia") %>%
  mutate(
    nombre = coalesce(nombre, nombre_endpoint),
    consulta_endpoint_exitosa = !is.na(nombres) | !is.na(ape_pat) | !is.na(ape_mat),
    estatus_consulta_curp = case_when(
      !formato_curp_valido ~ estatus_validacion_curp,
      nombre_recuperado_base_previa ~ "Nombre recuperado de base previa",
      consulta_endpoint_exitosa ~ "CURP encontrada en endpoint",
      TRUE ~ "CURP válida en formato, no encontrada en endpoint"))

tabla_validaciones <- base_limpia %>%
  count(estatus_consulta_curp, sort = TRUE)

curps_invalidas <- base_limpia %>% 
  filter(!formato_curp_valido)

curps_corregidas <- base_limpia %>% 
  filter(cambio_limpieza)

sin_datos_curp <- base_limpia %>%
  filter(
    formato_curp_valido,
    is.na(nombres),
    is.na(ape_pat),
    is.na(ape_mat),
    is.na(nombre)) %>%
  mutate(
    motivo_eliminacion = "CURP válida pero sin nombre/apellidos en endpoint")

base_limpia <- base_limpia %>% 
  filter(
    !(
      formato_curp_valido &
        is.na(nombres) &
        is.na(ape_pat) &
        is.na(ape_mat) &
        is.na(nombre))) %>%
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
    enlace_a_carpeta = link_carpeta)

base_final <- base_limpia %>% 
  mutate(
    puesto_arm = case_when(
      clave_del_puesto == "MG001" ~ "Medicina General",
      clave_del_puesto == "ME001" ~ "Anestesiologia",
      clave_del_puesto == "ME002" ~ "Cirugia",
      clave_del_puesto %in% c("EN002", "EN005") ~ "Enfermeria quirurgica",
      clave_del_puesto %in% c("PA022", "PA020") ~ "Chofer",
      TRUE ~ NA_character_)) %>% 
  filter(!is.na(puesto_arm))

resumen_equipos <- base_final %>% 
  count(estado_ancla, cluster_id, puesto_arm, name = "n") %>% 
  tidyr::pivot_wider(
    names_from = puesto_arm,
    values_from = n,
    values_fill = 0) %>% 
  clean_names() %>% 
  mutate(
    equipo_itinerante = pmin(
      anestesiologia,
      cirugia,
      medicina_general,
      enfermeria_quirurgica)) %>% 
  select(estado_ancla, cluster_id, equipo_itinerante)


base_final <- base_final %>% 
  left_join(
    resumen_equipos,
    by = c("estado_ancla", "cluster_id"))


total_equipos <- base_final %>% 
  distinct(estado_ancla, cluster_id, equipo_itinerante) %>% 
  summarise(
    total_equipos = sum(equipo_itinerante, na.rm = TRUE))

val <- inner_join(
  team_completos,
  base_final,
  by = intersect(names(team_completos), names(base_final)))


base_final <- base_final %>% 
  anti_join(
    team_completos,
    by = intersect(names(team_completos), names(base_final))) %>% 
  arrange(estado_ancla,clave_del_puesto,clues_ancla,nombre_del_ancla,
          curp)

writexl::write_xlsx(
  base_final,
  "C:/Users/brittany.pereo/Downloads/casos nuevos.xlsx")

val2 <- inner_join(base_final,team_completos,
                   by = intersect(names(base_final), names(team_completos)))



# BASE COMPLETA POR CLUSTER -----------------------------------------------
puestos_requeridos <- tibble::tribble(
  ~puesto_arm,                 ~codigo_cnpm, ~nombre_del_puesto,
  "Cirugia",                   "ME002",      "Cirujano",
  "Anestesiologia",            "ME001",      "Anestesiólogo",
  "Enfermeria quirurgica",     "EN005",      "Enfermera quirúrgica",
  "Chofer",                    "PA020",      "Chofer",
  "Medicina General",          "MG001",      "Médico general"
)

orden_puestos <- c("Cirugia", "Anestesiologia", "Enfermeria quirurgica",
                   "Chofer", "Medicina General")

clusters_alex <- base_alex %>%
  transmute(cluster_id = str_trim(nombre_cluster),
            clues_ancla, nombre_del_ancla = ancla_nombre) %>%
  filter(!is.na(cluster_id), cluster_id != "") %>%
  distinct(cluster_id, .keep_all = TRUE) %>%
  mutate(orden_cluster = as.integer(str_extract(cluster_id, "\\d+$"))) %>%
  arrange(clues_ancla, orden_cluster, cluster_id)

validacion_clusters_alex <- clusters_alex %>%
  summarise(total_clusters = n())

base_necesidad_375 <- clusters_alex %>%
  tidyr::crossing(puestos_requeridos) %>%
  arrange(clues_ancla,orden_cluster,cluster_id,
          match(puesto_arm, orden_puestos)) %>% 
  group_by(cluster_id, puesto_arm) %>%
  mutate(slot = row_number()) %>%
  ungroup()

validacion_base_necesidad <- base_necesidad_375 %>%
  summarise(total_renglones = n())

candidatos_uas <- base_final %>%
  rename(cnpm = clave_del_puesto) %>% 
  mutate(curp_limpia = limpiar_curp(curp),
         curp = curp_limpia,
         cluster_id = str_trim(cluster_id),
         cnpm = str_trim(str_to_upper(cnpm)),
         estatus_uas = str_trim(str_to_upper(estatus_uas))) %>% 
  anti_join(team_completos_oficiales %>% distinct(curp_limpia),
            by = "curp_limpia") %>% 
  anti_join(team_completos %>% 
              mutate(curp_limpia = limpiar_curp(curp)) %>% 
              distinct(cluster_id, curp_limpia),
            by = c("cluster_id", "curp_limpia")) %>% 
  mutate(puesto_arm = case_when(
    cnpm == "MG001" ~ "Medicina General",
    cnpm == "ME001" ~ "Anestesiologia",
    cnpm == "ME002" ~ "Cirugia",
    cnpm %in% c("EN002", "EN005") ~ "Enfermeria quirurgica",
    cnpm %in% c("PA022", "PA020") ~ "Chofer",
    TRUE ~ NA_character_),
    fuente_candidato = "Base validada UAS",
    prioridad_estatus = case_when(
      estatus_uas == "APROBADO" ~ 1L,
      is.na(estatus_uas) | estatus_uas == "" ~ 2L,
      TRUE ~ 3L),
    prioridad_cnpm = case_when(
      puesto_arm == "Enfermeria quirurgica" & cnpm == "EN005" ~ 1L,
      puesto_arm == "Enfermeria quirurgica" & cnpm == "EN002" ~ 2L,
      puesto_arm == "Chofer" & cnpm == "PA020" ~ 1L,
      puesto_arm == "Chofer" & cnpm == "PA022" ~ 2L,
      TRUE ~ 1L)) %>%
  filter(!is.na(puesto_arm),
         !is.na(cluster_id),
         cluster_id != "", cluster_id != "Sin match en cluster",
         is.na(estatus_uas) | estatus_uas == "" | estatus_uas == "APROBADO") %>%
  arrange(
    cluster_id,
    puesto_arm,
    prioridad_estatus,
    prioridad_cnpm,
    nombre
  ) %>%
  group_by(cluster_id, puesto_arm) %>%
  mutate(slot = row_number()) %>%
  ungroup() %>%
  select(cluster_id, puesto_arm, slot, nombre, curp,
         enlace_a_carpeta, estatus_uas, fuente_candidato,
         codigo_cnpm_uas = cnpm, nombre_del_puesto_uas = puesto)

base_cluster_375_con_uas <- base_necesidad_375 %>%
  left_join(candidatos_uas, by = c("cluster_id", "puesto_arm", "slot")) %>%
  mutate(ocupado_con_uas = !is.na(curp),
         estatus_ocupacion = case_when(
           ocupado_con_uas ~ "Cubierto con candidato UAS",
           TRUE ~ "Vacante / sin candidato UAS"),
         codigo_cnpm = coalesce(codigo_cnpm_uas, codigo_cnpm),
         nombre_del_puesto = coalesce(nombre_del_puesto_uas, nombre_del_puesto)) %>%
  select(cluster_id,clues_ancla, nombre_del_ancla, codigo_cnpm, nombre_del_puesto,
         puesto_arm, nombre, curp, enlace_a_carpeta, estatus_uas, fuente_candidato,
         ocupado_con_uas, estatus_ocupacion)

validacion_general <- base_cluster_375_con_uas %>%
  summarise(
    total_renglones = n(),
    total_clusters = n_distinct(cluster_id),
    cubiertos = sum(ocupado_con_uas, na.rm = TRUE),
    vacantes = sum(!ocupado_con_uas, na.rm = TRUE))

validacion_por_puesto <- base_cluster_375_con_uas %>%
  count(puesto_arm, estatus_ocupacion)

validacion_renglones_por_cluster <- base_cluster_375_con_uas %>%
  count(cluster_id) %>%
  count(n, name = "clusters_con_ese_numero_de_renglones")

resumen_por_cluster <- base_cluster_375_con_uas %>%
  group_by(cluster_id, clues_ancla, nombre_del_ancla) %>%
  summarise(
    puestos_requeridos = n(),
    puestos_cubiertos = sum(ocupado_con_uas, na.rm = TRUE),
    puestos_vacantes = sum(!ocupado_con_uas, na.rm = TRUE),
    puestos_faltantes = paste(
      nombre_del_puesto[!ocupado_con_uas],
      collapse = ", "),
    .groups = "drop")

clusters_incompletos <- resumen_por_cluster %>%
  filter(puestos_vacantes > 0)

universo_uas <- candidatos_uas %>%
  distinct(curp, .keep_all = TRUE)

asignados_final <- base_cluster_375_con_uas %>%
  filter(!is.na(curp)) %>%
  distinct(curp)

no_asignados_uas <- universo_uas %>%
  anti_join(asignados_final, by = "curp") %>%
  mutate(
    resultado_validacion = "No asignado en base",
    motivo_probable = "Sin espacio disponible en cluster/puesto")

asignados_final_detalle <- base_cluster_375_con_uas %>%
  filter(!is.na(curp)) %>%
  distinct(curp, .keep_all = TRUE) %>%
  select(
    curp,
    cluster_id_asignado = cluster_id,
    puesto_arm_asignado = puesto_arm)

auditoria_uas <- candidatos_uas %>%
  left_join(asignados_final_detalle, by = "curp") %>%
  mutate(
    resultado = case_when(
      !is.na(cluster_id_asignado) ~ "Asignado en base",
      TRUE ~ "No asignado en base"),
    motivo_probable = case_when(
      resultado == "Asignado en base" ~ "Asignado correctamente",
      TRUE ~ "Sin espacio disponible en cluster/puesto"))

clusters_alex_no_en_375 <- clusters_alex %>%
  anti_join(
    base_cluster_375_con_uas %>% distinct(cluster_id),
    by = "cluster_id")

clusters_uas_no_en_alex <- candidatos_uas %>%
  filter(
    !is.na(cluster_id),
    cluster_id != "",
    cluster_id != "Sin match en cluster") %>%
  distinct(cluster_id) %>%
  anti_join(
    clusters_alex %>% distinct(cluster_id),
    by = "cluster_id")

resumen_no_asignados_uas <- no_asignados_uas %>%
  count(puesto_arm, name = "no_asignados")

resumen_auditoria_uas <- auditoria_uas %>%
  count(resultado, puesto_arm)

base_cluster_375_con_uas <- base_cluster_375_con_uas %>% 
  mutate(curp_limpia = limpiar_curp(curp)) %>% 
  anti_join(
    team_completos %>% 
      mutate(curp_limpia = limpiar_curp(curp)) %>% 
      distinct(cluster_id, curp_limpia),
    by = c("cluster_id", "curp_limpia")) %>% 
  select(-curp_limpia)

val <- full_join(
  base_cluster_375_con_uas,
  team_completos,
  by = c("cluster_id", "clues_ancla", "nombre_del_ancla",
         "nombre", "curp", "enlace_a_carpeta", "estatus_uas")
)

val1 <- base_alex %>%
  anti_join(
    val,
    by = c("nombre_cluster" = "cluster_id")
  )
writexl::write_xlsx(
  list(
    base_cluster = base_cluster_375_con_uas,
    resumen_por_cluster = resumen_por_cluster,
    clusters_incompletos = clusters_incompletos,
    # no_asignados_uas = no_asignados_uas,
    # auditoria_uas = auditoria_uas,
    # validacion_general = validacion_general,
    # validacion_por_puesto = validacion_por_puesto,
    # validacion_renglones_por_cluster = validacion_renglones_por_cluster,
    # clusters_alex_no_en_375 = clusters_alex_no_en_375,
    # clusters_uas_no_en_alex = clusters_uas_no_en_alex,
    # resumen_no_asignados_uas = resumen_no_asignados_uas,
    resumen_auditoria_uas = resumen_auditoria_uas
  ),
  "C:/Users/brittany.pereo/Downloads/base_clusters_uas_validada.xlsx"
)


# ENTIDADES A LAS QUE LES FALTA MAS DEL 75% -------------------------------
basa_cluster_id <- base_alex_original %>% 
  clean_names() %>% 
  st_drop_geometry() %>% 
  data.table() %>% 
  mutate(clues_ancla = str_remove(nombre_cluster, "_\\d+$")) %>% 
  distinct(clues_ancla, nombre_cluster, ancla_entidad,
           ancla_nombre) %>% 
  filter(!is.na(clues_ancla))

unique(base_cluster_375_con_uas$cluster_id)
unique(team_completos$cluster_id)

clusters_completos <- team_completos %>%
  distinct(cluster_id) %>%
  rename(nombre_cluster = cluster_id)

clusters_incompletos <- base_cluster_375_con_uas %>%
  distinct(cluster_id) %>%
  rename(nombre_cluster = cluster_id)

universo_clusters <- basa_cluster_id %>%
  distinct(ancla_entidad, nombre_cluster)

resumen_entidad <- universo_clusters %>%
  left_join(
    clusters_incompletos %>%
      mutate(incompleto = TRUE),
    by = "nombre_cluster"
  ) %>%
  left_join(
    clusters_completos %>%
      mutate(completo = TRUE),
    by = "nombre_cluster"
  ) %>%
  mutate(
    incompleto = coalesce(incompleto, FALSE),
    completo = coalesce(completo, FALSE)
  ) %>%
  group_by(ancla_entidad) %>%
  summarise(
    clusters_totales = n(),
    clusters_completos = sum(completo),
    clusters_incompletos = sum(incompleto),
    pct_completos = clusters_completos / clusters_totales,
    pct_incompletos = clusters_incompletos / clusters_totales,
    .groups = "drop"
  ) %>%
  arrange(desc(pct_incompletos))

writexl::write_xlsx(resumen_entidad,
                    "C:/Users/brittany.pereo/Downloads/resumen_entidad.xlsx")
