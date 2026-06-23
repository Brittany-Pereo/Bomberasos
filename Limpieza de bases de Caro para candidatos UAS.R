library(dplyr)
library(reticulate)
library(sf)
library(janitor)
library(stringr)
library(readxl)
library(readr)

# FUNCIONES ---------------------------------------------------------------
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

    payload = {'curp': str(curp).strip()}

    try:
        r = requests.post(
            'https://us-central1-os-gobierno-de-nuevo-leon.cloudfunctions.net/nuevoLeon-checkCurp',
            data=json.dumps(payload),
            headers=headers,
            verify=False
        )

        if r.status_code == 200:
            out = r.json()
            out['curp'] = str(curp).strip()
            return out

        return {'curp': str(curp).strip(), 'error': 'No encontrada'}

    except Exception as e:
        return {'curp': str(curp).strip(), 'error': str(e)}


def consultar_curps(curps):
    resultados = [consultar_curp(x) for x in curps]
    return pd.DataFrame(resultados)
")

# BASE CLUSTERS -----------------------------------------------------------
base_alex_original <- st_read(
  "C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/80_Basico comunitarios dificil acceso/bases/cluster_19_rutas_geo_long.gpkg"
) %>% 
  clean_names() %>% 
  st_drop_geometry() %>% 
  mutate(clues_ancla = str_remove(nombre_cluster, "_\\d+$")) %>% 
  distinct(clues_ancla, nombre_cluster, ancla_entidad) %>% 
  filter(!is.na(clues_ancla))

catalogo_puestos <- read_excel(
  "C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/Plantilla/catalogos/Catalogo_CNPM_2026_F.xlsx"
) %>% 
  clean_names() %>% 
  select(
    cnpm = codigo_cnpm_26,
    denominacion_de_puesto
  )
puebla <- readxl::read_xlsx(
 "C:/Users/brittany.pereo/Downloads/COMPLETOS ITINERANTES info.xlsx",
  sheet = "candidatos_UAS"
) %>% 
  janitor::clean_names() %>% 
  mutate(
    curp = str_squish(as.character(curp)),
    cnpm = as.character(cnpm),
    clave_puesto = as.character(clave_puesto)) %>%
  filter(estado != "Hidalgo")
  
curp_puebla <- puebla %>% 
  filter(!is.na(curp), curp != "") %>% 
  distinct(curp) %>% 
  pull(curp)

resultado_curps_py_it <- py$consultar_curps(curp_puebla)

base_limpia <- resultado_curps_py_it$to_csv(index = FALSE) %>% 
  readr::read_csv(show_col_types = FALSE) %>% 
  select(
    curp_limpia = curp,
    ape_pat = apePat,
    ape_mat = apeMat,
    nombres
  ) %>% 
  mutate(
    across(everything(), as.character),
    nombre = str_squish(paste(nombres, ape_pat, ape_mat)),
    consulta_endpoint_exitosa = !is.na(nombres) | !is.na(ape_pat) | !is.na(ape_mat),
    estatus_consulta_curp = if_else(
      consulta_endpoint_exitosa,
      "CURP encontrada",
      "CURP no encontrada en endpoint"
    )
  )

puebla_proc <- puebla %>% 
  left_join(
    base_alex_original,
    by = c("clues" = "clues_ancla")
  ) %>% 
  left_join(
    base_limpia,
    by = c("curp" = "curp_limpia")
  ) %>% 
  mutate(
    cnpm = if_else(is.na(cnpm) | cnpm == "", clave_puesto, cnpm)
  ) %>% 
  left_join(
    catalogo_puestos,
    by = "cnpm"
  ) %>% 
  transmute(
    estado_ancla = stringr::str_to_title(estado),
    clues_ancla = clues,
    nombre_del_ancla = unidad_medica,
    nombre,
    curp,
    puesto = denominacion_de_puesto,
    clave_del_puesto = cnpm,
    estatus_uas = revision_uas,
    cluster_id = nombre_cluster,
    enlace_a_carpeta = link_carpeta,
    consulta_endpoint_exitosa,
    estatus_consulta_curp
  )


base_limpia_final <- puebla_proc %>% 
  mutate(
    clave_del_puesto = case_when(
      clave_del_puesto ==  "OP057 CHOFER PROMOTOR POLIVALENTE" ~"PA022",
      clave_del_puesto ==   "ME002 CIRUGIA GENERAL"   ~"ME002",
      TRUE ~ clave_del_puesto),
    
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
  select(-ends_with("_sobrante")) %>% 
  mutate(estado_ancla = str_to_title(estado_ancla))


resumen_team_qx <- base_limpia_final %>% 
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
    team_qx = if_else(equipo_itinerante >= 1, 1L, 0L),
    estado_ancla = str_to_title(estado_ancla)
  )
base_final <- puebla_proc %>% 
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

base_corregida <- base_final %>% 
  select(
    -team_qx,
    -puestos_faltantes,
    -equipo_itinerante_incompleto
  ) %>% 
  mutate(
    estado_ancla = str_to_title(estado_ancla)
  ) %>% 
  arrange(
    estado_ancla,
    clues_ancla,
    clave_del_puesto
  )

write_xlsx(
  list(
    base = base_corregida
  ),
  "C:/Users/brittany.pereo/Downloads/puebla.xlsx"
)

