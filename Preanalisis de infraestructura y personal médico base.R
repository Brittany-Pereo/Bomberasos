library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

vector_clues <- arrow::read_parquet(
  "C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/CLUES/clues.parquet"
) %>%
  transmute(clues_imb, clues_ssa_y_sme) %>%
  {unique(c(.$clues_imb, .$clues_ssa_y_sme))} %>%
  na.omit()
  
data_raw <- readxl::read_xlsx(
  "C:/Users/brittany.pereo/Downloads/BD2005_2025 por CLUES 03072026 ENVIADO.xlsx",
  skip = 2) %>% 
  janitor::clean_names()

data_clean_aliados <- data_raw %>% 
  filter(clues %in% vector_clues) %>% 
  group_by(anio = ano, entidad = nombre_estado) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
            .groups = "drop") %>% 
  mutate(origen = "IMSS Bienestar")

data_clean_enemigos <- data_raw %>% 
  filter(!clues %in% vector_clues) %>% 
  group_by(anio = ano, entidad = nombre_estado) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
            .groups = "drop") %>% 
  mutate(origen = "No IMSS Bienestar")

datos_infra <- bind_rows(data_clean_aliados, data_clean_enemigos) %>% 
  select(anio, entidad, origen, total_de_consultorios,
         camas_area_hospitalizacion_camas_censables,
         camas_en_otras_areas_sin_area_de_hospitalizacion_no_censables)

datos_personal <- bind_rows(data_clean_aliados, data_clean_enemigos) %>% 
  select(anio, entidad, origen, medicos_generales, pediatras,
         ginecoobstetras, medicos_cirujanos, medicos_internistas,
         medicos_oftalmologos, medicos_otorrinonaringologos,
         medicos_traumatologos, medicos_dermatologos,
         medicos_anestesiologos, medicos_psiquiatras, odontologos,
         odontologos_especialistas_incluye_cirujano, medicos_endrocrinologos,
         medicos_gastroenterologos, medicos_cardiologos,
         medicos_en_rehabilitacion_medicina_fisica, medicos_urologos,
         medicos_cirujanos_plasticos_y_reconstructivos, numero_de_medicos_neumologos,
         medicos_neurologos, medicos_oncologos, medicos_hematologos,  
         medicos_urgenciologos, medicos_otras_especialidades,
         total_personal_medico_en_formacion, pasantes_de_medicina,
         pasante_de_odontologia, interno_de_pregrado, medicos_residentes,
         total_medicos_en_otras_labores, medicos_en_labores_administrativas,
         medicos_en_labores_de_ensenanza_e_investigacion, medicos_epidemiologos,
         medicos_anatomo_patologos, medicos_otras_actividades,
         total_enfermeras_en_contacto_con_el_paciente, personal_de_enfermeria_general,
         personal_de_enfermeria_especialista, personal_de_enfermeria_pasante,
         personal_de_enfermeria_auxiliar, total_enfermeras_en_otras_labores,
         numero_personal_de_enfermeria_en_labores_administrativas,
         personal_de_enfermeria_en_labores_de_ensenanza_e_investigacion,
         personal_de_enfermeria_en_otras_actividades)

analisis_infra <- datos_infra %>% 
  pivot_longer(
    cols = -c(anio, entidad, origen),
    names_to = "variable",
    values_to = "valor") %>% 
  group_by(anio, origen, variable) %>% 
  summarise(
    total = sum(valor, na.rm = TRUE),
    .groups = "drop")

analisis_personal <- datos_personal %>% 
  pivot_longer(
    cols = -c(anio, entidad, origen),
    names_to = "variable",
    values_to = "valor") %>% 
  group_by(anio, origen, variable) %>% 
  summarise(
    total = sum(valor, na.rm = TRUE),
    .groups = "drop")

comparacion_infra <- analisis_infra %>% 
  pivot_wider(
    names_from = origen,
    values_from = total,
    values_fill = 0) %>% 
  mutate(
    total_general = `IMSS Bienestar` + `No IMSS Bienestar`,
    pct_imss_bienestar = `IMSS Bienestar` / total_general)

comparacion_personal <- analisis_personal %>% 
  pivot_wider(
    names_from = origen,
    values_from = total,
    values_fill = 0) %>% 
  mutate(
    total_general = `IMSS Bienestar` + `No IMSS Bienestar`,
    pct_imss_bienestar = `IMSS Bienestar` / total_general)

ggplot(analisis_infra, aes(x = anio, y = total, color = origen, group = origen)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ variable, scales = "free_y") +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Infraestructura por año",
    subtitle = "IMSS Bienestar vs No IMSS Bienestar",
    x = NULL,
    y = NULL,
    color = NULL
  ) +
  theme_minimal()

ggplot(analisis_personal, aes(x = anio, y = total, color = origen, group = origen)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ variable, scales = "free_y") +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Personal por año",
    subtitle = "IMSS Bienestar vs No IMSS Bienestar",
    x = NULL,
    y = NULL,
    color = NULL
  ) +
  theme_minimal()
