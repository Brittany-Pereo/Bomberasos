library(openxlsx)
library(dplyr)
library(scales)
library(readxl)
library(janitor)
library(ggplot2)
# Base equipos completos------------------------------------------------------
base <- read_xlsx(
  "C:/Users/Cecilia Pereo/Downloads/equipo itinerantes 23062026.xlsx"
) %>% 
  clean_names() %>% 
  mutate(estado_ancla = stringr::str_to_title(estado_ancla))

team_qx_cluster <- base %>% 
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
  group_by(estado_ancla, cluster_id) %>% 
  summarise(
    medicina_general = sum(puesto_arm == "Medicina General"),
    anestesiologia = sum(puesto_arm == "Anestesiologia"),
    cirugia = sum(puesto_arm == "Cirugia"),
    enfermeria_quirurgica = sum(puesto_arm == "Enfermeria quirurgica"),
    chofer = sum(puesto_arm == "Chofer"),
    .groups = "drop"
  ) %>% 
  mutate(
    teams_qx = pmin(
      medicina_general,
      anestesiologia,
      cirugia,
      enfermeria_quirurgica,
      chofer
    )
  )

tabla_cluster_team_qx <- team_qx_cluster %>% 
  transmute(
    estado = estado_ancla,
    cluster_id,
    anestesiologos = anestesiologia,
    enfermeras = enfermeria_quirurgica,
    medicos_cirujanos = cirugia,
    medicos_generales = medicina_general,
    choferes = chofer,
    team_qx = pmin(
      anestesiologia,
      enfermeria_quirurgica,
      medicina_general,
      cirugia))

resumen_estado <- tabla_cluster_team_qx %>% 
  group_by(estado) %>% 
  summarise(
    team_armados = sum(team_qx),
    .groups = "drop"
  )

# Base de Alex ------------------------------------------------------------
base_alex_original <- st_read(
  "C:/Users/Cecilia Pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/80_Basico comunitarios dificil acceso/bases/cluster_19_rutas_geo_long.gpkg"
) 

base_alex <-base_alex_original%>% 
  clean_names() %>% 
  st_drop_geometry() %>% 
  data.table::data.table() %>% 
  mutate(clues_ancla = str_remove(nombre_cluster, "_\\d+$"),
         ancla_entidad = stringr::str_to_title(ancla_entidad)) %>% 
  distinct(clues_ancla, cluster_id =nombre_cluster, entidad = ancla_entidad) %>% 
  filter(!is.na(clues_ancla))

tabla <- tabla_cluster_team_qx %>% 
  select(cluster_id, team_qx) %>% 
  full_join(base_alex, by = "cluster_id") %>% 
  group_by(entidad) %>% 
  summarise(team_qx_necesarios = n_distinct(cluster_id),
            team_qx_existentes = sum(team_qx, na.rm = TRUE),
            pct_avance = ifelse(team_qx_existentes > 0,
                                team_qx_existentes/team_qx_necesarios, 0
                                )) %>% 
  arrange(desc(pct_avance))

  

# Tabla para exportar -----------------------------------------------------
tabla_excel <- tabla %>% 
  mutate(
    pct_avance = pct_avance
  ) %>% 
  arrange(desc(pct_avance))

# Crear workbook ----------------------------------------------------------
wb <- createWorkbook()

addWorksheet(wb, "Avance teams QX")

# Estilos -----------------------------------------------------------------
estilo_titulo <- createStyle(
  fontSize = 16,
  textDecoration = "bold",
  fontColour = "#611232",
  halign = "center"
)

estilo_header <- createStyle(
  fgFill = "#611232",
  fontColour = "white",
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  border = "Bottom"
)

estilo_cuerpo <- createStyle(
  halign = "center",
  valign = "center",
  border = "Bottom",
  borderColour = "#DDDDDD"
)

estilo_entidad <- createStyle(
  halign = "left",
  valign = "center",
  border = "Bottom",
  borderColour = "#DDDDDD"
)

estilo_pct <- createStyle(
  numFmt = "0.0%",
  halign = "center",
  valign = "center",
  border = "Bottom",
  borderColour = "#DDDDDD"
)

# Título ------------------------------------------------------------------
writeData(wb, "Avance teams QX", "Avance de teams quirúrgicos por entidad", startRow = 1, startCol = 1)
mergeCells(wb, "Avance teams QX", cols = 1:4, rows = 1)
addStyle(wb, "Avance teams QX", estilo_titulo, rows = 1, cols = 1:4)

# Tabla -------------------------------------------------------------------
writeData(wb, "Avance teams QX", tabla_excel, startRow = 3, startCol = 1)

addStyle(wb, "Avance teams QX", estilo_header, rows = 3, cols = 1:ncol(tabla_excel), gridExpand = TRUE)
addStyle(wb, "Avance teams QX", estilo_cuerpo, rows = 4:(nrow(tabla_excel) + 3), cols = 2:3, gridExpand = TRUE)
addStyle(wb, "Avance teams QX", estilo_entidad, rows = 4:(nrow(tabla_excel) + 3), cols = 1, gridExpand = TRUE)
addStyle(wb, "Avance teams QX", estilo_pct, rows = 4:(nrow(tabla_excel) + 3), cols = 4, gridExpand = TRUE)

# Nombres bonitos de columnas ---------------------------------------------
colnames(tabla_excel)
writeData(
  wb,
  "Avance teams QX",
  x = data.frame(
    Entidad = tabla_excel$entidad,
    `Team QX necesarios` = tabla_excel$team_qx_necesarios,
    `Team QX existentes` = tabla_excel$team_qx_existentes,
    `% avance` = tabla_excel$pct_avance
  ),
  startRow = 3,
  startCol = 1,
  colNames = TRUE
)

# Reaplicar estilos después de escribir -----------------------------------
addStyle(wb, "Avance teams QX", estilo_header, rows = 3, cols = 1:4, gridExpand = TRUE)
addStyle(wb, "Avance teams QX", estilo_entidad, rows = 4:(nrow(tabla_excel) + 3), cols = 1, gridExpand = TRUE)
addStyle(wb, "Avance teams QX", estilo_cuerpo, rows = 4:(nrow(tabla_excel) + 3), cols = 2:3, gridExpand = TRUE)
addStyle(wb, "Avance teams QX", estilo_pct, rows = 4:(nrow(tabla_excel) + 3), cols = 4, gridExpand = TRUE)

# Semáforo en porcentaje --------------------------------------------------
conditionalFormatting(
  wb, "Avance teams QX",
  cols = 4,
  rows = 4:(nrow(tabla_excel) + 3),
  rule = ">=0.8",
  style = createStyle(bgFill = "#D9EAD3")
)

conditionalFormatting(
  wb, "Avance teams QX",
  cols = 4,
  rows = 4:(nrow(tabla_excel) + 3),
  rule = ">=0.5",
  style = createStyle(bgFill = "#FFF2CC")
)

conditionalFormatting(
  wb, "Avance teams QX",
  cols = 4,
  rows = 4:(nrow(tabla_excel) + 3),
  rule = "<0.5",
  style = createStyle(bgFill = "#F4CCCC")
)

# Detalles finales --------------------------------------------------------
setColWidths(wb, "Avance teams QX", cols = 1:4, widths = c(32, 20, 20, 15))
freezePane(wb, "Avance teams QX", firstActiveRow = 4)
addFilter(wb, "Avance teams QX", rows = 3, cols = 1:4)

saveWorkbook(
  wb,
  "C:/Users/Cecilia Pereo/Downloads/avance_teams_qx_entidad1.xlsx",
  overwrite = TRUE
)
