###--- INFORME DE ECE ---###
## PPT INTERCAMBIO DE SERVICIOS EN NÚMEROS
## - TOTAL
## - CONSULTAS
## - URGENCIAS
## - HOSPITALIZACIÓN
## - CIRUGÍAS
## - UCI

library(arrow)
library(readxl)
library(ggplot2)
library(readr)
library(dplyr)
library(lubridate)
library(data.table)
library(ggplot2)
library(tidyr)
library(janitor)
library(stringr)
library(openxlsx)
library(officer)
library(flextable)


# USUARIO 
home <- Sys.getenv("USERPROFILE")
base_onedrive <- file.path(home,"IMSS-BIENESTAR") ##MODIFICAR SEGÚN CÓMO SALGA TU ONEDRIVE, 
# base_onedrive <- file.path(home,"IMSS-BIENESTAR") 

#===============================================================
# RUTAS
ruta_actual <- file.path(base_onedrive,
                         "División de Procesamiento de información - Proyectos",
                         "78_transicion sistemas prod",
                         "data_raw",
                         "EDS actual")

ruta_salida <- file.path(base_onedrive,
                         "División de Procesamiento de información - Proyectos",
                         "78_transicion sistemas prod",
                         "informe")


#===============================================================
# CARGA DE ARCHIVOS
leer_parquet_mas_reciente <- function(carpeta, nombre_archivo) {
  
  archivos <- list.files(
    path = carpeta,
    pattern = paste0("^", nombre_archivo, "_\\d{8}\\.parquet$"),
    full.names = TRUE)
  
  if (length(archivos) == 0) {
    stop("No se encontraron archivos para: ", nombre_archivo)
  }
  
  fechas <- stringr::str_extract(basename(archivos), "\\d{8}")
  fechas <- as.Date(fechas, format = "%Y%m%d")
  
  archivo_mas_reciente <- archivos[which.max(fechas)]
  message("Archivo cargado: ", basename(archivo_mas_reciente))
  arrow::read_parquet(archivo_mas_reciente) |>
    janitor::clean_names()
}

moce <- leer_parquet_mas_reciente(ruta_actual, "moce")
cirugia <- leer_parquet_mas_reciente(ruta_actual, "cirugia")
egresos <- leer_parquet_mas_reciente(ruta_actual, "hospital")
egresos_original <- leer_parquet_mas_reciente(ruta_actual, "hospital")
uci <- leer_parquet_mas_reciente(ruta_actual, "uci")
urgencias <- leer_parquet_mas_reciente(ruta_actual, "urgencias")
names(moce)
names(urgencias)
names(cirugia)
names(egresos)
names(uci)
gc()


#===============================================================
# INFORME EN GENERAL

calcular_informe <- function(base, var_clues, var_agregado) {
  agregado <- base[[var_agregado]]
  no_derechohabiente <- str_detect(
    agregado,
    "^0.*ND$")
  data.frame(
    clues = n_distinct(base[[var_clues]], na.rm = TRUE),
    registros = nrow(base),
    atenciones_derechohabientes = sum(no_derechohabiente == FALSE, na.rm = TRUE),
    porcentaje_no_derechohabiencia = round(
      mean(no_derechohabiente, na.rm = TRUE) * 100, 2 ))
}

calcular_informe_agregado <- function(base, var_clues, var_agregado) {
  agregado <- base[[var_agregado]]
  no_derechohabiente <- stringr::str_detect(
    agregado,
    "^0.*ND$")
  
  data.frame(
    clues = dplyr::n_distinct(base[[var_clues]], na.rm = TRUE),
    registros = nrow(base),
    porcentaje_no_derechohabiencia = round(
      mean(no_derechohabiente, na.rm = TRUE) * 100, 2 ))
}

calcular_informe_vigencia <- function(base, var_clues, var_vigencia) {
  
  vigencia <- base[[var_vigencia]]
  
  no_derechohabiente <- vigencia == "No"
  derechohabiente <- vigencia == "Si" | vigencia == "Sí"
  
  data.frame(
    clues = n_distinct(base[[var_clues]], na.rm = TRUE),
    registros = nrow(base),
    atenciones_derechohabientes = sum(derechohabiente, na.rm = TRUE),
    porcentaje_no_derechohabiencia = round(
      mean(no_derechohabiente, na.rm = TRUE) * 100, 2 ))
}


# REVISIÓN
# uci$cve_registro_hos
table(egresos$vigencia, useNA = "ifany")
table(urgencias$vigencia, useNA = "ifany")
table(cirugia$vigencia, useNA = "ifany")
table(cirugia$vigencia, useNA = "ifany")
table(uci$vigencia, useNA = "ifany")
#Se confirma que no hay NAs

informe <- bind_rows(
  Moce = calcular_informe(moce, "clues", "ref_agregado_medico"),
  Egresos = calcular_informe_vigencia(egresos, "clues", "vigencia"),
  Urgencias = calcular_informe_vigencia(urgencias, "clues", "vigencia"),
  Cirugias = calcular_informe_vigencia(cirugia, "clues", "vigencia"),
  UCI = calcular_informe_vigencia(uci, "clues", "vigencia"),
  .id = "base")
names(informe)

informe$texto <- paste0(
  "- ", informe$base, ": ",
  informe$clues, " clues, ",
  informe$registros, " registros y ",
  informe$porcentaje_no_derechohabiencia, "% de no derechohabiencia")
cat(paste(informe$texto, collapse = "\n"))



#===============================================================
### Personas atendidas

curp_modulos <- bind_rows(
  moce      %>% transmute(curp = cve_curp_hash32, modulo = "moce"),
  urgencias %>% transmute(curp = pac_curp_hash32, modulo = "urgencias"),
  cirugia   %>% transmute(curp = curp_hash32,     modulo = "cirugia"),
  egresos   %>% transmute(curp = pac_curp_hash32, modulo = "egresos"),
  uci       %>% transmute(curp = pac_curp_hash32, modulo = "uci"))

curp_unicas_general <- curp_modulos %>%
  filter(!is.na(curp), curp != "") %>%
  distinct(curp)

nrow(curp_unicas_general) # 126,876

urgencias %>%
  summarise(
    total_curps = n_distinct(pac_curp_hash32, na.rm = TRUE))

egresos %>%
  summarise(
    total_curps = n_distinct(pac_curp_hash32, na.rm = TRUE))

curp_unicas_general <- curp_modulos %>%
  filter(!is.na(curp), curp != "") %>%
  distinct(curp)

curp_unicas_modulos <- curp_modulos %>%
  filter(!is.na(curp), curp != "") %>%
  distinct(curp, modulo)

n_distinct(curp_unicas_general$curp)

table(curp_unicas_modulos$modulo)


#===============================================================
# RESUMEN CUADRO -  Intercambio de servicios en números 

# Función para formato Con IMSS: n (%)
formato_con_imss <- function(n_imss, total) {
  paste0(
    format(n_imss, big.mark = ",", scientific = FALSE),
    " (",
    round(n_imss / total * 100, 0),
    "%)"  )
}

# Personas únicas por módulo
personas_consulta <- moce %>%
  filter(!is.na(cve_curp_hash32), cve_curp_hash32 != "") %>%
  summarise(n = n_distinct(cve_curp_hash32)) %>%
  pull(n)

personas_urgencias <- urgencias %>%
  filter(!is.na(pac_curp_hash32), pac_curp_hash32 != "") %>%
  summarise(n = n_distinct(pac_curp_hash32)) %>%
  pull(n)

personas_hospitalizacion <- egresos %>%
  filter(!is.na(pac_curp_hash32), pac_curp_hash32 != "") %>%
  summarise(n = n_distinct(pac_curp_hash32)) %>%
  pull(n)

personas_total <- curp_modulos %>%
  filter(!is.na(curp), curp != "") %>%
  summarise(n = n_distinct(curp)) %>%
  pull(n)

# Extraer datos del informe
datos <- informe %>%
  select(base, registros, atenciones_derechohabientes)

get_total <- function(base_nombre, variable) {
  datos %>%
    filter(base == base_nombre) %>%
    pull({{ variable }})
}

# Valores por módulo
consultas_total <- get_total("Moce", registros)
consultas_imss  <- get_total("Moce", atenciones_derechohabientes)

urgencias_total <- get_total("Urgencias", registros)
urgencias_imss  <- get_total("Urgencias", atenciones_derechohabientes)

hospital_total <- get_total("Egresos", registros)
hospital_imss  <- get_total("Egresos", atenciones_derechohabientes)

cirugias_total <- get_total("Cirugias", registros)
cirugias_imss  <- get_total("Cirugias", atenciones_derechohabientes)

uci_total <- get_total("UCI", registros)
uci_imss  <- get_total("UCI", atenciones_derechohabientes)

atenciones_total <- sum(
  consultas_total,
  urgencias_total,
  hospital_total,
  cirugias_total,
  uci_total,
  na.rm = TRUE)

atenciones_imss <- sum(
  consultas_imss,
  urgencias_imss,
  hospital_imss,
  cirugias_imss,
  uci_imss,
  na.rm = TRUE)

# Cuadro final
cuadro_intercambio <- tibble::tibble(
  `Tipo de atención` = c(
    "Personas atendidas",
    "Atenciones ofrecidas",
    "",
    "Personas en consulta",
    "Consultas",
    "",
    "Personas en urgencias",
    "Atenciones",
    "",
    "Personas en hospitalización",
    "Eventos de hospitalización",
    "Eventos de cirugías",
    "UCI"
  ),
  Total = c(
    format(personas_total, big.mark = ",", scientific = FALSE),
    format(atenciones_total, big.mark = ",", scientific = FALSE),
    "",
    format(personas_consulta, big.mark = ",", scientific = FALSE),
    format(consultas_total, big.mark = ",", scientific = FALSE),
    "",
    format(personas_urgencias, big.mark = ",", scientific = FALSE),
    format(urgencias_total, big.mark = ",", scientific = FALSE),
    "",
    format(personas_hospitalizacion, big.mark = ",", scientific = FALSE),
    format(hospital_total, big.mark = ",", scientific = FALSE),
    format(cirugias_total, big.mark = ",", scientific = FALSE),
    format(uci_total, big.mark = ",", scientific = FALSE)
  ),
  `Con IMSS` = c(
    "---",
    formato_con_imss(atenciones_imss, atenciones_total),
    "",
    "---",
    formato_con_imss(consultas_imss, consultas_total),
    "",
    "---",
    formato_con_imss(urgencias_imss, urgencias_total),
    "",
    "---",
    formato_con_imss(hospital_imss, hospital_total),
    formato_con_imss(cirugias_imss, cirugias_total),
    formato_con_imss(uci_imss, uci_total)
  )
)

cuadro_intercambio

openxlsx::write.xlsx(
  cuadro_intercambio,
  file = file.path(ruta_salida, "Intercambio_servicios_tabla.xlsx"),
  overwrite = TRUE)

#===============================================================
# PPTX

ruta_master <- file.path(
  base_onedrive,
  "División de Procesamiento de información - Proyectos",
  "78_transicion sistemas prod",
  "informe",
  "master.pptx")

ppt <- read_pptx(ruta_master)

### REVISAR PRIMERO LOS LAYOUT
layout_summary(ppt)
# Ver placeholders del layout
plot_layout_properties(
  x = ppt,
  layout = "validados_entidad",
  master = "Tema de Office")

lp <- layout_properties(
  x = ppt,
  layout = "validados_entidad",
  master = "Tema de Office")

lp[, c("type", "id", "ph_label", "offx", "offy", "cx", "cy")]


# Tabla con formato
library(flextable)

ft_cuadro <- flextable(cuadro_intercambio) %>%
  font(fontname = "Noto Sans", part = "all") %>%
  fontsize(size = 18, part = "all") %>%
  line_spacing(space = 1, part = "all") %>%
  valign(valign = "center", part = "all") %>%
  
  # Ancho de columnas en pulgadas
  width(j = 1, width = 11.77 / 2.54) %>%
  width(j = 2, width = 9.11 / 2.54) %>%
  width(j = 3, width = 9.11 / 2.54) %>%
  
  # Alto de filas en pulgadas
  height_all(height = 1.02 / 2.54) %>%
  
  # Alineación por columna
  align(j = 1, align = "left", part = "all") %>%
  align(j = 2:3, align = "center", part = "all") %>%
  
  # Formato de primera fila / encabezado
  bg(i = 1, bg = "#1B5C4E", part = "body") %>%
  color(i = 1, color = "white", part = "body") %>%
  bold(i = 1, bold = TRUE, part = "body") %>%
  
  # Evita que autofit cambie los anchos definidos
  set_table_properties(layout = "fixed")

# TÍTULO
titulo_txt <- "Intercambio de servicios en números"

fp_titulo <- fp_text(
  font.family = "Noto Sans",
  font.size = 22)

titulo_par <- fpar(
  ftext(titulo_txt, prop = fp_titulo))

# Leyenda
fuente_txt <- "Fuente: Bases de datos PHEDS y MOCE. De noviembre del 2025 a la fecha de corte: __________________"

fp_fuente <- fp_text(
  font.family = "Noto Sans",
  font.size = 14)

fuente_par <- fpar(
  ftext(fuente_txt, prop = fp_fuente))

ruta_pptx <- file.path(
"C:/Users/brittany.pereo/Downloads",
  "auto_intercambio_servicios.pptx")

# Crear ppt
ppt <- read_pptx(ruta_master)

ppt <- ppt %>%
  add_slide(
    layout = "validados_entidad",
    master = "Tema de Office" ) %>%
  ph_with(
    value = ft_cuadro,
    location = ph_location_id(id = 12)) %>%
  ph_with(
    value = titulo_par,
    location = ph_location_id(id = 7)) %>% 
  ph_with(
    value = fuente_par,
    location = ph_location_id(id = 5))

print(ppt, target = ruta_pptx)

#===============================================================
