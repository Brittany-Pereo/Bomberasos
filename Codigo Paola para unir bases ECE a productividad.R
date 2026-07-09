###REVISIÓN DE BASES DE DATOS - ECE MERO:
##BASES CON LA CURP HASHEADA
##

library(dplyr)
library(lubridate)
library(arrow)
library(readxl)
library(ggplot2)
library(readr)
library(data.table)
library(tidyr)
library(janitor)
library(stringr)
library(DBI)
library(duckdb)
library(glue)


#===============================================================
## RUtas
# Cambiar según lo necesario
home <- Sys.getenv("USERPROFILE")

# Carpeta base de OneDrive institucional
base_onedrive <- file.path(home,"IMSS-BIENESTAR")
# base_onedrive <- file.path(home,"IMSS-BIENESTAR")

# Rutas del proyecto
ruta_repo <- file.path(base_onedrive,  "División de Procesamiento de información - Repositorio de Datos")

ruta_actual <- file.path(base_onedrive,
                         "División de Procesamiento de información - Proyectos",
                         "78_transicion sistemas prod",
                         "data_raw",
                         "EDS actual")

ruta_salida <- file.path(base_onedrive,
                         "División de Procesamiento de información - Proyectos",
                         "78_transicion sistemas prod",
                         "data")

ruta_2025 <- file.path(base_onedrive,
                       "División de Procesamiento de información - Proyectos",
                       "78_transicion sistemas prod",
                       "data_raw",
                       "ece_2025")

ruta_proy_89 <- file.path(
  base_onedrive,
  "División de Procesamiento de información - Proyectos",
  "89_correciones_parquets_dn")


#===============================================================
## CARGA DE ARCHIVOS ECE
#===============================================================
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
# --- SEPARACIÓN DE DATOS DE 2025
#===============================================================
separar_2025_2026 <- function(base, var_fecha, nombre_base, carpeta_salida) {
  
  var_fecha <- rlang::ensym(var_fecha)
  
  base <- base %>%
    mutate(
      fecha_tmp = as.POSIXct(!!var_fecha, tz = "America/Mexico_City"),
      anio_tmp = lubridate::year(fecha_tmp))
  
  tabulado <- base %>%
    count(anio_tmp, name = "n") %>%
    filter(anio_tmp %in% c(2025, 2026))
  
  cat("\nTabulado para:", nombre_base, "\n")
  print(tabulado)
  
  base_2025 <- base %>%
    filter(anio_tmp == 2025) %>%
    select(-fecha_tmp, -anio_tmp)
  
  base_2026 <- base %>%
    filter(anio_tmp == 2026) %>%
    select(-fecha_tmp, -anio_tmp)
  
  arrow::write_parquet(
    base_2025,
    sink = file.path(carpeta_salida, paste0(nombre_base, "_2025.parquet")))
  
  return(base_2026)
}

###separamos eso
moce <- separar_2025_2026(
  base = moce,
  var_fecha = fecha_atencion,
  nombre_base = "moce",
  carpeta_salida = ruta_2025)

cirugia <- separar_2025_2026(
  base = cirugia,
  var_fecha = fecha_de_realizacion,
  nombre_base = "cirugia",
  carpeta_salida = ruta_2025)

urgencias <- separar_2025_2026(
  base = urgencias,
  var_fecha = fecha_hora_egreso,
  nombre_base = "urgencias",
  carpeta_salida = ruta_2025)

egresos <- separar_2025_2026(
  base = egresos,
  var_fecha = fecha_hora_egreso,
  nombre_base = "egresos",
  carpeta_salida = ruta_2025)

uci <- separar_2025_2026(
  base = uci,
  var_fecha = fecha_hora_ingreso,
  nombre_base = "uci",
  carpeta_salida = ruta_2025)


#===============================================================
# --- PROCEDIMIENTOS
#===============================================================
## CARGA DE ARCHIVO DE PROCEDIMIENTOS
qx_2026 <- read_parquet(
  file.path( ruta_proy_89,
             "finales procedimientos",
             "quirurgicos 2026 nuevo.parquet"))

nrow(qx_2026)  # 315,492; #332,332
# names(qx_2026)
# names(cirugia)
table(qx_2026$eliminado, useNA = "ifany") #CERO

#cargo el catalogo de una
catalogo <- read_excel(
  file.path( ruta_proy_89,
             "bases_soporte",
             "PROCEDIMIENTO_202402.xlsx"))
catalogo <- catalogo %>% 
  clean_names()
names(catalogo)
## -- NOTAS --
## IDENTIFICACIÓN DE VARIABLES CIE-9 -> PROCEDIMIENTOS
# qx_2026$cod_cie_procedimiento
# qx_2026$descrip_procedimiento
# cirugia$procedimiento_principal_de_la_solicitud ## viene junto, hay que separarlo en dos columnas

## IDENTIFICACIÓN DE VARIABLES CIE-10 -> DIAGNÓSTICO
# qx_2026$cod_cie_afeccion_principal
# qx_2026$desc_afeccion_principal
# cirugia$cie10_diagnostico_principal_de_la_solicitud  ## viene junto, hay que separarlo en dos columnas

##Generación de variables con el mismo nombre y eliminación de las canceladas
cirugia <- cirugia %>%
  filter(is.na(motivo_de_cancelacion)) %>%  #Se eliminan 995 que están canceldas
  mutate(
    cod_cie_procedimiento = if_else(
      is.na(procedimiento_principal_de_la_solicitud),
      NA_character_,
      substr(procedimiento_principal_de_la_solicitud, 1, 4)
    ),
    
    descrip_procedimiento = if_else(
      is.na(procedimiento_principal_de_la_solicitud),
      NA_character_,
      trimws(substr(
        procedimiento_principal_de_la_solicitud,
        6,
        nchar(procedimiento_principal_de_la_solicitud)
      ))
    ),
    
    cod_cie_afeccion_principal = if_else(
      is.na(cie10_diagnostico_principal_de_la_solicitud),
      NA_character_,
      substr(cie10_diagnostico_principal_de_la_solicitud, 1, 4)
    ),
    
    desc_afeccion_principal = if_else(
      is.na(cie10_diagnostico_principal_de_la_solicitud),
      NA_character_,
      trimws(substr(
        cie10_diagnostico_principal_de_la_solicitud,
        6,
        nchar(cie10_diagnostico_principal_de_la_solicitud)
      ))
    )
  )

#Preguntar  Armando si esto se tiene que fusionar y hacer el mismo filtrado para Q D T
# cirugia
# egresos
# Dice Armando que solo se debe tomar la de cirugías

qx_2026_2 <- qx_2026 %>%
  filter(eliminado !=1) %>% 
  select(
    clues,
    curp_hash32,
    extracto_curp,
    folio,
    fecha_ingreso,
    fecha_egreso,
    fecha_insert,
    cod_cie_procedimiento,
    descrip_procedimiento,
    cod_cie_afeccion_principal,
    desc_afeccion_principal )

#limpiar cirugia cuando no tienen curp o fecha de realización
nrow(cirugia) #
cirugia_curp <- cirugia %>%
  filter(
    !is.na(fecha_de_realizacion),
    !is.na(curp_hash32),
    trimws(curp_hash32) != "",
    trimws(fecha_de_realizacion) != "",
    year(as.Date(fecha_de_realizacion)) == 2026 )

nrow(cirugia_curp) # 4,156 -> después de mandar a hash32, quedó en 4396
# probablemente, porque ahora se eliminaron tmb las curps inválidas

cirugia_curp <- cirugia_curp %>% 
  select(fecha_de_realizacion,curp_hash32, everything())
#Entonces, para hacer el match con la base de cirugías, tendremos que revisar
# que cirugia$fecha_de_realizacion se encuentre entre así:

# cirugia$fecha_de_realizacion >= qx_2026_2$fecha_ingreso &
#   cirugia$fecha_de_realizacion <= qx_2026_2$fecha_egreso

#homologar formatos de fechas
qx_2026_2 <- qx_2026_2 %>%
  mutate(
    fecha_ingreso = as.Date(fecha_ingreso),
    fecha_egreso = as.Date(fecha_egreso)  )

# cirugia_curp <- cirugia_curp %>%
#   mutate(
#     extracto_curp = substr(curp, 5, 13),
#     fecha_realizacion_dia = as.Date(fecha_de_realizacion)
#   )

#revisar la info

# 1. Identificar cirugías de cirugia_curp que ya están cubiertas por qx_2026_2
cirugia_ya_en_qx <- cirugia_curp %>%
  inner_join(
    qx_2026_2 %>%
      select(clues, curp_hash32, fecha_ingreso, fecha_egreso),
    by = c("clues", "curp_hash32"),
    relationship = "many-to-many"
  ) %>%
  filter(
    fecha_de_realizacion >= fecha_ingreso,
    fecha_de_realizacion <= fecha_egreso
  ) %>%
  distinct(clues, curp_hash32, fecha_de_realizacion)

# 2. Quedarnos solo con cirugías que NO están cubiertas en qx_2026_2
cirugia_faltante <- cirugia_curp %>%
  anti_join(
    cirugia_ya_en_qx,
    by = c("clues", "curp_hash32", "fecha_de_realizacion"))

nrow(cirugia_faltante) #2,565

# 3. Adaptarlas a la estructura de qx_2026_2
cirugia_faltante_qx <- cirugia_faltante %>%
  # mutate(
  # sexo_homologado = case_when(
  # sexo == "M" ~ "1",
  # sexo == "F" ~ "2",
  # sexo %in% c("1", "2") ~ sexo,
  # TRUE ~ NA_character_
  # ),
  # ) %>%
  transmute(
    clues = clues,
    curp_hash32 = curp_hash32,
    # extracto_curp = extracto_curp,
    sexo_homologado = as.character(sexo),
    fecha_egreso = fecha_de_realizacion,
    folio = NA_character_,
    fecha_ingreso = fecha_de_realizacion,
    fecha_insert = NA,
    cod_cie_procedimiento = cod_cie_procedimiento,
    descrip_procedimiento = descrip_procedimiento,
    cod_cie_afeccion_principal = cod_cie_afeccion_principal,
    desc_afeccion_principal = desc_afeccion_principal,
    fuente = "cirugia"
  )

# eliminar obs incompletas
cirugia_faltante_qx <- cirugia_faltante_qx %>%
  filter(
    !is.na(curp_hash32),
    trimws(curp_hash32) != "",
    !is.na(fecha_egreso))

nrow(cirugia_faltante_qx) # 2,376

# 4. Unir ambas bases
qx_2026_3 <- bind_rows(
  qx_2026_2 %>%
    mutate(fuente = "qx_2026_2"),
  cirugia_faltante_qx)

#revission
cirugia_faltante_qx %>%
  summarise(
    n = n(),
    sin_extracto_curp = sum(is.na(curp_hash32) | trimws(curp_hash32) == ""),
    sin_fecha_egreso = sum(is.na(fecha_egreso)))

names(qx_2026_3)
table(qx_2026_3$fuente) #    
addmargins(table(qx_2026_3$fuente)) # 387,379 

#HOmologar las fechas y mandar fecha insert igual afecha egreso
qx_2026_3 <- qx_2026_3 %>%
  mutate(
    fecha_insert = if_else(
      is.na(fecha_insert),
      fecha_egreso,
      fecha_insert
    ),
    across(
      c(fecha_ingreso, fecha_egreso, fecha_insert),
      as.Date
    ))

#revisar una fecha que está mal:
# View(
#   cirugia %>%
#     filter(
#       clues == "CSIMB002980",
#       cod_cie_procedimiento == "740X",
#       cod_cie_afeccion_principal == "Z321"
#     )
# )

#arreglar el error de la fecha 0203-03-08, ya se revisó iy corroborró que 
# la fecha correcta es 2026-03-08
qx_2026_3 <- qx_2026_3 %>%
  mutate(
    fecha_ingreso = if_else(
      fecha_ingreso == as.Date("0203-03-08"), 
      as.Date("2026-03-08"), 
      fecha_ingreso
    ),
    fecha_egreso = if_else(
      fecha_egreso == as.Date("0203-03-08"), 
      as.Date("2026-03-08"), 
      fecha_egreso
    ),
    fecha_insert = if_else(
      fecha_insert == as.Date("0203-03-08"), 
      as.Date("2026-03-08"), 
      fecha_insert
    ))


#el pinche catalogo
qx_2026_3 <- qx_2026_3 %>%
  left_join(
    catalogo %>%
      select(catalog_key, procedimiento_type),
    by = c("cod_cie_procedimiento" = "catalog_key")
  ) %>%
  rename(
    proced_catalogo = procedimiento_type
  )
table(qx_2026_3$proced_catalogo, useNA = "ifany")
nrow(qx_2026_3) #334,726

#salvar
arrow::write_parquet(
  qx_2026_3,
  sink = file.path(ruta_salida, "proc_qx_con_ECE_2026.parquet"))


#===============================================================
# --- CONSULTAS
#===============================================================

#Extraer datos del query de consultas
# prueba <- read_parquet("C:/Users/angelica.gonzalezl/OneDrive - IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/Productividad/Bases originales/2026_daniel/salud_mental_01_01_2026_a_20_05_2026.parquet")
# names(prueba)
#Clasificar el tipo de consultas: general especialidad
names(moce)
#esto porque el pend de daniel, le cambia el nombre a la variable hdlv
moce <- moce %>%
  rename_with(
    ~ "curp_hash32",
    .cols = any_of("cve_curp_hash32"))

table(moce$estatus_cita, useNA = "ifany")
moce <- moce %>% 
  filter(
    coalesce(estatus_cita, "") != "No se Presentó")

moce <- moce %>% 
  mutate(
    tipo_consulta = case_when(
      clave_de_servicio %in% c("MG01", # Medicina General
                               "6301", # Psicología
                               "9999", # No Especificada
                               "PREC", # Pre Consulta
                               "5001", # Consultas en Primer Contacto
                               "6601"  # Nutrición y Dietítica
      ) ~ "general",
      TRUE ~ "especialidad"))

table(moce$tipo_consulta)

moce_match <- moce %>%
  mutate(
    clues = as.character(clues),
    fecha_consulta = as.Date(ymd_hms(fecha_atencion))
    # extracto_curp = substr(cve_curp, 5, 13)
  ) %>%
  filter(year(fecha_consulta) == 2026) %>%
  as.data.frame()

#conexión DDB para unir las varias bases:

con <- dbConnect(duckdb::duckdb())

ultimo_miercoles <- function(fecha = Sys.Date()) {
  fecha <- as.Date(fecha)
  dias <- as.numeric(format(fecha, "%u")) - 3
  
  if (dias == 0) {
    return(fecha - 7)
  } else {
    dias_restar <- ifelse(dias > 0, dias, dias + 7)
    return(fecha - dias_restar)
  }
}

fecha_actual <- ultimo_miercoles() |> format("%d_%m")
fecha_de_corte <- ultimo_miercoles()

ruta <- file.path(
  "C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR",
  "División de Procesamiento de información - Repositorio de Datos",
  "Productividad",
  "Bases originales",
  "2026_daniel")

query_crear_vista <- glue("
CREATE OR REPLACE VIEW consultas_2026 AS
SELECT
  clues,
  servicio_atencion,
  COALESCE(CAST(fecha_insert AS DATE), CAST(fecha_consulta AS DATE)) AS fecha_insert,
  CAST(fecha_consulta AS DATE) AS fecha_consulta,
  curp_prestador, 
  CASE WHEN filename LIKE '%salud_mental%' THEN 'salud mental'
    WHEN filename LIKE '%salud_bucal%' THEN 'salud bucal'
    WHEN filename LIKE '%planificacion_familiar%' THEN 'planificacion familiar'
    WHEN filename LIKE '%consulta_externa%' THEN 'consulta externa'
  END AS fuente,
  edad,
  sexo,
  tipo_personal,
  codigo_cie_diagnostico1,
  descripcion_diagnostico1,
  teleconsulta,
  clave_edad,
  extracto_curp,
  curp_invalida_razon,
  curp_hash32,
  tipo_consulta
FROM parquet_scan(
  [
    '{ruta}/salud_mental_01_01_2026_a_{fecha_actual}_2026.parquet',
    '{ruta}/salud_bucal_01_01_2026_a_{fecha_actual}_2026.parquet',
    '{ruta}/planificacion_familiar_01_01_2026_a_{fecha_actual}_2026.parquet',
    '{ruta}/consulta_externa_01_01_2026_a_{fecha_actual}_2026.parquet'
  ],
  union_by_name = TRUE
)
WHERE fecha_insert IS NOT NULL
  AND CAST(fecha_insert AS DATE) <= DATE '{fecha_de_corte}'
")

dbExecute(con, query_crear_vista)

dbWriteTable(
  con,
  "moce_match",
  moce_match,
  overwrite = TRUE)

dbExecute(con, "
CREATE OR REPLACE VIEW consultas_2026_personas AS
SELECT DISTINCT
  CAST(clues AS VARCHAR) AS clues,
  CAST(fecha_consulta AS DATE) AS fecha_consulta,
  CAST(curp_hash32  AS VARCHAR) AS curp_hash32 
FROM consultas_2026
WHERE clues IS NOT NULL
  AND fecha_consulta IS NOT NULL
  AND extracto_curp IS NOT NULL
")

dbGetQuery(con, "
SELECT
  COUNT(*) AS total_moce,
  COUNT(c.curp_hash32) AS ya_existen_en_consultas_2026,
  COUNT(*) - COUNT(c.curp_hash32) AS faltan_por_agregar
FROM moce_match m
LEFT JOIN consultas_2026_personas c
  ON CAST(m.clues AS VARCHAR) = c.clues
 AND m.fecha_consulta = c.fecha_consulta
 AND CAST(m.curp_hash32 AS VARCHAR) = c.curp_hash32
")

#Falta por agregar: 33,582
## extraer
moce_faltantes <- dbGetQuery(con, "
SELECT m.*
FROM moce_match m
LEFT JOIN consultas_2026_personas c
  ON CAST(m.clues AS VARCHAR) = c.clues
 AND m.fecha_consulta = c.fecha_consulta
 AND CAST(m.curp_hash32 AS VARCHAR) = c.curp_hash32
WHERE c.curp_hash32 IS NULL
")

nrow(moce_faltantes) ## 33,582, aumentó respecto al anterior que no usaba hash32
names(moce_faltantes)
dbGetQuery(con, "
DESCRIBE consultas_2026
")

#homologar variables
moce_faltantes_homologado <- moce_faltantes %>%
  transmute(
    clues = as.character(clues),
    servicio_atencion = as.character(desc_servicio),
    fecha_insert = as.Date(fecha_consulta),
    fecha_consulta = as.Date(fecha_consulta),
    curp_prestador = as.character(matricula),
    edad = as.character(NA),
    sexo = as.character(NA),
    tipo_personal = as.character(tipo_medico),
    codigo_cie_diagnostico1 = as.character(x10_principal),
    descripcion_diagnostico1 = as.character(ocasion_principal),
    teleconsulta = as.character(NA),
    clave_edad = as.character(NA),
    # extracto_curp = as.character(extracto_curp),
    curp_invalida_razon = as.character(NA),
    curp_hash32 = as.character(curp_hash32),
    tipo_consulta = as.character(tipo_consulta))

#subirlo a DDB
dbWriteTable(
  con,
  "moce_faltantes_homologado",
  moce_faltantes_homologado,
  overwrite = TRUE)

## consultas originales + las faltantes de moce
dbExecute(con, "
CREATE OR REPLACE VIEW consultas_2026_con_moce AS

SELECT 
  clues,
  servicio_atencion,
  fecha_insert,
  fecha_consulta,
  curp_prestador,
  fuente,
  edad,
  sexo,
  tipo_personal,
  codigo_cie_diagnostico1,
  descripcion_diagnostico1,
  teleconsulta,
  clave_edad,
  -- extracto_curp,
  curp_invalida_razon,
  curp_hash32,
  tipo_consulta
FROM consultas_2026

UNION ALL

SELECT 
  clues,
  servicio_atencion,
  fecha_insert,
  fecha_consulta,
  curp_prestador,
  'moce' AS fuente,
  edad,
  sexo,
  tipo_personal,
  codigo_cie_diagnostico1,
  descripcion_diagnostico1,
  teleconsulta,
  clave_edad,
  -- extracto_curp,
  curp_invalida_razon,
  curp_hash32,
  tipo_consulta
FROM moce_faltantes_homologado
")

#Una pequeña rev de las fechas
dbGetQuery(con, "
SELECT 
  fuente,
  COUNT(*) AS n,
  SUM(CASE WHEN fecha_insert IS NULL THEN 1 ELSE 0 END) AS n_fecha_insert_null,
  SUM(CASE WHEN fecha_consulta IS NULL THEN 1 ELSE 0 END) AS n_fecha_consulta_null
FROM consultas_2026_con_moce
GROUP BY fuente
ORDER BY fuente
")

##cuantas vienen de moce
dbGetQuery(con, "
SELECT
  fuente,
  COUNT(*) AS total
FROM consultas_2026_con_moce
GROUP BY fuente

UNION ALL
SELECT
  'TOTAL' AS fuente,
  COUNT(*) AS total
FROM consultas_2026_con_moce
")

#Guardar

archivo_salida <- file.path(ruta_salida, "consultas_con_ECE_2026.parquet")

query_exportar <- glue::glue("
COPY consultas_2026_con_moce
TO '{archivo_salida}'
(FORMAT PARQUET)
")

dbExecute(con, query_exportar)

dbDisconnect(con, shutdown = TRUE)


#===============================================================
# --- EGRESOS
#===============================================================

#VARS DE CIE10:
#EGRESOS ECE: dx_prin_egreso
#EGRESOS_FINAL: cod_cie_afeccion_principal, desc_afeccion_principal

# 1. Ruta del parquet grande
ruta_egresos_final <- file.path(
  ruta_proy_89,
  "finales egresos",
  "egresos 2026 nuevo.parquet")

# 2. Conexión
con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")

# 3. Abrir egresos_final como vista
dbExecute(con, paste0("
  CREATE VIEW egresos_final AS
  SELECT * FROM read_parquet('", ruta_egresos_final, "')
"))

# 4. Ver variables de egresos_final
vars_final <- dbGetQuery(con, "DESCRIBE egresos_final")
vars_final_names <- vars_final$column_name
vars_final_names

egresos <- egresos %>%
  mutate(fecha_egreso = as.Date(fecha_hora_egreso))

egresos <- egresos %>%
  rename_with(
    ~ "curp_hash32",
    .cols = any_of("pac_curp_hash32") )

egresos <- egresos %>%
  filter(
    !is.na(clues),
    !is.na(curp_hash32),
    !is.na(fecha_egreso),
    year(as.Date(fecha_egreso)) == 2026)

egresos <- egresos %>%
  mutate(fecha_egreso = as.Date(fecha_hora_egreso))

#agregamos las variables que son del cod cie
vars_extra_final <- c(
  "cod_cie_afeccion_principal",
  "desc_afeccion_principal",
  "fecha_insert")

vars_extra_egresos <- c("dx_prin_egreso")

vars_comunes_base <- intersect(names(egresos), vars_final_names)

vars_comunes_base <- setdiff(
  vars_comunes_base,
  c(vars_extra_final, vars_extra_egresos))

dbExecute(con, paste0("
  CREATE OR REPLACE VIEW egresos_final_comun AS
  SELECT
    ", paste(vars_comunes_base, collapse = ", "), ",
    cod_cie_afeccion_principal,
    desc_afeccion_principal,
    fecha_insert,
    'egresos_final' AS fuente
  FROM egresos_final
"))

egresos_match <- egresos %>%
  filter(
    !is.na(clues),
    !is.na(curp_hash32),
    !is.na(fecha_egreso)
  ) %>%
  select(
    all_of(vars_comunes_base),
    dx_prin_egreso
  ) %>%
  mutate(
    cod_cie_afeccion_principal = dx_prin_egreso,
    desc_afeccion_principal = NA_character_,
    fecha_insert = as.POSIXct(fecha_egreso, tz = "UTC"),
    fuente = "egresos_ece"
  ) %>%
  select(
    all_of(vars_comunes_base),
    cod_cie_afeccion_principal,
    desc_afeccion_principal,
    fecha_insert,
    fuente)

copy_to(con, egresos_match, "egresos", overwrite = TRUE)

llaves <- c("clues", "curp_hash32", "fecha_egreso")

egresos_nuevos <- tbl(con, "egresos") %>%
  anti_join(
    tbl(con, "egresos_final_comun"),
    by = llaves
  ) %>%
  collect()

copy_to(con, egresos_nuevos, "egresos_nuevos", overwrite = TRUE)
dbListTables(con)

dbExecute(con, "
  CREATE OR REPLACE TABLE egresos AS
  SELECT
    * EXCLUDE(fecha_insert),
    CAST(fecha_insert AS TIMESTAMPTZ) AS fecha_insert
  FROM egresos
")

nombre_archivo <- "egresos_con_ECE_2026.parquet"
ruta_salida_ece <- file.path(ruta_salida, nombre_archivo)

dbExecute(con, paste0("
  COPY (
    SELECT * FROM egresos_final_comun
    UNION ALL
    SELECT * FROM egresos_nuevos
  )
  TO '", ruta_salida_ece, "'
  (FORMAT PARQUET)
"))


#revisar el tabulado
dbGetQuery(con, paste0("
  SELECT
    SUM(CASE WHEN fuente = 'egresos_final' THEN 1 ELSE 0 END) AS estaban_antes,
    SUM(CASE WHEN fuente = 'egresos_ece' THEN 1 ELSE 0 END) AS agregadas,
    COUNT(*) AS total_final
  FROM read_parquet('", ruta_salida_ece, "')
"))

dbGetQuery(con, paste0("
  DESCRIBE SELECT *
  FROM read_parquet('", ruta_salida_ece, "')
"))

dbGetQuery(con, paste0("
  DESCRIBE SELECT *
  FROM read_parquet('", ruta_salida_ece, "')
"))
dbDisconnect(con, shutdown = TRUE)

=======
###REVISIÓN DE BASES DE DATOS - ECE MERO:
##BASES CON LA CURP HASHEADA
##

library(dplyr)
library(lubridate)
library(arrow)
library(readxl)
library(ggplot2)
library(readr)
library(data.table)
library(tidyr)
library(janitor)
library(stringr)
library(DBI)
library(duckdb)
library(glue)


#===============================================================
## RUtas
# Cambiar según lo necesario
home <- Sys.getenv("USERPROFILE")

# Carpeta base de OneDrive institucional
base_onedrive <- file.path(home,"IMSS-BIENESTAR")
# base_onedrive <- file.path(home,"IMSS-BIENESTAR")

# Rutas del proyecto
ruta_repo <- file.path(base_onedrive,  "División de Procesamiento de información - Repositorio de Datos")

ruta_actual <- file.path(base_onedrive,
                         "División de Procesamiento de información - Proyectos",
                         "78_transicion sistemas prod",
                         "data_raw",
                         "EDS actual")

ruta_salida <- file.path("C:/Users/brittany.pereo/Downloads")

# file.path(base_onedrive,
#           "División de Procesamiento de información - Proyectos",
#           "78_transicion sistemas prod",
#           "data")

ruta_2025 <- file.path(base_onedrive,
                       "División de Procesamiento de información - Proyectos",
                       "78_transicion sistemas prod",
                       "data_raw",
                       "ece_2025")

ruta_proy_89 <- file.path(
  base_onedrive,
  "División de Procesamiento de información - Proyectos",
  "89_correciones_parquets_dn")


#===============================================================
## CARGA DE ARCHIVOS ECE
#===============================================================
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
# --- SEPARACIÓN DE DATOS DE 2025
#===============================================================
separar_2025_2026 <- function(base, var_fecha, nombre_base, carpeta_salida) {
  
  var_fecha <- rlang::ensym(var_fecha)
  
  base <- base %>%
    mutate(
      fecha_tmp = as.POSIXct(!!var_fecha, tz = "America/Mexico_City"),
      anio_tmp = lubridate::year(fecha_tmp))
  
  tabulado <- base %>%
    count(anio_tmp, name = "n") %>%
    filter(anio_tmp %in% c(2025, 2026))
  
  cat("\nTabulado para:", nombre_base, "\n")
  print(tabulado)
  
  base_2025 <- base %>%
    filter(anio_tmp == 2025) %>%
    select(-fecha_tmp, -anio_tmp)
  
  base_2026 <- base %>%
    filter(anio_tmp == 2026) %>%
    select(-fecha_tmp, -anio_tmp)
  
  arrow::write_parquet(
    base_2025,
    sink = file.path(carpeta_salida, paste0(nombre_base, "_2025.parquet")))
  
  return(base_2026)
}

###separamos eso
moce <- separar_2025_2026(
  base = moce,
  var_fecha = fecha_atencion,
  nombre_base = "moce",
  carpeta_salida = ruta_2025)

cirugia <- separar_2025_2026(
  base = cirugia,
  var_fecha = fecha_de_realizacion,
  nombre_base = "cirugia",
  carpeta_salida = ruta_2025)

urgencias <- separar_2025_2026(
  base = urgencias,
  var_fecha = fecha_hora_egreso,
  nombre_base = "urgencias",
  carpeta_salida = ruta_2025)

egresos <- separar_2025_2026(
  base = egresos,
  var_fecha = fecha_hora_egreso,
  nombre_base = "egresos",
  carpeta_salida = ruta_2025)

uci <- separar_2025_2026(
  base = uci,
  var_fecha = fecha_hora_ingreso,
  nombre_base = "uci",
  carpeta_salida = ruta_2025)


#===============================================================
# --- PROCEDIMIENTOS
#===============================================================
## CARGA DE ARCHIVO DE PROCEDIMIENTOS
qx_2026 <- read_parquet(
  file.path( ruta_proy_89,
             "finales procedimientos",
             "quirurgicos 2026 nuevo.parquet"))

nrow(qx_2026)  # 315,492; #332,332
# names(qx_2026)
# names(cirugia)
table(qx_2026$eliminado, useNA = "ifany") #CERO

#cargo el catalogo de una
catalogo <- read_excel(
  file.path( ruta_proy_89,
             "bases_soporte",
             "PROCEDIMIENTO_202402.xlsx"))
catalogo <- catalogo %>% 
  clean_names()
names(catalogo)
## -- NOTAS --
## IDENTIFICACIÓN DE VARIABLES CIE-9 -> PROCEDIMIENTOS
# qx_2026$cod_cie_procedimiento
# qx_2026$descrip_procedimiento
# cirugia$procedimiento_principal_de_la_solicitud ## viene junto, hay que separarlo en dos columnas

## IDENTIFICACIÓN DE VARIABLES CIE-10 -> DIAGNÓSTICO
# qx_2026$cod_cie_afeccion_principal
# qx_2026$desc_afeccion_principal
# cirugia$cie10_diagnostico_principal_de_la_solicitud  ## viene junto, hay que separarlo en dos columnas

##Generación de variables con el mismo nombre y eliminación de las canceladas
cirugia <- cirugia %>%
  filter(is.na(motivo_de_cancelacion)) %>%  #Se eliminan 995 que están canceldas
  mutate(
    cod_cie_procedimiento = if_else(
      is.na(procedimiento_principal_de_la_solicitud),
      NA_character_,
      substr(procedimiento_principal_de_la_solicitud, 1, 4)
    ),
    
    descrip_procedimiento = if_else(
      is.na(procedimiento_principal_de_la_solicitud),
      NA_character_,
      trimws(substr(
        procedimiento_principal_de_la_solicitud,
        6,
        nchar(procedimiento_principal_de_la_solicitud)
      ))
    ),
    
    cod_cie_afeccion_principal = if_else(
      is.na(cie10_diagnostico_principal_de_la_solicitud),
      NA_character_,
      substr(cie10_diagnostico_principal_de_la_solicitud, 1, 4)
    ),
    
    desc_afeccion_principal = if_else(
      is.na(cie10_diagnostico_principal_de_la_solicitud),
      NA_character_,
      trimws(substr(
        cie10_diagnostico_principal_de_la_solicitud,
        6,
        nchar(cie10_diagnostico_principal_de_la_solicitud)
      ))
    )
  )

#Preguntar  Armando si esto se tiene que fusionar y hacer el mismo filtrado para Q D T
# cirugia
# egresos
# Dice Armando que solo se debe tomar la de cirugías

qx_2026_2 <- qx_2026 %>%
  filter(eliminado !=1) %>% 
  select(
    clues,
    curp_hash32,
    extracto_curp,
    folio,
    fecha_ingreso,
    fecha_egreso,
    fecha_insert,
    cod_cie_procedimiento,
    descrip_procedimiento,
    cod_cie_afeccion_principal,
    desc_afeccion_principal )

#limpiar cirugia cuando no tienen curp o fecha de realización
nrow(cirugia) #
cirugia_curp <- cirugia %>%
  filter(
    !is.na(fecha_de_realizacion),
    !is.na(curp_hash32),
    trimws(curp_hash32) != "",
    trimws(fecha_de_realizacion) != "",
    year(as.Date(fecha_de_realizacion)) == 2026 )

nrow(cirugia_curp) # 4,156 -> después de mandar a hash32, quedó en 4396
# probablemente, porque ahora se eliminaron tmb las curps inválidas

cirugia_curp <- cirugia_curp %>% 
  select(fecha_de_realizacion,curp_hash32, everything())
#Entonces, para hacer el match con la base de cirugías, tendremos que revisar
# que cirugia$fecha_de_realizacion se encuentre entre así:

# cirugia$fecha_de_realizacion >= qx_2026_2$fecha_ingreso &
#   cirugia$fecha_de_realizacion <= qx_2026_2$fecha_egreso

#homologar formatos de fechas
qx_2026_2 <- qx_2026_2 %>%
  mutate(
    fecha_ingreso = as.Date(fecha_ingreso),
    fecha_egreso = as.Date(fecha_egreso)  )

# cirugia_curp <- cirugia_curp %>%
#   mutate(
#     extracto_curp = substr(curp, 5, 13),
#     fecha_realizacion_dia = as.Date(fecha_de_realizacion)
#   )

#revisar la info

# 1. Identificar cirugías de cirugia_curp que ya están cubiertas por qx_2026_2
cirugia_ya_en_qx <- cirugia_curp %>%
  inner_join(
    qx_2026_2 %>%
      select(clues, curp_hash32, fecha_ingreso, fecha_egreso),
    by = c("clues", "curp_hash32"),
    relationship = "many-to-many"
  ) %>%
  filter(
    fecha_de_realizacion >= fecha_ingreso,
    fecha_de_realizacion <= fecha_egreso
  ) %>%
  distinct(clues, curp_hash32, fecha_de_realizacion)

# 2. Quedarnos solo con cirugías que NO están cubiertas en qx_2026_2
cirugia_faltante <- cirugia_curp %>%
  anti_join(
    cirugia_ya_en_qx,
    by = c("clues", "curp_hash32", "fecha_de_realizacion"))

nrow(cirugia_faltante) #2,565

# 3. Adaptarlas a la estructura de qx_2026_2
cirugia_faltante_qx <- cirugia_faltante %>%
  # mutate(
  # sexo_homologado = case_when(
  # sexo == "M" ~ "1",
  # sexo == "F" ~ "2",
  # sexo %in% c("1", "2") ~ sexo,
  # TRUE ~ NA_character_
  # ),
  # ) %>%
  transmute(
    clues = clues,
    curp_hash32 = curp_hash32,
    # extracto_curp = extracto_curp,
    sexo_homologado = as.character(sexo),
    fecha_egreso = fecha_de_realizacion,
    folio = NA_character_,
    fecha_ingreso = fecha_de_realizacion,
    fecha_insert = NA,
    cod_cie_procedimiento = cod_cie_procedimiento,
    descrip_procedimiento = descrip_procedimiento,
    cod_cie_afeccion_principal = cod_cie_afeccion_principal,
    desc_afeccion_principal = desc_afeccion_principal,
    fuente = "cirugia"
  )

# eliminar obs incompletas
cirugia_faltante_qx <- cirugia_faltante_qx %>%
  filter(
    !is.na(curp_hash32),
    trimws(curp_hash32) != "",
    !is.na(fecha_egreso))

nrow(cirugia_faltante_qx) # 2,376

# 4. Unir ambas bases
qx_2026_3 <- bind_rows(
  qx_2026_2 %>%
    mutate(fuente = "qx_2026_2"),
  cirugia_faltante_qx)

#revission
cirugia_faltante_qx %>%
  summarise(
    n = n(),
    sin_extracto_curp = sum(is.na(curp_hash32) | trimws(curp_hash32) == ""),
    sin_fecha_egreso = sum(is.na(fecha_egreso)))

names(qx_2026_3)
table(qx_2026_3$fuente) #    
addmargins(table(qx_2026_3$fuente)) # 387,379 

#HOmologar las fechas y mandar fecha insert igual afecha egreso
qx_2026_3 <- qx_2026_3 %>%
  mutate(
    fecha_insert = if_else(
      is.na(fecha_insert),
      fecha_egreso,
      fecha_insert
    ),
    across(
      c(fecha_ingreso, fecha_egreso, fecha_insert),
      as.Date
    ))

#revisar una fecha que está mal:
# View(
#   cirugia %>%
#     filter(
#       clues == "CSIMB002980",
#       cod_cie_procedimiento == "740X",
#       cod_cie_afeccion_principal == "Z321"
#     )
# )

#arreglar el error de la fecha 0203-03-08, ya se revisó iy corroborró que 
# la fecha correcta es 2026-03-08
qx_2026_3 <- qx_2026_3 %>%
  mutate(
    fecha_ingreso = if_else(
      fecha_ingreso == as.Date("0203-03-08"), 
      as.Date("2026-03-08"), 
      fecha_ingreso
    ),
    fecha_egreso = if_else(
      fecha_egreso == as.Date("0203-03-08"), 
      as.Date("2026-03-08"), 
      fecha_egreso
    ),
    fecha_insert = if_else(
      fecha_insert == as.Date("0203-03-08"), 
      as.Date("2026-03-08"), 
      fecha_insert
    ))


#el pinche catalogo
qx_2026_3 <- qx_2026_3 %>%
  left_join(
    catalogo %>%
      select(catalog_key, procedimiento_type),
    by = c("cod_cie_procedimiento" = "catalog_key")
  ) %>%
  rename(
    proced_catalogo = procedimiento_type
  )
table(qx_2026_3$proced_catalogo, useNA = "ifany")
nrow(qx_2026_3) #334,726

#salvar
arrow::write_parquet(
  qx_2026_3,
  sink = file.path(ruta_salida, "proc_qx_con_ECE_2026.parquet"))


#===============================================================
# --- CONSULTAS
#===============================================================

#Extraer datos del query de consultas
# prueba <- read_parquet("C:/Users/angelica.gonzalezl/OneDrive - IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/Productividad/Bases originales/2026_daniel/salud_mental_01_01_2026_a_20_05_2026.parquet")
# names(prueba)
#Clasificar el tipo de consultas: general especialidad
names(moce)
#esto porque el pend de daniel, le cambia el nombre a la variable hdlv
moce <- moce %>%
  rename_with(
    ~ "curp_hash32",
    .cols = any_of("cve_curp_hash32"))

table(moce$estatus_cita, useNA = "ifany")
moce <- moce %>% 
  filter(
    coalesce(estatus_cita, "") != "No se Presentó")

moce <- moce %>% 
  mutate(
    tipo_consulta = case_when(
      clave_de_servicio %in% c("MG01", # Medicina General
                               "6301", # Psicología
                               "9999", # No Especificada
                               "PREC", # Pre Consulta
                               "5001", # Consultas en Primer Contacto
                               "6601"  # Nutrición y Dietítica
      ) ~ "general",
      TRUE ~ "especialidad"))

table(moce$tipo_consulta)

moce_match <- moce %>%
  mutate(
    clues = as.character(clues),
    fecha_consulta = as.Date(ymd_hms(fecha_atencion))
    # extracto_curp = substr(cve_curp, 5, 13)
  ) %>%
  filter(year(fecha_consulta) == 2026) %>%
  as.data.frame()

#conexión DDB para unir las varias bases:

con <- dbConnect(duckdb::duckdb())

ultimo_miercoles <- function(fecha = Sys.Date()) {
  fecha <- as.Date(fecha)
  dias <- as.numeric(format(fecha, "%u")) - 3
  
  if (dias == 0) {
    return(fecha - 7)
  } else {
    dias_restar <- ifelse(dias > 0, dias, dias + 7)
    return(fecha - dias_restar)
  }
}

fecha_actual <- ultimo_miercoles() |> format("%d_%m")
fecha_de_corte <- ultimo_miercoles()

ruta <- file.path(
  "C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR",
  "División de Procesamiento de información - Repositorio de Datos",
  "Productividad",
  "Bases originales",
  "2026_daniel")

query_crear_vista <- glue("
CREATE OR REPLACE VIEW consultas_2026 AS
SELECT
  clues,
  servicio_atencion,
  COALESCE(CAST(fecha_insert AS DATE), CAST(fecha_consulta AS DATE)) AS fecha_insert,
  CAST(fecha_consulta AS DATE) AS fecha_consulta,
  curp_prestador, 
  CASE WHEN filename LIKE '%salud_mental%' THEN 'salud mental'
    WHEN filename LIKE '%salud_bucal%' THEN 'salud bucal'
    WHEN filename LIKE '%planificacion_familiar%' THEN 'planificacion familiar'
    WHEN filename LIKE '%consulta_externa%' THEN 'consulta externa'
  END AS fuente,
  edad,
  sexo,
  tipo_personal,
  codigo_cie_diagnostico1,
  descripcion_diagnostico1,
  teleconsulta,
  clave_edad,
  extracto_curp,
  curp_invalida_razon,
  curp_hash32,
  tipo_consulta
FROM parquet_scan(
  [
    '{ruta}/salud_mental_01_01_2026_a_{fecha_actual}_2026.parquet',
    '{ruta}/salud_bucal_01_01_2026_a_{fecha_actual}_2026.parquet',
    '{ruta}/planificacion_familiar_01_01_2026_a_{fecha_actual}_2026.parquet',
    '{ruta}/consulta_externa_01_01_2026_a_{fecha_actual}_2026.parquet'
  ],
  union_by_name = TRUE
)
WHERE fecha_insert IS NOT NULL
  AND CAST(fecha_insert AS DATE) <= DATE '{fecha_de_corte}'
")

dbExecute(con, query_crear_vista)

dbWriteTable(
  con,
  "moce_match",
  moce_match,
  overwrite = TRUE)

dbExecute(con, "
CREATE OR REPLACE VIEW consultas_2026_personas AS
SELECT DISTINCT
  CAST(clues AS VARCHAR) AS clues,
  CAST(fecha_consulta AS DATE) AS fecha_consulta,
  CAST(curp_hash32  AS VARCHAR) AS curp_hash32 
FROM consultas_2026
WHERE clues IS NOT NULL
  AND fecha_consulta IS NOT NULL
  AND extracto_curp IS NOT NULL
")

dbGetQuery(con, "
SELECT
  COUNT(*) AS total_moce,
  COUNT(c.curp_hash32) AS ya_existen_en_consultas_2026,
  COUNT(*) - COUNT(c.curp_hash32) AS faltan_por_agregar
FROM moce_match m
LEFT JOIN consultas_2026_personas c
  ON CAST(m.clues AS VARCHAR) = c.clues
 AND m.fecha_consulta = c.fecha_consulta
 AND CAST(m.curp_hash32 AS VARCHAR) = c.curp_hash32
")

#Falta por agregar: 33,582
## extraer
moce_faltantes <- dbGetQuery(con, "
SELECT m.*
FROM moce_match m
LEFT JOIN consultas_2026_personas c
  ON CAST(m.clues AS VARCHAR) = c.clues
 AND m.fecha_consulta = c.fecha_consulta
 AND CAST(m.curp_hash32 AS VARCHAR) = c.curp_hash32
WHERE c.curp_hash32 IS NULL
")

nrow(moce_faltantes) ## 33,582, aumentó respecto al anterior que no usaba hash32
names(moce_faltantes)
dbGetQuery(con, "
DESCRIBE consultas_2026
")

#homologar variables
moce_faltantes_homologado <- moce_faltantes %>%
  transmute(
    clues = as.character(clues),
    servicio_atencion = as.character(desc_servicio),
    fecha_insert = as.Date(fecha_consulta),
    fecha_consulta = as.Date(fecha_consulta),
    curp_prestador = as.character(matricula),
    edad = as.character(NA),
    sexo = as.character(NA),
    tipo_personal = as.character(tipo_medico),
    codigo_cie_diagnostico1 = as.character(x10_principal),
    descripcion_diagnostico1 = as.character(ocasion_principal),
    teleconsulta = as.character(NA),
    clave_edad = as.character(NA),
    # extracto_curp = as.character(extracto_curp),
    curp_invalida_razon = as.character(NA),
    curp_hash32 = as.character(curp_hash32),
    tipo_consulta = as.character(tipo_consulta))

#subirlo a DDB
dbWriteTable(
  con,
  "moce_faltantes_homologado",
  moce_faltantes_homologado,
  overwrite = TRUE)

## consultas originales + las faltantes de moce
dbExecute(con, "
CREATE OR REPLACE VIEW consultas_2026_con_moce AS

SELECT 
  clues,
  servicio_atencion,
  fecha_insert,
  fecha_consulta,
  curp_prestador,
  fuente,
  edad,
  sexo,
  tipo_personal,
  codigo_cie_diagnostico1,
  descripcion_diagnostico1,
  teleconsulta,
  clave_edad,
  -- extracto_curp,
  curp_invalida_razon,
  curp_hash32,
  tipo_consulta
FROM consultas_2026

UNION ALL

SELECT 
  clues,
  servicio_atencion,
  fecha_insert,
  fecha_consulta,
  curp_prestador,
  'moce' AS fuente,
  edad,
  sexo,
  tipo_personal,
  codigo_cie_diagnostico1,
  descripcion_diagnostico1,
  teleconsulta,
  clave_edad,
  -- extracto_curp,
  curp_invalida_razon,
  curp_hash32,
  tipo_consulta
FROM moce_faltantes_homologado
")

#Una pequeña rev de las fechas
dbGetQuery(con, "
SELECT 
  fuente,
  COUNT(*) AS n,
  SUM(CASE WHEN fecha_insert IS NULL THEN 1 ELSE 0 END) AS n_fecha_insert_null,
  SUM(CASE WHEN fecha_consulta IS NULL THEN 1 ELSE 0 END) AS n_fecha_consulta_null
FROM consultas_2026_con_moce
GROUP BY fuente
ORDER BY fuente
")

##cuantas vienen de moce
dbGetQuery(con, "
SELECT
  fuente,
  COUNT(*) AS total
FROM consultas_2026_con_moce
GROUP BY fuente

UNION ALL
SELECT
  'TOTAL' AS fuente,
  COUNT(*) AS total
FROM consultas_2026_con_moce
")

#Guardar

archivo_salida <- file.path(ruta_salida, "consultas_con_ECE_2026.parquet")

query_exportar <- glue::glue("
COPY consultas_2026_con_moce
TO '{archivo_salida}'
(FORMAT PARQUET)
")

dbExecute(con, query_exportar)

dbDisconnect(con, shutdown = TRUE)


#===============================================================
# --- EGRESOS
#===============================================================

#VARS DE CIE10:
#EGRESOS ECE: dx_prin_egreso
#EGRESOS_FINAL: cod_cie_afeccion_principal, desc_afeccion_principal

# 1. Ruta del parquet grande
ruta_egresos_final <- file.path(
  ruta_proy_89,
  "finales egresos",
  "egresos 2026 nuevo.parquet")

# 2. Conexión
con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")

# 3. Abrir egresos_final como vista
dbExecute(con, paste0("
  CREATE VIEW egresos_final AS
  SELECT * FROM read_parquet('", ruta_egresos_final, "')
"))

# 4. Ver variables de egresos_final
vars_final <- dbGetQuery(con, "DESCRIBE egresos_final")
vars_final_names <- vars_final$column_name
vars_final_names

egresos <- egresos %>%
  mutate(fecha_egreso = as.Date(fecha_hora_egreso))

egresos <- egresos %>%
  rename_with(
    ~ "curp_hash32",
    .cols = any_of("pac_curp_hash32") )

egresos <- egresos %>%
  filter(
    !is.na(clues),
    !is.na(curp_hash32),
    !is.na(fecha_egreso),
    year(as.Date(fecha_egreso)) == 2026)

egresos <- egresos %>%
  mutate(fecha_egreso = as.Date(fecha_hora_egreso))

#agregamos las variables que son del cod cie
vars_extra_final <- c(
  "cod_cie_afeccion_principal",
  "desc_afeccion_principal",
  "fecha_insert")

vars_extra_egresos <- c("dx_prin_egreso")

vars_comunes_base <- intersect(names(egresos), vars_final_names)

vars_comunes_base <- setdiff(
  vars_comunes_base,
  c(vars_extra_final, vars_extra_egresos))

dbExecute(con, paste0("
  CREATE OR REPLACE VIEW egresos_final_comun AS
  SELECT
    ", paste(vars_comunes_base, collapse = ", "), ",
    cod_cie_afeccion_principal,
    desc_afeccion_principal,
    fecha_insert,
    'egresos_final' AS fuente
  FROM egresos_final
"))

egresos_match <- egresos %>%
  filter(
    !is.na(clues),
    !is.na(curp_hash32),
    !is.na(fecha_egreso)
  ) %>%
  select(
    all_of(vars_comunes_base),
    dx_prin_egreso
  ) %>%
  mutate(
    cod_cie_afeccion_principal = dx_prin_egreso,
    desc_afeccion_principal = NA_character_,
    fecha_insert = as.POSIXct(fecha_egreso, tz = "UTC"),
    fuente = "egresos_ece"
  ) %>%
  select(
    all_of(vars_comunes_base),
    cod_cie_afeccion_principal,
    desc_afeccion_principal,
    fecha_insert,
    fuente)

copy_to(con, egresos_match, "egresos", overwrite = TRUE)

llaves <- c("clues", "curp_hash32", "fecha_egreso")

egresos_nuevos <- tbl(con, "egresos") %>%
  anti_join(
    tbl(con, "egresos_final_comun"),
    by = llaves
  ) %>%
  collect()

copy_to(con, egresos_nuevos, "egresos_nuevos", overwrite = TRUE)
dbListTables(con)

dbExecute(con, "
  CREATE OR REPLACE TABLE egresos AS
  SELECT
    * EXCLUDE(fecha_insert),
    CAST(fecha_insert AS TIMESTAMPTZ) AS fecha_insert
  FROM egresos
")

nombre_archivo <- "egresos_con_ECE_2026.parquet"
ruta_salida_ece <- file.path(ruta_salida, nombre_archivo)

dbExecute(con, paste0("
  COPY (
    SELECT * FROM egresos_final_comun
    UNION ALL
    SELECT * FROM egresos_nuevos
  )
  TO '", ruta_salida_ece, "'
  (FORMAT PARQUET)
"))


#revisar el tabulado
dbGetQuery(con, paste0("
  SELECT
    SUM(CASE WHEN fuente = 'egresos_final' THEN 1 ELSE 0 END) AS estaban_antes,
    SUM(CASE WHEN fuente = 'egresos_ece' THEN 1 ELSE 0 END) AS agregadas,
    COUNT(*) AS total_final
  FROM read_parquet('", ruta_salida_ece, "')
"))

dbGetQuery(con, paste0("
  DESCRIBE SELECT *
  FROM read_parquet('", ruta_salida_ece, "')
"))

dbGetQuery(con, paste0("
  DESCRIBE SELECT *
  FROM read_parquet('", ruta_salida_ece, "')
"))
dbDisconnect(con, shutdown = TRUE)
