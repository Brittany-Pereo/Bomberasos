library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)

setwd("C:/Users/brittany.pereo/GitHub/planes_de_justicia")

# Bases de cubos ----------------------------------------------------------
cubos_completos_plan_justicia <- readxl::read_xlsx(
    "C:/Users/brittany.pereo/Downloads/cubos_completos_plan_justicia.xlsx"
  ) 

cubos_completos_plan_justicia_nacional <- cubos_completos_plan_justicia %>% 
  group_by(fecha, anio) %>% 
  summarise(clues = "NACIONAL",
    across(
      where(is.numeric),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# cubos_completos_plan_justicia_estatal <- clues_info %>% 
#   select(entidad, clues = clues_imb, categoria_gerencial, 
#          estatus_de_operacion, nombre_de_la_unidad,
#          nivel_atencion) %>% 
#   right_join(cubos_completos_plan_justicia, 
#              by = "clues") %>% 
#   group_by(anio, fecha) %>% 
#   summarise(clues = entidad,
#             across(
#               where(is.numeric),
#               ~ sum(.x, na.rm = TRUE)
#             ),
#             .groups = "drop"
#   )


cubos_completos_plan_justicia <- rbind(cubos_completos_plan_justicia,
                                       cubos_completos_plan_justicia_nacional
                                       # ,
                                       # cubos_completos_plan_justicia_estatal
                                       ) %>% 
  filter(!is.na(clues))

cubos_completos_procedimiento <- arrow::read_parquet(
  "C:/Users/brittany.pereo/GitHub/crear_pptx_clues/inst/app/data/Cubos_completos_2020_2025.parquet"
)

arrow::write_parquet(cubos_completos_plan_justicia,
  "C:/Users/brittany.pereo/GitHub/planes_de_justicia/inst/app/data/Cubos_completos_2020_2025.parquet"
)

# Ptocedimientos ----------------------------------------------------------
procedimientos_personas_plan_justicia <- readxl::read_excel(
    "C:/Users/brittany.pereo/Downloads/procedimientos_personas_plan_justicia.xlsx", 
                      sheet = "Sheet1") %>% 
  filter(!is.na(anio_insert),
         !is.na(tipo_procedimiento)) 

procedimientos_personas_plan_justicia_nacional <- procedimientos_personas_plan_justicia %>% 
  group_by(anio_insert, tipo_procedimiento) %>% 
  summarise(id = "NACIONAL",
            across(
              where(is.numeric),
              ~ sum(.x, na.rm = TRUE)
            ),
            .groups = "drop"
  )

# procedimientos_personas_plan_justicia_estatal <- clues_info %>% 
#   select(entidad, id = clues_imb, categoria_gerencial, 
#          estatus_de_operacion, nombre_de_la_unidad,
#          nivel_atencion) %>% 
#   right_join(procedimientos_personas_plan_justicia, 
#              by = "id") %>% 
#   group_by(anio_insert, tipo_procedimiento) %>% 
#   summarise(id = entidad,
#             across(
#               where(is.numeric),
#               ~ sum(.x, na.rm = TRUE)
#             ),
#             .groups = "drop"
#   )


procedimientos_personas_plan_justicia <- rbind(procedimientos_personas_plan_justicia,
                                               procedimientos_personas_plan_justicia_nacional
                                               #procedimientos_personas_plan_justicia_estatal
                                               ) %>% 
  filter(!is.na(id))

procedimientos_personas_productividad <- arrow::read_parquet(
  "C:/Users/brittany.pereo/GitHub/crear_pptx_clues/inst/app/data/procedimientos_personas.parquet"
)

arrow::write_parquet(procedimientos_personas_plan_justicia,
                    "C:/Users/brittany.pereo/GitHub/planes_de_justicia/inst/app/data/procedimientos_personas.parquet"
)


# Metas y CLUES -----------------------------------------------------------
# Metas y CLUES para planes de justicia -----------------------------------
# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

#
require(arrow)
require(dplyr)
clues_info <- readxl::read_xlsx(
  "C:/Users/brittany.pereo/Downloads/metas_planes_justicia 1.xlsx") %>% 
  janitor::clean_names()

choices_etiquetas <-
  setNames(
    c("NACIONAL", clues_info$clues_imb),
    paste0(c("NACIONAL", clues_info$clues_imb), " - ", 
           c("NACIONAL", clues_info$nombre_de_la_unidad))
  )

clues_info <-clues_info |>
  filter(!is.na(entidad))|>
  select(entidad, clues_imb, categoria_gerencial, 
         estatus_de_operacion, nombre_de_la_unidad,
         nivel_atencion)

metas <- readxl::read_xlsx(
 "C:/Users/brittany.pereo/Downloads/metas_planes_justicia 1.xlsx"  
 ) 

metas <- rbind(metas,
               metas |>
                 summarise(
                   entidad="NACIONAL",
                   clues_imb="NACIONAL",
                   categoria_gerencial="NACIONAL",
                   estatus_de_operacion="EN OPERACION",
                   nombre_de_la_unidad="México",
                   nivel_atencion="NACIONAL",
                   meta_general_anual=sum(meta_general_anual),
                   meta_especialidad_anual=sum(meta_especialidad_anual),
                   meta_cirugia_anual = sum(meta_cirugia_anual),
                   meta_egresos_anual = sum(meta_egresos_anual)
                 ),
               metas |>
                 summarise(
                   clues_imb=first(entidad),
                   categoria_gerencial=first(entidad),
                   estatus_de_operacion="EN OPERACION",
                   nombre_de_la_unidad=first(entidad),
                   nivel_atencion=first(entidad),
                   meta_general_anual=sum(meta_general_anual, na.rm=TRUE),
                   meta_especialidad_anual=sum(meta_especialidad_anual, na.rm=TRUE),
                   meta_cirugia_anual =sum(meta_cirugia_anual, na.rm=TRUE),
                   meta_egresos_anual =sum(meta_egresos_anual, na.rm=TRUE),
                   .by="entidad"
                 )
) %>% 
  filter(!is.na(entidad))


choices_etiquetas <-
  setNames(
    c("NACIONAL", clues_info$clues_imb),
    paste0(
      c("NACIONAL", clues_info$clues_imb),
      " - ",
      c("México", clues_info$nombre_de_la_unidad)
    )
  )

require(data.table)
pruebita <- metas |>
  dplyr::select(-meta_general_anual, -meta_especialidad_anual, meta_cirugia_anual, -meta_egresos_anual ) |>
  dplyr::filter(!clues_imb%in%clues_info$clues_imb)

#clues_info <- plyr::rbind.fill(clues_info, pruebita)


usethis::use_data(clues_info, choices_etiquetas, metas,
                  internal = TRUE,
                  overwrite = TRUE)


file.exists("R/sysdata.rda")

load("R/sysdata.rda")
ruta <- "C:/Users/brittany.pereo/Downloads/datos_clues_E. Sinaloa_2026-06-29.xlsx"
datos_consulta <-       list(
  datos = ruta |> readxl::read_excel(sheet= "productividad detalle"),
  resumen = ruta|> readxl::read_excel(skip =7, sheet=1),
  clues_seleccionada = "E. Sinaloa")

codigo_clues = datos_consulta$clues_seleccionada
clues_info = clues_info
metas = metas
historicos = datos_consulta$datos
procedimientos_personas = datos_consulta$resumen
ruta_master = "C:/Users/brittany.pereo/GitHub/planes_de_justicia/inst/app/data/master_presentacion.pptx"

print(presentacion,
      target = "C:/Users/brittany.pereo/Downloads/BCIMB000022.pptx")




