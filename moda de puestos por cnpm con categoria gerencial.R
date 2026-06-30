library(dplyr)
library(stringr)

nomina <- readxl::read_xlsx(
  "C:/Users/Cecilia Pereo/Downloads/nomina_general.xlsx"
) %>% 
  janitor::clean_names() %>% 
  mutate(
    clues = str_trim(str_to_upper(clues)),
    clues = str_extract(clues, "^[A-Z]{2}IMB[0-9]{6}")
  )

catalogo_clues <- arrow::read_parquet(
  "C:/Users/Cecilia Pereo/IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/CLUES/clues.parquet"
) %>% 
  select(clues = clues_imb, categoria_gerencial)

catalogo_cnpm <- readxl::read_xlsx(
  "C:/Users/Cecilia Pereo/IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/Plantilla/catalogos/Catalogo_CNPM_2026_F.xlsx"
) %>% 
  janitor::clean_names() %>% 
  select(codigo_de_cargo_actual = codigo_cnpm,
         cnpm = codigo_cnpm_26,
         denominacion_cnpm = denominacion_de_puesto)

catalogo_puestos <- readxl::read_xlsx(
  "C:/Users/Cecilia Pereo/IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/Plantilla/catalogos/Catálogo de Código de Puesto CRH.xlsx"
) %>% 
  janitor::clean_names() %>% 
  select(puesto = codigo_del_puesto,
         descripcion_de_puesto)

moda_cnpm <- nomina %>% 
  left_join(catalogo_clues, by = "clues") %>% 
  group_by(codigo_de_cargo_actual, puesto, categoria_gerencial) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  group_by(codigo_de_cargo_actual) %>% 
  slice_max(n, n = 1, with_ties = FALSE) %>% 
  ungroup() %>% 
  left_join(catalogo_cnpm, by = "codigo_de_cargo_actual") %>% 
  left_join(catalogo_puestos, by = "puesto") %>% 
  select(categoria_gerencial, cnpm, denominacion_cnpm, puesto, descripcion_de_puesto, n) %>% 
  arrange(categoria_gerencial,cnpm, puesto)


sin_c <- nomina %>% 
  anti_join(catalogo_clues, by = "clues") %>% 
  distinct(clues)

writexl::write_xlsx(moda_cnpm,
                    "C:/Users/Cecilia Pereo/Downloads/moda de puestos por cnpm con categoria gerencial.xlsx"
                    )
