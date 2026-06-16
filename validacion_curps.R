library(googlesheets4)
library(tidyr)
library(dplyr)
library(writexl)

gs4_deauth()
gs4_auth(
  email = "lia.pereo@ciencias.unam.mx",
  scopes = "https://www.googleapis.com/auth/spreadsheets.readonly"
)

url <- "https://docs.google.com/spreadsheets/d/1Axj6z0U1odyFcdDz7Rjdv2jOZgnh9yIxKAXiWzvXKW4/edit"

df <- read_sheet(
  ss = url,
  sheet = "Registros_completos")

curps_val <- readxl::read_xlsx(
  "C:/Users/brittany.pereo/Downloads/Plantilla UEH marzo 2026.xlsx"
) %>% 
  janitor::clean_names()


val <- df %>% 
  janitor::clean_names() %>% 
  select(estado, clues, fase, curp, cnpm,
         revision_uas) %>% 
  right_join(curps_val, by = "curp")

curps_duplicadas <- val %>% 
  count(curp) %>% 
  filter(n > 1)

val_duplicadas <- val %>% 
  filter(curp %in% curps_duplicadas$curp)

no_match <- val %>% 
  filter(is.na(estado))

match_ok <- val %>% 
  filter(!is.na(estado))

curps_sin_match <- curps_val %>% 
  anti_join(df %>% janitor::clean_names(), by = "curp")


val %>% 
  summarise(
    total = n(),
    duplicadas = sum(duplicated(curp)),
    sin_match = sum(is.na(estado)),
    con_match = sum(!is.na(estado))
  )

resumen <- val %>% 
  summarise(
    total = n(),
    duplicadas = sum(duplicated(curp)),
    sin_match = sum(is.na(estado)),
    con_match = sum(!is.na(estado))
  )

write_xlsx(
  list(
    "Base_completa" = val,
    "Duplicadas" = val_duplicadas,
    "Sin_match" = no_match,
    "Con_match" = match_ok,
    "Solo_CURPs_sin_match" = curps_sin_match,
    "Resumen" = resumen
  ),
  path = "C:/Users/brittany.pereo/Downloads/revision_curps.xlsx"
)
