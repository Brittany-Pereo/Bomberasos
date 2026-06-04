library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(svglite)

# Bases -------------------------------------------------------------------
catalogo_clues <- arrow::read_parquet(
  "C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/CLUES/clues.parquet"
)

catalogo_procedimientos <- readxl::read_xlsx(
"C:/Users/brittany.pereo/Downloads/PROCEDIMIENTO_202402 (4).xlsx"
) %>% 
  janitor::clean_names() %>% 
  select(cod_cie_procedimiento = catalog_key, procedimiento_type)

procedimientos_2024_d <- arrow::read_parquet(
  "C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/89_correciones_parquets_dn/finales procedimientos/quirurgicos 2024 nuevo.parquet"
)%>% 
  transmute(clues, folio, fecha_egreso, cod_cie_procedimiento,
            val = "Se queda")

procedimientos_2025_d <- arrow::read_parquet(
 "C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/89_correciones_parquets_dn/finales procedimientos/quirurgicos 2025 nuevo.parquet"
 )%>% 
  transmute(clues, folio, fecha_egreso, cod_cie_procedimiento,
            val = "Se queda")

procedimientos_propios <- arrow::read_parquet(
"C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/78_transicion sistemas prod/data/proc_qx_con_ECE_2026.parquet"
) %>% 
  transmute(clues, folio, fecha_egreso, cod_cie_procedimiento,
            val = "Se queda")

procedimientos_propios_d <- bind_rows(
  procedimientos_2024_d,
  procedimientos_2025_d,
  procedimientos_propios
) %>% 
  mutate(
    fecha_egreso = as.Date(fecha_egreso)
  ) %>% 
  distinct(
    clues, folio, fecha_egreso, cod_cie_procedimiento
  )
# egresos -----------------------------------------------------------------
egresos_2024 <- arrow::read_parquet(
  "C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/89_correciones_parquets_dn/finales egresos/egresos 2024 nuevo.parquet"
  ) %>% 
  janitor::clean_names() %>% 
  filter(
    eliminado == 0,
    !is.na(eliminado)
  ) 

egresos_2025 <- arrow::read_parquet(
 "C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/89_correciones_parquets_dn/finales egresos/egresos 2025 nuevo.parquet"
 )%>% 
  janitor::clean_names() %>% 
  filter(
    eliminado == 0,
    !is.na(eliminado)
  ) 

egresos_2026 <- arrow:: read_parquet(
 "C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/89_correciones_parquets_dn/finales egresos/egresos 2026 nuevo.parquet"
  ) %>% 
  janitor::clean_names() %>% 
  filter(
    eliminado == 0,
    !is.na(eliminado)
  ) 

egresos_historicos <- bind_rows(
  egresos_2024,
  egresos_2025,
  egresos_2026
) %>% 
  janitor::clean_names() %>% 
  filter(
    eliminado == 0,
    !is.na(eliminado)
  ) %>% 
  transmute(
    clues,
    folio,
    fecha_egreso = as.Date(fecha_egreso),
    fecha_nacimiento_paciente,
    sexo
  ) %>% 
  filter(
    !is.na(sexo),
    !is.na(fecha_nacimiento_paciente)
  )

# Procedimientos ----------------------------------------------------------
procedimientos_2024 <- arrow::read_parquet(
 "C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR/Archivos de Daniel Antonio Jiménez Ángeles - 0021_procedimientos_universe/2024/procedimientos_2024_sin_thanificar.parquet"
 )
procedimientos_2025 <- arrow::read_parquet(
  "C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR/Archivos de Daniel Antonio Jiménez Ángeles - 0021_procedimientos_universe/2025/procedimientos_2025_sin_thanificar.parquet"
)
procedimientos_2026 <- arrow::read_parquet(
  "C:\\Users\\brittany.pereo\\OneDrive - IMSS-BIENESTAR\\Archivos de Daniel Antonio Jiménez Ángeles - 0021_procedimientos_universe\\2026\\procedimientos_2026_sin_thanificar.parquet"
) 

procedimientos_historicos <- bind_rows(
  procedimientos_2024,
  procedimientos_2025,
  procedimientos_2026
) %>% 
  janitor::clean_names() %>% 
  transmute(
    clues,
    folio,
    fecha_egreso = as.Date(fecha_egreso),
    numero_procedimiento,
    quirofano_dentro_fuera,
    cod_cie_procedimiento
  ) %>% 
  filter(clues %in% catalogo_clues$clues_imb)

df_limpio <- procedimientos_historicos %>% 
 semi_join(
    egresos_historicos,
    by = c("clues", "folio", "fecha_egreso")
  ) %>% 
  left_join(
    catalogo_procedimientos,
    by = "cod_cie_procedimiento"
  ) %>% 
  mutate(
    categoria = case_when(
      quirofano_dentro_fuera == 1 &
        procedimiento_type %in% c("T", "D") ~"Procedimientos quirurgicos",
      procedimiento_type == "Q" ~ "Procedimientos quirurgicos",
      TRUE ~ "Procedimientos no quirurgicos"
    ),
    semana = floor_date(fecha_egreso,
                        "week", week_start = 1)
  ) 
# -------------------------------------------------------------------------
# GRAFICA 1 POR SEMANA
# -------------------------------------------------------------------------
procedimientos_eventos <- df_limpio %>% 
  group_by(clues, folio, fecha_egreso, semana) %>% 
  summarise(evento_quirurgico = any(
    categoria == "Procedimientos quirurgicos",
    na.rm = TRUE),
    .groups = "drop")

df_grafica_1_semana <- procedimientos_eventos %>% 
  group_by(semana) %>% 
  summarise(
    eventos_totales = n(),
    eventos_quirurgicos = sum(evento_quirurgico),
    eventos_no_quirurgicos = sum(!evento_quirurgico),
    .groups = "drop"
  )

grafica_1_por_semana <- ggplot(df_grafica_1_semana, aes(x = semana)) +
  geom_line(aes(y = eventos_totales, color = "Total"), linewidth = 1.1) +
  geom_line(aes(y = eventos_quirurgicos, color = "Quirurgico"), linewidth = 1.1) +
  geom_line(aes(y = eventos_no_quirurgicos, color = "No quirurgico"), linewidth = 1.1) +
  geom_point(aes(y = eventos_totales, color = "Total"), size = 2) +
  geom_point(aes(y = eventos_quirurgicos, color = "Quirurgico"), size = 2) +
  geom_point(aes(y = eventos_no_quirurgicos, color = "No quirurgico"), size = 2) +
  scale_x_date(
    date_breaks = "4 weeks",
    date_labels = "%d-%b-%y"
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Eventos por semana",
    x = NULL,
    y = "Eventos",
    color = NULL
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1))
grafica_1_por_semana
# -------------------------------------------------------------------------
# GRAFICA 2 POR SEMANA
# -------------------------------------------------------------------------
df_limpio_eventos <- df_limpio %>%
  group_by(clues, folio, fecha_egreso, semana) %>% 
  mutate(evento_quirurgico = any(
    categoria == "Procedimientos quirurgicos",
    na.rm = TRUE)) %>% 
  filter(evento_quirurgico == TRUE) 

df_grafica_2_semana <- df_limpio_eventos %>%
  group_by(clues, folio, fecha_egreso, semana) %>% 
  summarise(
    numero_eventos = 1,
    numero_de_procedimientos = n_distinct(numero_procedimiento),
    procedimientos_t_d = sum(procedimiento_type %in% c("T", "D"),
                             na.rm = TRUE),
    procedimientos_q = sum(procedimiento_type == "Q", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(semana) %>%
  summarise(
    numero_eventos = sum(numero_eventos, na.rm = TRUE),
    numero_de_procedimientos = sum(numero_de_procedimientos, na.rm = TRUE),
    procedimientos_t_d = sum(procedimientos_t_d, na.rm = TRUE),
    procedimientos_q = sum(procedimientos_q, na.rm = TRUE),
    .groups = "drop")

grafica_2_por_semana <- ggplot(
  df_grafica_2_semana,
  aes(x = semana)
) +
  geom_line(
    aes(
      y = numero_eventos,
      color = "Eventos"
    ),
    linewidth = 1.2
  ) +
  geom_line(
    aes(
      y = numero_de_procedimientos,
      color = "Procedimientos"
    ),
    linewidth = 1.2
  ) +
  geom_line(
    aes(
      y = procedimientos_t_d,
      color = "Procedimientos T/D"
    ),
    linewidth = 1,
    linetype = "dashed"
  ) +
  geom_line(
    aes(
      y = procedimientos_q,
      color = "Procedimientos Q"
    ),
    linewidth = 1,
    linetype = "dashed"
  ) +
  scale_y_continuous(
    name = "Número",
    labels = scales::comma
  ) +
  scale_x_date(
    date_breaks = "4 weeks",
    date_labels = "%d-%b-%y"
  ) +
  labs(
    title = "Eventos y procedimientos quirúrgicos por semana",
    x = NULL,
    y = NULL,
    color = NULL
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    legend.position = "bottom"
  )

grafica_2_por_semana

# -------------------------------------------------------------------------
# Grafica 3
# -------------------------------------------------------------------------
df_graf_3 <- df_limpio_eventos %>% 
  group_by(clues, folio, fecha_egreso, semana) %>% 
  summarise(
    procedimientos_t_d_1 = sum(procedimiento_type %in% c("T", "D")&
                               quirofano_dentro_fuera == 1,
                             na.rm = TRUE),
    procedimientos_t_d_0 = sum(procedimiento_type %in% c("T", "D")&
                                   quirofano_dentro_fuera != 1,
                                 na.rm = TRUE),
    procedimientos_q_1 = sum(procedimiento_type == "Q"&
                             quirofano_dentro_fuera == 1, na.rm = TRUE),
    procedimientos_q_0 = sum(procedimiento_type == "Q"&
                             quirofano_dentro_fuera != 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(semana) %>%
  summarise(
    procedimientos_t_d_1 = sum(procedimientos_t_d_1, na.rm = TRUE),
    procedimientos_q_1 = sum(procedimientos_q_1, na.rm = TRUE),
    procedimientos_t_d_0 = sum(procedimientos_t_d_0, na.rm = TRUE),
    procedimientos_q_0 = sum(procedimientos_q_0, na.rm = TRUE),
    .groups = "drop")


grafica_3_por_semana <- ggplot(
  df_graf_3,
  aes(x = semana)
) +
  geom_line(
    aes(
      y = procedimientos_t_d_1,
      color = "Procedimientos T/D en quirofano"
    ),
    linewidth = 1
  ) +
  geom_line(
    aes(
      y = procedimientos_q_1,
      color = "Procedimientos Q en quirofano"
    ),
    linewidth = 1,
    linetype = "dashed"
  ) +
  geom_line(
    aes(
      y = procedimientos_t_d_0,
      color = "Procedimientos T/D fuera quirofano"
    ),
    linewidth = 1
  ) +
  geom_line(
    aes(
      y = procedimientos_q_0,
      color = "Procedimientos Q fuera quirofano"
    ),
    linewidth = 1,
    linetype = "dashed"
  ) +
  scale_y_continuous(
    name = "Número",
    labels = scales::comma
  ) +
  scale_x_date(
    date_breaks = "4 weeks",
    date_labels = "%d-%b-%y"
  ) +
  labs(
    title = "Eventos y procedimientos quirúrgicos por semana",
    x = NULL,
    y = NULL,
    color = NULL
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    legend.position = "bottom"
  )

grafica_3_por_semana

# GUARDAR -----------------------------------------------------------------
# Carpeta destino
ruta_salida <- "C:\\Users\\brittany.pereo\\Downloads\\ceci_codigo"

# Lista de gráficas
graficas <- list(
  grafica_1_por_semana = grafica_1_por_semana,
  grafica_2_por_semana = grafica_2_por_semana)

# Exportar a SVG
purrr::iwalk(
  graficas,
  ~ ggsave(
    filename = file.path(ruta_salida, paste0(.y, ".svg")),
    plot = .x,
    width = 10,
    height = 6,
    units = "in"
  )
)


library(officer)
library(rvg)

ppt <- read_pptx()

graficas <- list(
  grafica_1_por_semana,
  grafica_2_por_semana,
  grafica_3_por_semana
)

for (g in graficas) {
  ppt <- ppt %>% 
    add_slide(layout = "Blank", master = "Office Theme") %>% 
    ph_with(
      value = dml(ggobj = g),
      location = ph_location_fullsize()
    )
}

print(
  ppt,
  target = "C:/Users/brittany.pereo/Downloads/graficas_semana_editables.pptx"
)
