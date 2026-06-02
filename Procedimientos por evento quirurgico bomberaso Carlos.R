library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

# Funciones ---------------------------------------------------------------
hacer_grafica_2 <- function(df, variable_fecha, titulo) {
  
  factor_escala <- max(df$numero_de_procedimientos, na.rm = TRUE) /
    max(df$relacion, na.rm = TRUE)
  
  ggplot(df, aes(x = {{ variable_fecha }})) +
    geom_line(aes(y = numero_eventos, color = "Eventos"), linewidth = 1) +
    geom_line(aes(y = numero_de_procedimientos, color = "Procedimientos"), linewidth = 1) +
    geom_line(
      aes(y = relacion * factor_escala, color = "Relación"),
      linewidth = 1.2,
      linetype = 2
    ) +
    scale_y_continuous(
      name = "Eventos / Procedimientos",
      labels = comma,
      sec.axis = sec_axis(
        ~ . / factor_escala,
        name = "Procedimientos por evento"
      )
    ) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(
      x = NULL,
      color = NULL,
      title = titulo
    ) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# Bases -------------------------------------------------------------------
egresos_2026 <- arrow:: read_parquet(
  "C:/Users/liape/Downloads/egresos_sin_thanificar_01_01_2026_a_27_05_2026.parquet"
) %>% 
  janitor::clean_names() %>% 
  select(clues, folio, fecha_egreso) %>% 
  mutate(origen = "egresos")

procedimientos_2026 <- arrow::read_parquet(
  "C:/Users/liape/Downloads/procedimientos_sin_thanificar_01_01_2026_a_27_05_2026.parquet"
) %>% 
  janitor::clean_names() %>% 
  select(clues, folio, fecha_egreso, numero_procedimiento) %>% 
  mutate(origen = "procedimientos")

df_limpio <- egresos %>% 
  left_join(procedimientos,
            by = c("clues", "folio", "fecha_egreso"),
            suffix = c("_egreso", "_procedimiento")) %>% 
  mutate(fecha_egreso = as.Date(fecha_egreso),
         origen = case_when(
           !is.na(origen_egreso) & !is.na(origen_procedimiento) ~ "egreso con procedimiento",
           !is.na(origen_egreso) &  is.na(origen_procedimiento) ~ "solo egreso",
           TRUE ~ "revisar"))

# -------------------------------------------------------------------------
# GRAFICA 1
# -------------------------------------------------------------------------
# Por dia -----------------------------------------------------------------
df_grafica_1_dia <- df_limpio %>% 
  distinct(clues, folio, fecha_egreso, origen) %>% 
  count(fecha_egreso, origen, name = "numero_eventos") %>% 
  bind_rows(df_limpio %>% 
              distinct(clues, folio, fecha_egreso, origen) %>% 
              count(fecha_egreso, name = "numero_eventos") %>% 
              mutate(origen = "total"))

grafica_1_por_dia <- ggplot(
  df_grafica_1_dia,
  aes(fecha_egreso, numero_eventos, color = origen)) +
  geom_line(linewidth = 1.1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(labels = comma) +
  labs(x = NULL, y = "Eventos", color = NULL) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1))
grafica_1_por_dia
# Por semana --------------------------------------------------------------
df_grafica_1_semana <- df_limpio %>% 
  distinct(clues, folio, fecha_egreso, origen) %>% 
  mutate(
    semana = floor_date(fecha_egreso, "week", week_start = 1)
  ) %>% 
  count(semana, origen, name = "numero_eventos") %>% 
  bind_rows(
    df_limpio %>% 
      distinct(clues, folio, fecha_egreso, origen) %>% 
      mutate(
        semana = floor_date(fecha_egreso, "week", week_start = 1)
      ) %>% 
      count(semana, name = "numero_eventos") %>% 
      mutate(origen = "total"))

grafica_1_por_semana <- ggplot(
  df_grafica_1_semana,
  aes(semana, numero_eventos, color = origen)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d-%b") +
  scale_y_continuous(labels = comma) +
  labs(x = NULL, y = "Eventos", color = NULL) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1))
grafica_1_por_semana
# Por mes -----------------------------------------------------------------
df_grafica_1_mes <- df_limpio %>% 
  distinct(clues, folio, fecha_egreso, origen) %>% 
  mutate(mes = floor_date(fecha_egreso, "month")) %>% 
  count(mes, origen, name = "numero_eventos") %>% 
  bind_rows(df_limpio %>% 
              distinct(clues, folio, fecha_egreso, origen) %>% 
              mutate(mes = floor_date(fecha_egreso, "month")) %>% 
              count(mes, name = "numero_eventos") %>% 
              mutate(origen = "total"))

grafica_1_por_mes <- ggplot(
  df_grafica_1_mes,
  aes(mes, numero_eventos, color = origen)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(labels = comma) +
  labs(x = NULL, y = "Eventos", color = NULL) +
  theme_bw()
grafica_1_por_mes
# -------------------------------------------------------------------------
#GRAFICA 2
# -------------------------------------------------------------------------
# Por dia -----------------------------------------------------------------
df_grafica_2_dia <- df_limpio %>% 
  filter(!is.na(origen_procedimiento)) %>% 
  group_by(clues, folio, fecha_egreso) %>% 
  summarise(
    numero_de_procedimientos_por_evento = n_distinct(numero_procedimiento),
    .groups = "drop") %>% 
  group_by(fecha_egreso) %>% 
  summarise(
    numero_eventos = n_distinct(paste(clues, folio, sep = "_")),
    numero_de_procedimientos = sum(numero_de_procedimientos_por_evento, na.rm = TRUE),
    relacion = numero_de_procedimientos / numero_eventos,
    .groups = "drop")

grafica_2_por_dia <- hacer_grafica_2(
  df_grafica_2_dia,
  fecha_egreso,
  "Eventos, procedimientos y relación diaria")
grafica_2_por_dia
# Por semana --------------------------------------------------------------
df_grafica_2_semana <- df_grafica_2_dia %>% 
  mutate(
    semana = floor_date(fecha_egreso, "week", week_start = 1)) %>% 
  group_by(semana) %>% 
  summarise(
    numero_eventos = sum(numero_eventos, na.rm = TRUE),
    numero_de_procedimientos = sum(numero_de_procedimientos, na.rm = TRUE),
    relacion = numero_de_procedimientos / numero_eventos,
    .groups = "drop")

grafica_2_por_semana <- hacer_grafica_2(
  df_grafica_2_semana,
  semana,
  "Eventos, procedimientos y relación semanal")
grafica_2_por_semana
# Por mes -----------------------------------------------------------------
df_grafica_2_mes <- df_grafica_2_dia %>% 
  mutate(mes = floor_date(fecha_egreso, "month")) %>% 
  group_by(mes) %>% 
  summarise(
    numero_eventos = sum(numero_eventos, na.rm = TRUE),
    numero_de_procedimientos = sum(numero_de_procedimientos, na.rm = TRUE),
    relacion = numero_de_procedimientos / numero_eventos,
    .groups = "drop")

grafica_2_por_mes <- hacer_grafica_2(
  df_grafica_2_mes,
  mes,
  "Eventos, procedimientos y relación mensual")
grafica_2_por_mes

# GUARDAR -----------------------------------------------------------------
# Carpeta destino
ruta_salida <- "C:/Users/liape/Downloads"

# Lista de gráficas
graficas <- list(
  grafica_1_por_dia = grafica_1_por_dia,
  grafica_1_por_semana = grafica_1_por_semana,
  grafica_1_por_mes = grafica_1_por_mes,
  grafica_2_por_dia = grafica_2_por_dia,
  grafica_2_por_semana = grafica_2_por_semana,
  grafica_2_por_mes = grafica_2_por_mes
)

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

