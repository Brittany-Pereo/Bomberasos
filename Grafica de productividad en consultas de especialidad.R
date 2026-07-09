library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(svglite)

# VALIDACION --------------------------------------------------------------
cubos_2020_2025 <- arrow::read_parquet(
  "C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/Productividad/Productividad - Cubos/Cubos_completos_2020_2025.parquet"
) %>% 
  filter(fecha >= "2020-09-01",
         fecha <= "2025-12-31")

# resumen_2025 <- cubos_2020_2025 %>%
#   group_by(anio) %>%
#   summarise(
#     across(where(is.numeric), ~sum(.x, na.rm = TRUE)),
#     .groups = "drop")
# resumen_2025
# 
# cubos_2020_2024 <- arrow::read_parquet(
#  "C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/Productividad/Productividad - Cubos/Productividad de Cubos 2020-2024/Cubos_completos_2020_2024.parquet"
#  )
# 
# resumen_2024 <- cubos_2020_2024 %>%
#   group_by(anio) %>%
#   summarise(
#     across(where(is.numeric), ~sum(.x, na.rm = TRUE)),
#     .groups = "drop")
# resumen_2024
# DATOS DE ESPECIALIDAD ---------------------------------------------------
catalogo_clues <- arrow::read_parquet(
  "C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/CLUES/clues.parquet"
) %>% 
  select(clues_imb, clues_ssa_y_sme)

vector_clues <- c(catalogo_clues$clues_imb, catalogo_clues$clues_ssa_y_sme) %>%
  unique() %>%
  na.omit()

data_especialidad_con_bienestar <- cubos_2020_2025 %>% 
  select(clues, anio, mes, consultas_de_especialidad) %>% 
  filter(clues %in% vector_clues) %>% 
  mutate(origen = "Con IMSS Bienestar") 

data_especialidad_sin_bienestar <- cubos_2020_2025 %>% 
  select(clues, anio, mes, consultas_de_especialidad) %>% 
  filter(!clues %in% vector_clues) %>% 
  mutate(origen = "Sin IMSS Bienestar")

data_especialidad <- bind_rows(
  data_especialidad_con_bienestar, data_especialidad_sin_bienestar) %>% 
  group_by(anio, mes, origen) %>% 
  summarise(total_de_consultas = sum(consultas_de_especialidad, na.rm = TRUE),
            .groups = "drop")

data_plot <- data_especialidad %>%
  filter(!is.na(mes), mes != "Unknown") %>%
  mutate(
    mes_num = case_when(
      mes == "Enero" ~ 1,
      mes == "Febrero" ~ 2,
      mes == "Marzo" ~ 3,
      mes == "Abril" ~ 4,
      mes == "Mayo" ~ 5,
      mes == "Junio" ~ 6,
      mes == "Julio" ~ 7,
      mes == "Agosto" ~ 8,
      mes == "Septiembre" ~ 9,
      mes == "Octubre" ~ 10,
      mes == "Noviembre" ~ 11,
      mes == "Diciembre" ~ 12
    ),
    fecha = ymd(paste(anio, mes_num, 1, sep = "-"))
  ) %>%
  arrange(fecha)

# GRAFICA -----------------------------------------------------------------
grafica_especialidad <- ggplot(
  data_plot,
  aes(
    x = fecha,
    y = total_de_consultas / 1e3,
    color = origen,
    group = origen
  )
) +
  geom_line(linewidth = 1.05, alpha = 0.9) +
  geom_point(size = 2.3, alpha = 0.95) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 0.8,
    linetype = "dashed",
    color = "#2F2F2F"
  ) +
  scale_color_manual(values = c(
    "Con IMSS Bienestar" = "#C8AA73",
    "Sin IMSS Bienestar" = "#6D6D6D"
  )) +
  scale_y_continuous(
    labels = label_number(accuracy = 1, big.mark = ","),
    name = "Total de consultas (miles)"
  ) +
  scale_x_date(
    limits = c(as.Date("2020-01-01"), as.Date("2025-12-31")),
    breaks = seq(
      as.Date("2020-01-01"),
      as.Date("2025-12-31"),
      by = "1 year"
    ),
    date_labels = "%Y",
    expand = c(0, 0)
  )+
  labs(
    title = "Productividad comparada IMSS Bienestar vs No IMSS Bienestar",
    subtitle = "Consultas de especialidad por mes – Con IMSS Bienestar vs. Sin IMSS Bienestar (2020-2026)",
    x = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 17,
      colour = "#111111"
    ),
    plot.subtitle = element_text(
      face = "bold.italic",
      size = 13,
      colour = "#4E8A7A",
      margin = margin(b = 15)
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 11,
      colour = "#111111",
      margin = margin(r = 10)
    ),
    axis.text = element_text(
      size = 10,
      colour = "#333333"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      linewidth = 0.35,
      colour = "#E5E5E5"),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    plot.margin = margin(15, 25, 15, 15))
grafica_especialidad

# IMPRESION ---------------------------------------------------------------
svglite(
  filename = "C:/Users/brittany.pereo/Downloads/grafica_especialidad.svg",
  width = 12,
  height = 6)
print(grafica_especialidad)
dev.off()
