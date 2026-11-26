# Resume concentraciones observadas de la DGA en una figura de tres paneles.
# Puede ejecutarse desde la raíz del proyecto sin correr los otros scripts.
library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)

carpeta_dga <- file.path("datos", "observados")

leer_dga <- function(archivo, campos) {
  columnas <- setNames(rep(list(col_character()), length(campos)), campos)
  rutas <- file.path(carpeta_dga, c(archivo, sub("(2).txt", ".txt", archivo, fixed = TRUE)))
  rutas <- rutas[file.exists(rutas)]
  if (length(rutas) != 1L) {
    stop("Deja una sola copia de ", archivo, " en ", carpeta_dga,
         " (con o sin el sufijo (2)).", call. = FALSE)
  }
  datos <- read_csv(
    rutas[1],
    col_types = do.call(cols_only, columnas),
    trim_ws = TRUE
  )
  stop_for_problems(datos)
  if (!all(campos %in% names(datos))) {
    stop("Faltan columnas requeridas en ", archivo, call. = FALSE)
  }
  datos
}

rutinario <- leer_dga(
  "sedimentos_muestreo_rutinario_historico_9(2).txt",
  c("CODIGO ESTACION", "Año", "Mes", "Día", "Concentración", "Unidad")
) |>
  filter(`CODIGO ESTACION` == "09414001") |>
  transmute(
    fecha = make_date(as.integer(Año), as.integer(Mes), as.integer(Día)),
    valor = parse_double(Concentración),
    unidad = Unidad
  )

integrado <- leer_dga(
  "sedimentos_muestreo_integrado_historico_9(2).txt",
  c("CODIGO ESTACION", "Fecha", "Concentración_Ponderada_Diaria", "Unidad")
) |>
  filter(`CODIGO ESTACION` == "09414001") |>
  transmute(
    fecha = dmy(Fecha),
    valor = parse_double(Concentración_Ponderada_Diaria),
    unidad = Unidad
  )

nitrato <- leer_dga(
  "calidad_agua_historico_9(2).txt",
  c("COD. ESTACIÓN", "PARAMETRO", "FECHA MEDICION", "VALOR", "CALIDAD")
) |>
  filter(`COD. ESTACIÓN` == "09418001-5", PARAMETRO == "Nitrato") |>
  transmute(
    fecha = dmy(`FECHA MEDICION`),
    valor = parse_double(VALOR),
    calidad = coalesce(CALIDAD, "")
  )

stopifnot(
  "No hay registros en alguno de los tres conjuntos DGA" =
    nrow(rutinario) > 0L && nrow(integrado) > 0L && nrow(nitrato) > 0L,
  "La unidad de sedimentos debe ser mg/lt en ambos archivos" =
    all(c(rutinario$unidad, integrado$unidad) == "mg/lt"),
  "Hay marcas de calidad desconocidas en el nitrato" =
    all(nitrato$calidad %in% c("=", "", "<", "?"))
)

meses <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
           "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
mes_calendario <- function(fecha) factor(month(fecha), levels = 1:12, labels = meses)

control_nitrato <- nitrato |>
  mutate(mes = mes_calendario(fecha)) |>
  group_by(mes, .drop = FALSE) |>
  summarise(
    total = n(),
    incluidos = sum(calidad %in% c("=", "")),
    bajo_limite = sum(calidad == "<"),
    dudosos = sum(calidad == "?"),
    .groups = "drop"
  )

paneles <- c(
  "Trancura · Sedimentos rutinarios (mg/L)",
  "Trancura · Sedimentos integrados (mg/L)",
  "Quelhue · Nitrato (valor reportado por DGA)"
)

observaciones_dga <- bind_rows(
  transmute(rutinario, fecha, valor, panel = paneles[1]),
  transmute(integrado, fecha, valor, panel = paneles[2]),
  nitrato |>
    filter(calidad %in% c("=", "")) |>
    transmute(fecha, valor, panel = paneles[3])
)

stopifnot(
  "Hay fechas inválidas en los registros DGA" =
    !anyNA(c(observaciones_dga$fecha, nitrato$fecha)),
  "Hay concentraciones inválidas en los registros incluidos" =
    all(is.finite(observaciones_dga$valor) & observaciones_dga$valor >= 0),
  "Se requieren registros cuantificados de nitrato" =
    n_distinct(observaciones_dga$panel) == length(paneles)
)

observaciones_dga <- observaciones_dga |>
  mutate(
    panel = factor(panel, levels = paneles),
    mes = mes_calendario(fecha)
  )

estacional_dga <- observaciones_dga |>
  group_by(panel, mes) |>
  summarise(
    n = n(),
    mediana = median(valor),
    p25 = quantile(valor, 0.25),
    p75 = quantile(valor, 0.75),
    .groups = "drop"
  )

colores_dga <- setNames(c("#00786E", "#226585", "#85462A"), paneles)

fig_estacional <- ggplot(estacional_dga, aes(mes, mediana, colour = panel)) +
  geom_linerange(aes(ymin = p25, ymax = p75), linewidth = 0.9, alpha = 0.62) +
  geom_point(aes(size = n)) +
  facet_wrap(vars(panel), ncol = 1, scales = "free_y") +
  scale_colour_manual(values = colores_dga, guide = "none") +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(
    labels = scales::label_number(decimal.mark = ",", big.mark = "."),
    expand = expansion(mult = c(0.09, 0.13))
  ) +
  scale_size_continuous(
    name = "Registros por mes", range = c(2.1, 4.4), transform = "log10",
    breaks = c(1, 10, 100, 1000),
    labels = scales::label_number(decimal.mark = ",", big.mark = ".")
  ) +
  labs(x = "Mes calendario", y = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    text = element_text(colour = "#193944"),
    legend.position = "top",
    strip.text = element_text(hjust = 0, face = "bold", margin = margin(7, 0, 7, 0)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.spacing = grid::unit(1.05, "lines"),
    plot.margin = margin(8, 12, 8, 6)
  )

dir.create("output", showWarnings = FALSE)
dir.create("figuras", showWarnings = FALSE)
write_csv(estacional_dga, "output/estacional_dga.csv")
write_csv(control_nitrato, "output/control_nitrato_dga.csv")
ggsave("figuras/estacionalidad_dga.png", fig_estacional,
       width = 8, height = 7.3, dpi = 300, bg = "white")
