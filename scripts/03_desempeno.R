# Compara las tres coberturas y examina los rangos de caudal con CONAF.
# Ejecutar después de los scripts 01 y 02.
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

observados_validos <- caudal_mensual |>
  filter(!is.na(q_obs)) |>
  select(fecha, q_obs)

pares <- swat_mensual |>
  inner_join(observados_validos, by = "fecha", relationship = "many-to-one") |>
  group_by(fecha) |>
  filter(n_distinct(cobertura) == length(coberturas)) |>
  ungroup()

meses_excluidos <- caudal_mensual |>
  anti_join(distinct(pares, fecha), by = "fecha") |>
  mutate(motivo = if_else(is.na(q_obs), "Observación mensual incompleta",
                          "Mes ausente en alguna corrida"))

stopifnot("Se necesitan al menos tres meses comunes." = n_distinct(pares$fecha) >= 3L)

metricas_caudal <- function(datos) {
  obs <- datos$q_obs
  sim <- datos$q_sim
  error <- sim - obs
  media_obs <- mean(obs)
  sd_obs <- sd(obs)
  sd_sim <- sd(sim)
  variacion_obs <- sum((obs - media_obs)^2)

  r <- if (length(obs) >= 3L && sd_obs > 0 && sd_sim > 0) {
    cor(sim, obs)
  } else {
    NA_real_
  }

  alpha <- if (isTRUE(sd_obs > 0)) sd_sim / sd_obs else NA_real_
  beta <- if (media_obs > 0) mean(sim) / media_obs else NA_real_

  tibble(
    n_meses = length(obs),
    rmse = sqrt(mean(error^2)),
    nse = if (variacion_obs > 0) 1 - sum(error^2) / variacion_obs else NA_real_,
    # Convención simulado menos observado: negativo indica subestimación.
    pbias_pct = 100 * (beta - 1),
    # KGE original de Gupta et al. (2009).
    kge = 1 - sqrt((r - 1)^2 + (alpha - 1)^2 + (beta - 1)^2),
    r = r,
    alpha = alpha,
    beta = beta
  )
}

desempeno_coberturas <- pares |>
  group_by(cobertura) |>
  group_modify(~ metricas_caudal(.x)) |>
  ungroup()

conaf <- filter(pares, cobertura == "CONAF")
umbrales <- quantile(conaf$q_obs, probs = c(0.25, 0.75), names = FALSE)

stopifnot("Los umbrales P25 y P75 deben distinguirse." = umbrales[1] < umbrales[2])

umbrales_conaf <- tibble(p25_m3s = umbrales[1], p75_m3s = umbrales[2])

conaf <- conaf |>
  mutate(
    rango = case_when(
      q_obs <= umbrales[1] ~ "Bajo",
      q_obs >= umbrales[2] ~ "Alto",
      TRUE ~ "Intermedio"
    ),
    rango = factor(rango, levels = c("Bajo", "Intermedio", "Alto"))
  )

diagnostico_conaf <- conaf |>
  group_by(rango) |>
  group_modify(~ bind_cols(
    tibble(
      mediana_obs = median(.x$q_obs),
      mediana_sim = median(.x$q_sim),
      error_mediano = median(.x$q_sim - .x$q_obs)
    ),
    metricas_caudal(.x)
  )) |>
  ungroup()

# Conserva el calendario completo para que los meses excluidos dejen cortes.
serie_caudal <- tidyr::expand_grid(
  cobertura = factor(coberturas, levels = coberturas),
  fecha = caudal_mensual$fecha
) |>
  left_join(
    select(pares, cobertura, fecha, q_obs, q_sim),
    by = c("cobertura", "fecha"), relationship = "one-to-one"
  ) |>
  pivot_longer(c(q_obs, q_sim), names_to = "serie", values_to = "caudal") |>
  mutate(serie = recode(serie, q_obs = "Observado", q_sim = "Simulado"))

fig_caudal <- ggplot(serie_caudal, aes(fecha, caudal, colour = serie, linetype = serie)) +
  geom_line(linewidth = 0.45, na.rm = TRUE) +
  facet_wrap(vars(cobertura), ncol = 1) +
  scale_colour_manual(values = c(Observado = "#264653", Simulado = "#A64C1F")) +
  scale_linetype_manual(values = c(Observado = "solid", Simulado = "longdash")) +
  labs(
    x = "Año", y = "Caudal medio mensual (m³/s)", colour = NULL, linetype = NULL,
    caption = "La comparación usa los mismos meses para las tres coberturas."
  ) +
  theme_minimal() +
  theme(legend.position = "top")

dir.create("output", showWarnings = FALSE)
dir.create("figuras", showWarnings = FALSE)
write_csv(desempeno_coberturas, "output/desempeno_coberturas.csv", na = "")
write_csv(diagnostico_conaf, "output/diagnostico_conaf.csv", na = "")
write_csv(umbrales_conaf, "output/umbrales_conaf.csv")
write_csv(caudal_mensual, "output/control_caudal_mensual.csv", na = "")
write_csv(meses_excluidos, "output/meses_excluidos.csv", na = "")
ggsave("figuras/caudal_coberturas.png", fig_caudal, width = 9, height = 6, dpi = 300)
