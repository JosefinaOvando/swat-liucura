# Descarga y preparación del caudal observado. Ejecutar desde la raíz del proyecto.
library(dplyr)
library(lubridate)
library(readr)

inicio <- as.Date("1984-01-01")
fin <- as.Date("2019-12-31")
ruta_camels <- file.path("datos", "observados", "camels_9416001_diario.csv")

descargar_camels <- function(inicio, fin) {
  serie <- RcamelsCL::getData(
    x = 9416001, tscale = "daily", from = inicio, to = fin
  )
  columna <- intersect(c("Qobs_m3s_day", "Qobs_m3s"), colnames(serie))

  if (length(columna) != 1L) {
    stop("CAMELS-CL debe entregar una columna de caudal en m³/s.", call. = FALSE)
  }

  tibble(
    fecha = as.Date(zoo::index(serie)),
    q_obs = as.numeric(serie[, columna])
  )
}

validar_diario <- function(diario) {
  stopifnot(
    "CAMELS-CL: faltan fechas, se repiten o están fuera del periodo" =
      nrow(diario) > 0L && !anyNA(diario$fecha) &&
      anyDuplicated(diario$fecha) == 0L &&
      all(diario$fecha >= inicio & diario$fecha <= fin),
    "CAMELS-CL: se esperaba al menos un valor de caudal válido" =
      any(is.finite(diario$q_obs) & diario$q_obs >= 0)
  )
}

if (!file.exists(ruta_camels)) {
  diario <- descargar_camels(inicio, fin)
  validar_diario(diario)
  dir.create(dirname(ruta_camels), recursive = TRUE, showWarnings = FALSE)
  write_csv(diario, ruta_camels, na = "")
} else {
  diario <- read_csv(
    ruta_camels,
    col_types = cols(fecha = col_date(), q_obs = col_double())
  )
  stop_for_problems(diario)
  validar_diario(diario)
}

# El calendario permite contar los días ausentes de la serie.
caudal_mensual <- tibble(fecha = seq.Date(inicio, fin, by = "day")) |>
  left_join(diario, by = "fecha", relationship = "one-to-one") |>
  mutate(
    q_obs = if_else(is.finite(q_obs) & q_obs >= 0, q_obs, NA_real_),
    fecha = floor_date(fecha, "month")
  ) |>
  group_by(fecha) |>
  summarise(
    dias_calendario = n(),
    dias_validos = sum(!is.na(q_obs)),
    q_obs = if (all(!is.na(q_obs))) mean(q_obs) else NA_real_,
    .groups = "drop"
  )

message(
  "CAMELS-CL: ", nrow(caudal_mensual), " meses calendario; ",
  sum(!is.na(caudal_mensual$q_obs)), " con todos sus días válidos."
)
