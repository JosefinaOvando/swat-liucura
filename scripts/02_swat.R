# Lectura de las salidas mensuales de SWAT+. Ejecutar después de 01_camels.R.
library(dplyr)
library(lubridate)

coberturas <- c("CONAF", "MapBiomas", "CLDynamic")
# cha001 es el canal candidato del borrador. Confirma su correspondencia espacial.
canales <- c(CONAF = 1L, MapBiomas = 1L, CLDynamic = 1L)

leer_canal <- function(cobertura, canal) {
  ruta <- file.path("datos", "swat", cobertura, "channel_sd_mon.csv")
  encabezado <- readLines(ruta, n = 3, warn = FALSE)
  if (length(encabezado) != 3L) {
    stop("Encabezado SWAT+ incompleto en ", ruta, call. = FALSE)
  }

  nombres <- trimws(strsplit(encabezado[2], ",", fixed = TRUE)[[1]])
  unidades <- trimws(strsplit(encabezado[3], ",", fixed = TRUE)[[1]])
  esperadas <- c(flo_out = "m^3/s", sed_out = "tons", no3_out = "kgN")
  campos <- c("unit", "yr", "mon", names(esperadas))
  stopifnot(
    "Faltan campos requeridos en el encabezado SWAT+" = all(campos %in% nombres),
    "Unidades inesperadas en la salida SWAT+" =
      identical(unname(esperadas), unidades[match(names(esperadas), nombres)])
  )

  # Lee únicamente las seis columnas requeridas del archivo de canal.
  datos <- data.table::fread(
    ruta, skip = 3L, header = FALSE, sep = ",",
    select = match(campos, nombres), col.names = campos, showProgress = FALSE
  )
  seleccion <- datos |>
    filter(unit == canal) |>
    as_tibble()
  if (nrow(seleccion) == 0L || !all(seleccion$mon %in% 1:12)) {
    stop("No hay registros válidos para el canal ", canal, " en ", ruta, call. = FALSE)
  }

  seleccion |>
    transmute(
      cobertura = cobertura,
      fecha = make_date(yr, mon, 1L),
      q_sim = as.numeric(flo_out),
      sed_t_mes = as.numeric(sed_out),
      no3_kg_n_mes = as.numeric(no3_out)
    )
}

swat_mensual <- purrr::map2(
  coberturas, unname(canales[coberturas]), leer_canal
) |>
  purrr::list_rbind()

stopifnot("SWAT+: hay fechas inválidas" = !anyNA(swat_mensual$fecha))

swat_mensual <- swat_mensual |>
  filter(between(fecha, inicio, fin)) |>
  mutate(cobertura = factor(cobertura, levels = coberturas)) |>
  arrange(cobertura, fecha)

stopifnot(
  "SWAT+: falta una cobertura en el periodo" =
    n_distinct(swat_mensual$cobertura) == length(coberturas),
  "SWAT+: se repite una combinación de cobertura y mes" =
    anyDuplicated(swat_mensual[c("cobertura", "fecha")]) == 0L,
  "SWAT+: hay caudales inválidos" =
    all(is.finite(swat_mensual$q_sim) & swat_mensual$q_sim >= 0)
)
