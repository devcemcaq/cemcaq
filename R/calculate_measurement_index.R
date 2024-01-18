#' Calcula los indices de calidad del aire de un set de mediciones.
#'
#' @param measurements Un vector numerico que representa las mediciones para los calculos,
#' debe de tener al menos un valor. Los valores deben estar ordenados con los mas antiguos primero, mas recientes al
#' final. Se debe considerar las mediciones deben ser continuas, si existen falta de datos, se debe previamente
#' rellenar con `NAs`.
#' @param hours Cantidad de horas que se considerarán para el calculo. Por defecto 1.
#' @param weighted Indica si el calculo es para concentración promedio movil ponderado. FALSE por defecto.
#' @param relevant_gap Indica cuantos de los registros mas recientes deben considerarse como obligatorios para la
#' concentracion promedio movil ponderado (weighted = TRUE). No debe ser menor a 0 en caso de usarse, y no debe ser
#' mayor al valor de hours.
#' @param min_relevant_records Indica cuantas de las registros mas recientes (indicados por relevant_gap) son
#' obligatorios para que se puede hacer el calculo para concentración promedio móvil ponderado.
#'
#' @return Vector numerico que representa el Indice de calidad del aire.
#'
#' @examples
#' # Indice horario (1 hora)
#' index <- calculate_measurements_index(c(1,2,3))
#'
#' # Indice 8 horas
#' index <- calculate_measurements_index(c(1,2,3,4,5,6,7,8), hours=8)
#'
#' # Indice 12 horas ponderado
#' index <- calculate_measurements_index(
#'  c(1,2,3,4,5,6,7,8,9,10,11,12),
#'  hours=12,
#'  weighted=TRUE,
#'  relevant_gap=3,
#'  min_relevant_records=2
#' )
#' @export
calculate_measurements_index <- function(measurements, hours = 1, weighted = FALSE, relevant_gap = 3,
                                         min_relevant_records = 2) {
  if (hours < 1) {
    stop("Hours must be greater than zero")
  }

  if (hours == 1) {
    return(get_last_hour_measurement(measurements))
  }

  if (weighted) {
    return(calculate_weighted_moving_concentration(measurements, hours, relevant_gap, min_relevant_records))
  }

  return(calculate_hourly_average(measurements, hours))
}

get_last_hour_measurement <- function(measurements) {
  return(utils::tail(measurements, n = 1))
}

#' Calcula el promedio horario
#'
#' @param measurements Vector numerico con las mediciones.
#' @param hours Numero de horas correspondientes al calculo.
#' @param percentage_min_records Porcentaje minimo de registros que se necesitan para el calculo. Por defecto 75%.
#'
#' @return Vector numerico con el promedio calculado
calculate_hourly_average <- function(measurements, hours, percentage_min_records = 0.75) {
  min_records <- floor(hours * percentage_min_records)
  clean_measurements <- measurements[!is.na(measurements)]
  if (length(clean_measurements) < min_records) {
    return(NA)
  }
  return(mean(clean_measurements))
}

calculate_weighted_moving_concentration <- function(measurements, hours, relevant_gap, min_relevant_records) {
  if (length(measurements) != hours) {
    stop("Measurements count must be equals to the number of hours to evaluate")
  }

  if (relevant_gap < 0) {
    stop("Relevant gap must be greater than zero")
  }

  if (relevant_gap > length(measurements)) {
    stop("Relevant gap must be less or equal than the number of hours to evaluate")
  }

  if (min_relevant_records <= 0) {
    stop("Minimum number of relevant records must be greater than zero")
  }

  if (min_relevant_records > relevant_gap) {
    stop("Minimum number of relevant records must be less or equal than the relevant gap value")
  }

  relevant_gap <- utils::tail(measurements, n = relevant_gap)
  if (length(relevant_gap[!is.na(relevant_gap)]) < min_relevant_records) {
    return(NA)
  }

  weight <- get_moving_concentration_weight(measurements)
  factor <- 0
  divider <- 0
  for (i in 0:(hours - 1)) {
    value <- measurements[hours - i]
    if (is.na(value) || is.null(value)) {
      next
    }
    factor <- factor + (value * weight^i)
    divider <- divider + (weight^i)
  }
  media_movil_ponderada <- factor / divider
  return(media_movil_ponderada)
}

get_moving_concentration_weight <- function(x) {
  x <- x[!is.na(x)]
  weight <- 1 - (max(x) - min(x)) / max(x)
  return(ifelse(weight > 0.5, round(weight, 2), 0.5))
}