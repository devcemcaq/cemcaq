calculate_measurements_index <- function(measurements, hours = 1, weighted = FALSE) {
  if (hours < 1) {
    stop("Hours must be greater than zero")
  }

  if (hours == 1) {
    # Promedio horario
    return(ifelse(length(measurements) > 1, measurements[1], measurements))
  }

  if (weighted) {
    # Concentracion promedio movil ponderado
    factor <- 0
    divider <- 0
    weight <- obtener_pesos(measurements)
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

  return(mean(measurements))
}

concentracion_movil_ponderada <- function(x) {
  weight <- obtener_pesos(x)
  return((sum(x) * weight) / sum(weight))
}

obtener_pesos <- function(x) {
  x <- x[!is.na(x)]
  weight <- 1 - (max(x) - min(x)) / max(x)
  return(ifelse(weight > 0.5, round(weight, 2), 0.5))
}