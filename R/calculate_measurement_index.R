calculate_measurements_index <- function(measurements, hours = 1, weighted = FALSE, relevant_gap = 3, min_relevant_records = 2) {
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
  return(tail(measurements, n = 1))
}

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

  relevant_gap <- tail(measurements, n = relevant_gap)
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