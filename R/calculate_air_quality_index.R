calidad_aire_actual <- function(locations, parameters, measurements, control, date_time) {
  validate_data_inputs(locations, parameters, measurements, control, date_time)
}

validate_data_inputs <- function(locations, parameters, control, measurements, dateTime) {
  if (utils.is_data_empty(locations)) {
    stop("Locations has no valid data.")
  }

  if (utils.is_data_empty(parameters)) {
    stop("Parameters has no valid data.")
  }

  if (utils.is_data_empty(control)) {
    stop("Control has no valid data.")
  }

  if (is.null(dateTime) || is.na(dateTime)) {
    stop("DateTime")
  }

  if (utils.is_data_empty(measurements)) {
    stop("Measurements has no valid data.")
  }
}