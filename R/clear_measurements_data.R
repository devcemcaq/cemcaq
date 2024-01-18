clear_measurements_data <- function(measurements, min_value = NULL, max_value = NULL) {
  measurements[suppressWarnings(as.numeric(measurements)) == -9999] <- "ND"

  if (!is.null(min_value) && min_value <= 0) {
    stop("min_value must be greater than zero")
  }

  if (!is.null(max_value) && max_value <= 0) {
    stop("max_value must be greater than zero")
  }

  if (!is.null(min_value))
  {

    measurements[suppressWarnings(as.numeric(measurements)) < min_value] <- "IR"
  }
  if (!is.null(max_value))
  {
    measurements[suppressWarnings(as.numeric(measurements)) > max_value] <- "IR"
  }
  measurements[suppressWarnings(as.numeric(measurements)) == 0] <- "VZ"
  measurements[suppressWarnings(as.numeric(measurements)) < 0] <- "NN"

  return(measurements)
}