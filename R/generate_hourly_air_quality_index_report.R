generate_hourly_air_quality_index_report <- function(date_time, measurements_data, control, limits, intervals,
                                                     locations, parameters, categories, indexes) {
  index_control <- get_index_control(control)
  report <- list(
    "dateTime" <- date_time
  )

  for (i in seq_len(nrow(index_control))) {
    index <- get_index(index_control[i,], measurements_data, indexes, intervals, categories, parameters, limits)
  }
}

get_index_control <- function(control) {
  values <- cbind(control[1], stack(control[-1]))
  return(setNames(values, c("IndexCode", "Status", "LocationCode")))
}

get_index <- function(index_control, measurements_data, indexes, intervals, categories, parameters, limits) {
  if (index_control$Status == -1) {
    return(NA)
  }

  if (index_control$Status == 0) {
    return(0)
  }

  index_options <- find_row_by_row_name(indexes, index_control$IndexCode, "Code")
  limit_values <- find_row_by_row_name(limits, index_options$ParameterCode, "ParameterCode")
  measurements <- measurements_data[[paste(index_control$LocationCode, index_options$ParameterCode, sep = "_")]]
  parameter_options <- find_row_by_row_name(parameters, index_options$ParameterCode, "Code")

  measurements <- as.numeric(clear_measurements_data(measurements, limit_values$Min, limit_values$Max))

  if (length(measurements) == 0) {
    return(NA)
  }

  return(
    get_air_quality_index_by_measurement(measurements, index_options, parameter_options, intervals, categories)
  )
}
