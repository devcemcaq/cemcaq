generate_hourly_air_quality_index_report <- function(date_time, measurements_data, control, limits, intervals,
                                                     locations, parameters, categories, indexes) {
  index_control <- get_index_control(control)
  index_values <- NULL
  category_values <- data.frame()
  location_values <- data.frame()
  parameter_values <- data.frame()

  for (i in seq_len(nrow(index_control))) {
    index <- get_index(index_control[i,], measurements_data, indexes, intervals, categories, parameters, locations, limits)
    index_values <- append(index_values, index$Index)

    category_values <- rbind(category_values, as.data.frame(index$Category))
    location_values <- rbind(location_values, as.data.frame(index$Location))
    parameter_values <- rbind(parameter_values, as.data.frame(index$Parameter))
  }

  report <- data.frame(Index = index_values)
  report$Category <- category_values
  report$Location <- location_values
  report$Parameter <- parameter_values

  return(list(
    DateTime = date_time,
    Results = report
  ))
}

get_index_control <- function(control) {
  values <- cbind(control[1], stack(control[-1]))
  return(setNames(values, c("IndexCode", "Status", "LocationCode")))
}

get_index <- function(index_control, measurements_data, indexes, intervals, categories, parameters, locations, limits) {

  status <- index_control$Status
  index_options <- find_row_by(indexes, "Code", index_control$IndexCode)
  limit_values <- find_row_by(limits, "ParameterCode", index_options$ParameterCode)

  measurement_name <- paste(index_control$LocationCode, index_options$ParameterCode, sep = "_")
  measurement_name <- str_replace_all(measurement_name, "\\.", "")

  measurements <- measurements_data[[measurement_name]]

  parameter_options <- find_row_by(parameters, "Code", index_options$ParameterCode)
  location_options <- find_row_by(locations, "Code", index_control$LocationCode)

  measurements <- as.numeric(clear_measurements_data(measurements, limit_values$Min, limit_values$Max))

  index <- get_air_quality_index_by_measurement(
    measurements,
    index_options,
    parameter_options,
    intervals,
    categories,
    status
  )

  index$Location <- location_options
  index$Parameter <- parameter_options

  return(index)
}
