generate_hourly_air_quality_index_report <- function(date_time, measurements_data, control, limits, intervals,
                                                     locations, parameters, categories, indexes) {
  index_control <- get_index_control(control)
  report <- list(
    "dateTime" <- date_time
  )

  for (i in seq_len(nrow(index_control))) {
    print(index_control[i,])
    index <- get_index(index_control[i,], measurements_data, indexes, intervals, categories, parameters)
  }
}

get_index_control <- function(control) {
  values <- cbind(control[1], stack(control[-1]))
  return(setNames(values, c("IndexCode", "Status", "LocationCode")))
}

get_index <- function(index_control, measurements_data, indexes, intervals, categories, parameters) {
  if (index_control$Status == -1) {
    return(NA)
  }

  if (index_control$Status == 0) {
    return(0)
  }

  index_options <- find_row_by_row_name(indexes, index_control$IndexCode, "Code")
  measurements <- measurements_data[[paste(index_control$LocationCode, index_options$ParameterCode, sep = "_")]]

  if (length(measurements) == 0) {
    return(NA)
  }

  return(
    get_air_quality_index_by_measurement(measurements, index_options, intervals, categories, parameters)
  )
}
