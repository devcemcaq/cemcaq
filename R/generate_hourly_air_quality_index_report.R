generate_hourly_air_quality_index_report <- function(date_time, measurements_data, control, limits, intervals,
                                                     locations, parameters, categories, indexes) {
  location_codes <- colnames(control)[-1]
  index_codes <- control$IndexCode

  measurements_data <- format_measurements_dataset(
    measurements_data,
    date_time,
    max(indexes$Hours),
    "Date_Time"
  )

  report <- get_report(control, indexes, measurements_data, intervals, categories, parameters, limits)

  return(list(
    DateTime = date_time,
    Locations = dataframe_to_list_with_key(
      get_locations_filtered_and_ordered_by_control(locations, location_codes),
      "Code"
    ),
    Indexes = dataframe_to_list_with_key(
      get_indexes_filtered_and_ordered_by_control(indexes, index_codes),
      "Code"
    ),
    Parameters = dataframe_to_list_with_key(parameters, "Code"),
    Categories = dataframe_to_list_with_key(categories, "Id"),
    Results = report
  ))
}

get_report <- function(control, indexes, measurements_data, intervals, categories, parameters, limits) {
  report <- list()

  for (control_index in seq_len(nrow(control))) {
    control_row <- control[control_index,]
    index_code <- control_row$IndexCode
    index_options <- find_row_by(indexes, "Code", index_code)

    index_report <- list()
    for (location_index in 2:ncol(control_row)) {
      location_code <- names(control_row)[location_index]
      index_status <<- control_row[[location_code]]

      index <- get_index(
        index_options,
        location_code,
        index_status,
        measurements_data,
        intervals,
        categories,
        parameters,
        limits
      )
      index_report[[location_code]] <- index
    }
    report[[index_code]] <- index_report
  }

  return(report)
}

get_index_control <- function(control) {
  values <- cbind(control[1], stack(control[-1]))
  return(setNames(values, c("IndexCode", "Status", "LocationCode")))
}

get_index <- function(index_options, location_code, index_status, measurements_data, intervals, categories, parameters, limits) {

  limit_values <- find_row_by(limits, "ParameterCode", index_options$ParameterCode)

  measurement_name <- paste(location_code, index_options$ParameterCode, sep = "_")
  measurement_name <- stringr::str_replace_all(measurement_name, "\\.", "")

  measurements <- measurements_data[[measurement_name]]

  parameter_options <- find_row_by(parameters, "Code", index_options$ParameterCode)

  measurements <- as.numeric(
    clear_measurements_data(
      measurements,
      limit_values$Min,
      limit_values$Max
    )
  ) * parameter_options$Scale

  index <- get_air_quality_index_by_measurement(
    measurements,
    index_options,
    parameter_options$DecimalDigits,
    intervals,
    categories,
    index_status
  )

  index$LocationCode <- location_code
  index$ParameterCode <- index_options$ParameterCode

  return(index)
}

get_locations_filtered_and_ordered_by_control <- function(locations, location_codes) {
  row.names(locations) <- locations$Code

  return(locations[location_codes,])
}

get_indexes_filtered_and_ordered_by_control <- function(indexes, index_codes) {
  row.names(indexes) <- indexes$Code

  return(indexes[index_codes,])
}
