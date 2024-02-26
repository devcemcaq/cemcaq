round_value <- function(value, digits = 0) {
  return(round(value, digits))
}

common.get_measurement_column_names_from_config <- function(parameters, locations, append_columns = NULL) {
  parameter_codes <- parameters[parameters$IsComputed < 1,]$Code
  location_codes <- locations$Code

  column_names <- NULL

  for (location in location_codes) {
    for (parameter in parameter_codes) {
      column_name <- paste(location, parameter, sep = "_")
      if (is.null(column_names)) {
        column_names <- column_name
      }else {
        column_names <- c(column_names, column_name)
      }
    }
  }

  if (!is.null(append_columns)) {
    column_names <- c(column_names, append_columns)
  }

  return(column_names)
}

common.get_parameter_code_from_column_name <- function(column_name) {
  return(strsplit(column_name, "_")[[1]][2])
}

common.get_location_code_from_column_name <- function(column_name) {
  return(strsplit(column_name, "_")[[1]][1])
}
