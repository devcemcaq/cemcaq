#'
clear_measurements_dataset <- function(measurements, limits, columns_to_use, current_date_time, hours_to_use = 24) {
  columns_to_use <- intersect(colnames(measurements), columns_to_use)
  measurements <- measurements[columns_to_use]

  measurements <- get_prepared_datasets(measurements, parameters, limits, hours_to_use)

  return(measurements)
}

format_date_time_column <- function(measurements, current_date_time, hours_to_use = 24) {
  init_date_time <- current_date_time - (hours_to_use * 60 * 60)
  measurements$Date_Time <- as.POSIXct(measurements$Date_Time)
  measurements <- measurements[which(measurements$Date_Time > init_date_time & measurements$Date_Time <= current_date_time)]
  measurements <- measurements(order(measurements$Date_Times))
}

get_prepared_datasets <- function(measurements, parameters, limits_dataset, hours_to_calculate = 24, date_time_column_name = "DateTime") {
  selected_measurements <- take_last_n_values(
    measurements[date_time_column_name], hours_to_calculate
  )

  ozono_processed_dataset <- selected_measurements
  ozono_processed_dataset_names <- date_time_column_name


  for (column_name in colnames(selected_measurements[, -1])) {
    column_data <- measurements[column_name]

    column_data_clone <- column_data
    parameter_code <- common.get_parameter_code_from_column_name(column_name)
    measurement_scale <- utils.select_row_by_row_name(parameters, parameter_code, "Code")$Scale


    column_data <- calculate_measurements_data(
      column_data, parameter_code, limits_dataset
    ) / measurement_scale

    digits_to_round <- COMPOUND_DECIMAL_DIGITS[[compound_code]]
    column_data <- round_values(column_data, digits_to_round)

    selected_measurements <- cbind(selected_measurements, column_data)

    if (parameter_code == "3O") {
      column_data_clone <- calculate_measurements_data(
        column_data_clone, "3OM", limits_dataset
      ) / PARTICLES_PER_MILLION_SCALE

      ozono_processed_dataset <- cbind(
        ozono_processed_dataset, column_data_clone
      )
      ozono_processed_dataset_names <- c(
        ozono_processed_dataset_names, column_name
      )
    }
  }

  colnames(selected_measurements) <- colnames(measurements)
  rownames(selected_measurements) <- as.character(1:(HOURS_TO_CALCULATE + 1))

  colnames(ozono_processed_dataset) <- ozono_processed_dataset_names
  rownames(ozono_processed_dataset) <- as.character(1:(HOURS_TO_CALCULATE + 1))

  return(list(
    ozono_processed_dataset = ozono_processed_dataset,
    processed_dataset = selected_measurements
  ))
}

take_last_n_values <- function(values, items = 1) {
  values_count <- nrow(measurements_dataset)

  values <- as.matrix(values)[
    (values_count - items):values_count
  ]

  return(values)
}


calculate_measurements_data <- function(data_column, compound_code, limits_dataset) {
  data_column <- as.numeric(as.character(data_column))

  if (is.element(compound_code, "2C")) {
    data_column <- legacy.pmn(data_column, 8) # SJU
  } else if (is.element(compound_code, "4S")) {
    data_column <- legacy.pmn(data_column, 24)
  } else if (is.element(compound_code, c("5P", "6P"))) {
    data_column <- legacy.ppn(data_column, legacy.pppn(data_column, limits_dataset))
  }
  return(data_column)
}