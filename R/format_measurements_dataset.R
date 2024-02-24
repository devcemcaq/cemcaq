format_measurements_dataset <- function(measurements_data, current_date_time, max_hours, date_time_column_name = "Date_Time") {
  init_date_time <- get_init_date_time(current_date_time, max_hours)

  measurements_data <- convert_datetime_column_to_date_type(measurements_data, date_time_column_name)

  measurements_data <- filter_by_date_time_interval(measurements_data, init_date_time, current_date_time, date_time_column_name)

  measurements_data <- fill_missing_datetime_records(measurements_data, init_date_time, current_date_time, date_time_column_name)

  measurements_data <- sort_measurements_dataset_by_date_time(measurements_data, date_time_column_name)

  return(measurements_data)
}

convert_datetime_column_to_date_type <- function(measurements_data, date_time_column_name = "Date_Time") {
  measurements_data[[date_time_column_name]] <- as.POSIXct(measurements_data[[date_time_column_name]])
  return(measurements_data)
}

sort_measurements_dataset_by_date_time <- function(measurements_data, date_time_column_name = "Date_Time") {
  measurements_data <- measurements_data[order(measurements_data[[date_time_column_name]]),]
  return(measurements_data)
}

get_init_date_time <- function(current_date_time, max_hours) {
  if (max_hours < 1) {
    stop("max_hours must be greater than 0")
  }
  return(current_date_time - as.difftime(max_hours, units = "hours"))
}

filter_by_date_time_interval <- function(measurements_data, init_date_time, end_date_time, date_time_column_name = "Date_Time") {
  return(
    measurements_data[
      which(
        measurements_data[[date_time_column_name]] > init_date_time & measurements_data[[date_time_column_name]] <= end_date_time),
    ]
  )
}

fill_missing_datetime_records <- function(measurements, init_datetime, end_datetime, datetime_column_name = "Date_Time") {
  desired_datetimes <- data.frame(seq(init_datetime + as.difftime(1, units = "hours"), end_datetime, "hours"))
  colnames(desired_datetimes) <- datetime_column_name
  measurements <- merge(desired_datetimes, measurements, all = TRUE)
  return(measurements)
}
