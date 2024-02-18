utils.is_data_empty <- function(value) {
  if (is.na(value) || is.null(value)) {
    return(TRUE)
  }

  if (nrow(value) == 0 || ncol(value) == 0) {
    return(TRUE)
  }

  return(FALSE)
}

utils.get_current_date_time <- function() {
  return(Sys.time())
}

utils.extract_a_number_from_date_time <- function(date_time, segment) {
  return(as.numeric(format(date_time, paste0("%", segment))))
}

utils.find_index_by <- function(data, columnName, value) {
  return(which(data[[columnName]] == value)[1])
}

find_row_by_row_name <- function(data, row_name, column_name_as_key = NULL) {
  if (!is.null(column_name_as_key)) {
    row.names(data) <- data[[column_name_as_key]]
  }

  return(data[row_name,])
}