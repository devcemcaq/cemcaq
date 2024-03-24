find_row_by_row_name <- function(data, row_name, column_name_as_key = NULL) {
  if (!is.null(column_name_as_key)) {
    row.names(data) <- data[[column_name_as_key]]
  }

  return(data[row_name,])
}

find_row_by <- function(data, column_name, value) {
  return(data[which(data[[column_name]] == value, arr.ind = TRUE),])
}

dataframe_to_list_with_key <- function(data, key_name) {
  data_list <- list()

  for (i in seq_len(nrow(data))) {
    row <- data[i,]
    data_list[[as.character(row[[key_name]])]] <- as.list(row)
  }
  return(data_list)
}

get_locations_filtered_and_ordered_by_control <- function(locations, location_codes) {
  row.names(locations) <- locations$Code

  return(locations[location_codes,])
}

get_indexes_filtered_and_ordered_by_control <- function(indexes, index_codes) {
  row.names(indexes) <- indexes$Code

  return(indexes[index_codes,])
}
