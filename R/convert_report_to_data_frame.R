#' Convert a report to a data frame
#' @param report Report either daily or hourly to format
#' @export
convert_report_to_data_frame <- function(report) {
  report_frame <- data.frame()

  for (i in seq_along(report$Results)) {
    index_results <- report$Results[[i]]
    row_data <- get_frame_row_header(i, report)

    for (j in seq_along(index_results)) {
      row_data <- c(row_data, get_frame_cell_data(index_results, i, j, report))
    }
    report_frame <- rbind(report_frame, row_data)
  }
  colnames(report_frame) <- get_frame_column_headers(report$Locations)
  return(report_frame)
}

get_frame_cell_data <- function(index_results, result_index, location_index, report) {
  location_result <- index_results[[location_index]]
  category <- report$Categories[[as.character(location_result$CategoryId)]]
  if (result_index == 1 && location_index == 1) {
    cell_result <- paste(
      format(report$DateTime, "%d/%m/%Y %H:%M"),
      ifelse(is.na(location_result$Index), "", location_result$Index),
      category$ColorHex,
      sep = "@"
    )
  } else {
    cell_result <- paste(
      ifelse(is.na(location_result$Index), "", location_result$Index),
      category$ColorHex,
      sep = "@"
    )
  }
  return(cell_result)
}

get_frame_row_header <- function(results_index, report) {
  index_info <- report$Indexes[[names(report$Results[results_index])]]
  parameter_info <- report$Parameters[[index_info$ParameterCode]]
  return(paste(parameter_info$HtmlDisplay, index_info$Name, parameter_info$WebsiteUrl, sep = "@"))
}

get_frame_column_headers <- function(locations) {
  column_names <- ""

  for (i in seq_along(locations)) {
    location <- locations[[i]]
    column_name <- paste(location$Name, location$Code, "http://aire.cemcaq.mx/informacion/", sep = "@")
    column_names <- c(column_names, column_name)
  }

  return(column_names)
}