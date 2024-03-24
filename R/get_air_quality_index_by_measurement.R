get_air_quality_index_by_measurement <- function(measurements, index_options, parameter_decimal_digits, intervals, categories, status) {
  result <- get_index_and_category_id(measurements, index_options, parameter_decimal_digits, intervals, status)
  category <- find_row_by(categories, "Id", result$CategoryId)

  if (nrow(category) < 1) {
    stop("There is no category for index.")
  }

  return(list(
    Index = result$Index,
    CategoryId = result$CategoryId
  ))
}

get_index_and_category_id <- function(measurements, index_options, parameter_decimal_digits, intervals, status) {
  if (status == -1) {
    return(list(
      Index = NA,
      CategoryId = -1
    ))
  }

  if (status == 1 && length(measurements) > 0) {
    index <- calculate_measurements_index(
      measurements,
      hours = index_options$Hours,
      weighted = index_options$Weighted,
      relevant_gap = index_options$RelevantGap,
      min_relevant_gap_records = index_options$MinRelevantGapRecords,
      decimal_digits = parameter_decimal_digits,
      result_factor = index_options$ResultFactor,
      use_max_value = index_options$UseMaxValue
    )

    category_id <- get_air_quality_index_category_id(index, index_options$Code, intervals)

    return(list(
      Index = index,
      CategoryId = category_id
    ))
  }

  return(list(
    Index = NA,
    CategoryId = 0
  ))
}