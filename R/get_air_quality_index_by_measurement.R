get_air_quality_index_by_measurement <- function(measurements, index_options, intervals, categories, parameters) {
  parameter_options <- find_row_by_row_name(parameters, index_options$ParameterCode, "Code")
  index <- calculate_measurements_index(
    measurements,
    hours = index_options$Hours,
    weighted = index_options$Weighted,
    relevant_gap = index_options$RelevantGap,
    min_relevant_gap_records = index_options$MinRelevantGapRecords,
    decimal_digits = parameter_options$DecimalDigits,
    result_factor = index_options$ResultFactor
  )

  category_id <- get_air_quality_index_category_id(index, parameter_options$Code, intervals)
  category <- find_row_by_row_name(categories, category_id, "Id")

  return(list(
    "index" = index,
    "category" = category
  ))
}
