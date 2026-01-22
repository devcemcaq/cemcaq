get_air_quality_index_category_id <- function(index_value, index_code, intervals) {
  intervals <- intervals[intervals$IndexCode == index_code,]
  if (nrow(intervals) < 1 || is.na(index_value)) {
    return(0)
  }

  if (index_value < 0) {
    return(0)
  }

  if (index_value == 0) {
    return(1)
  }

  intervals <- intervals[order(intervals$FloorValue, decreasing = FALSE),]
  intervals <- intervals[intervals$FloorValue < index_value,]

  if (nrow(intervals) > 0) {
    return(utils::tail(intervals, n = 1)[["CategoryId"]])
  }

  return(0)
}
