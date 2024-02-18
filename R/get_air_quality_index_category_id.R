get_air_quality_index_category_id <- function(index_value, index_code, intervals) {
  intervals <- intervals[intervals$IndexCode == index_code,]

  if (nrow(intervals) < 1) {
    return(0)
  }

  intervals <- intervals[order(intervals$FloorValue, decreasing = FALSE),]
  intervals <- intervals[intervals$FloorValue < index_value,]

  if (nrow(intervals) > 0) {
    return(utils::tail(intervals, n = 1)[["CategoryId"]])
  }

  return(0)
}