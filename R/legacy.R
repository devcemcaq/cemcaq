legacy.set_nd_flags <- function(values) {
  values <- as.matrix(values)
  values[which(as.numeric(values) == -9999)] <- "ND"
  values[which(is.na(as.numeric(values)))] <- "ND"
  return(values)
}

legacy.set_ir_flags <- function(values, compound_name, limits_dataset) {
  values <- as.matrix(values)
  limit_ranges <- limits_dataset[compound_name,]

  values[which(as.numeric(values) < as.numeric(limit_ranges$Linf))] <- "IR"
  values[which(as.numeric(values) > as.numeric(limit_ranges$Lsup))] <- "IR"
  return(values)
}

legacy.set_vz_flags <- function(values) {
  values <- as.matrix(values)
  values[which(as.numeric(values) == 0)] <- "VZ"
  return(values)
}

legacy.clear_values <- function(column_data, compound_name, limits_dataset) {
  column_data <- as.numeric(as.character(as.matrix(column_data)))


  column_data_result <- legacy.set_ir_flags(
    legacy.set_vz_flags(
      legacy.set_nd_flags(column_data)
    ), compound_name, limits_dataset
  )

  return(column_data_result)
}

legacy.ppn <- function(data_values, w) {
  # TODO: Improve readability
  values_count <- length(data_values)
  data_values_result <- rep(NA, values_count)
  values_offset <- 11

  for (value_index in seq_len(values_count - values_offset)) {
    values <- data_values[value_index:(value_index + values_offset)]
    s <- values
    z <- w[value_index + values_offset]
    valid_values <- (is.na(values) == FALSE)
    values <- values[valid_values]
    id <- seq_len(values_offset + 1)
    id <- id[valid_values]
    weights <- as.numeric(z)^(values_offset + 1 - as.numeric(id))
    suma <- as.numeric(values) %*% weights
    flag <- length(values)
    data_values_result[value_index + values_offset] <- ifelse(
      flag > 0,
      suma / sum(weights),
      NA
    )
    y1 <- s[c(10, 11, 12)]
    k <- 3 - length(y1[is.na(y1) == FALSE])
    if (k > 1) {
      data_values_result[value_index + values_offset] <- NA
    }
  }
  return(data_values_result)
}

legacy.pppn <- function(data_values, limits_dataset) {
  values_count <- length(data_values)
  data_values_result <- rep(NA, values_count)
  values_offset <- 11

  for (value_index in seq_len(values_count - values_offset)) {
    clean_values <- legacy.clear_values(
      data_values[value_index:(values_offset + value_index)],
      "PM10",
      limits_dataset
    )
    non_na_values <- clean_values[!is.na(as.numeric(clean_values))]
    if (values_count - length(non_na_values) <= values_offset) {
      min_value <- min(non_na_values)
      max_value <- max(non_na_values)
      if (!is.infinite(min_value) && !is.na(min_value)) {
        weight <- 1 - (max_value - min_value) / max_value
        data_values_result[values_offset + value_index] <- 0.5
        if (weight > 0.5) {
          data_values_result[values_offset + value_index] <- weight
        }
      }
    }
  }

  return(data_values_result)
}

legacy.pmn <- function(data_values, n) {
  values_count <- length(data_values)
  data_values_result <- rep(NA, values_count)

  for (value_index in n:values_count) {
    values <- data_values[(value_index - n + 1):value_index]
    non_na_values <- values[!is.na(values)]
    nas_count <- n - length(non_na_values)
    maximodenas <- floor(n / 4)
    if (nas_count <= maximodenas) {
      data_values_result[value_index] <- mean(as.numeric(non_na_values))
    }
  }

  return(data_values_result)
}