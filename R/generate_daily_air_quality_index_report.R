#' Genera un reporte horario con los indices de calidad del aire
#' correspondientes a la hora y fecha indicada.
#'
#' @param date_time Indica un valor de tipo hora y fecha que indica un momento concreto para generar el reporte de
#' calidad del aire.
#' @param measurements_data Es un dataframe con los datos de las estaciones con los distintos parametros que son usados
#' para calcular los indices de calidad del aire.
#' @param control Es un dataframe que indica las estaciones e indices que se van a calcular, así como notar cuáles
#' modulos estan en mantenimiento o desactivados.
#' @param limits Dataframe que indica los valores minimos y maximos de los contaminantes que se usaran para los calculos.
#' @param intervals Dataframe que indica los rangos de los indices de calidad del aire y a qué nivel de riesgo o
#' categoria de calidad del aire corresponden.
#' @param locations Tabla que registra las ubicaciones o estaciones en donde se extraen las mediciones.
#' @param parameters Indica los parametros o contaminantes e informacion sobre su comportamiento para los calculos.
#' @param categories Registro de las categorias o niveles de riesgo de calidad del aire.
#' @param indexes Indica cómo o qué consideran los indices para su calculo, asi como a que parametro corresponden y
#' que ajustes de harán para calcular el indice de calidad del aire.
#'
#' @return Devuelve una lista con la hora y fecha del reporte, las ubicaciones consideradas, los parametros o contaminantes,
#' las categorias de calidad del aire, los indices y que consideran para el calculo, y los resultados, que contienen
#' todos los resultados de cada indice calculado.
#'
#' @export
generate_daily_air_quality_index_report <- function(date_time, measurements_data, control, limits,
                                                    intervals, locations, parameters, categories, indexes) {
  location_codes <- colnames(control)[-1]
  index_codes <- control$IndexCode

  measurements_data <- format_measurements_dataset(
    measurements_data,
    date_time,
    max(indexes$Hours),
    "Date_Time"
  )

  report <- get_daily_report(control, indexes, measurements_data, intervals, categories, parameters, limits)

  return(list(
    DateTime = format(date_time, "%Y-%m-%d %H:00:00"),
    Locations = dataframe_to_list_with_key(
      get_locations_filtered_and_ordered_by_control(locations, location_codes),
      "Code"
    ),
    Indexes = dataframe_to_list_with_key(
      get_indexes_filtered_and_ordered_by_control(indexes, index_codes),
      "Code"
    ),
    Parameters = dataframe_to_list_with_key(parameters, "Code"),
    Categories = dataframe_to_list_with_key(categories, "Id"),
    Results = report
  ))
}

get_daily_report <- function(control, indexes, measurements_data, intervals, categories, parameters, limits) {
  report <- list()

  for (control_index in seq_len(nrow(control))) {
    control_row <- control[control_index,]
    index_code <- control_row$IndexCode
    index_options <- find_row_by(indexes, "Code", index_code)

    index_report <- list()
    for (location_index in 2:ncol(control_row)) {
      location_code <- names(control_row)[location_index]
      index_status <- control_row[[location_code]]

      index <- get_daily_index(
        index_options,
        location_code,
        index_status,
        measurements_data,
        intervals,
        categories,
        parameters,
        limits
      )
      index_report[[location_code]] <- index
    }
    report[[index_code]] <- index_report
  }

  return(report)
}

get_daily_index <- function(index_options, location_code, index_status, measurements_data, intervals,
                            categories, parameters, limits) {
  measurement_name <- paste0(
    location_code,
    "_",
    ifelse(index_options$UseIndexValue, index_options$Code, index_options$ParameterCode),
    ifelse(index_options$UseIndexValue, "I", "")
  )
  measurement_name <- stringr::str_replace_all(measurement_name, "\\.", "")

  measurements <- measurements_data[[measurement_name]]

  parameter_options <- find_row_by(parameters, "Code", index_options$ParameterCode)

  if (!index_options$UseIndexValue) {
    limit_values <- find_row_by(limits, "ParameterCode", index_options$ParameterCode)
    measurements <- as.numeric(
      clear_measurements_data(
        measurements,
        limit_values$Min,
        limit_values$Max
      )
    ) * parameter_options$Scale
  }

  index <- get_air_quality_index_by_measurement(
    measurements,
    index_options,
    parameter_options$DecimalDigits,
    intervals,
    categories,
    index_status
  )

  index$LocationCode <- location_code
  index$ParameterCode <- index_options$ParameterCode
  index$IndexCode <- index_options$Code

  return(index)
}
