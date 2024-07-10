#' Extraer los datos historicos de los indices de calidad del aire horarios almacenados en una base de datos.
#'
#' @param connection Conexion a la base de datos
#' @param start_time Fecha y hora de inicio
#' @param end_time Fecha y hora de fin
#' @param filter Indica si se aplica el filtro interno que oculta los modulos desactivados internamente.
#' @param sufix_column_names Le da un sufijo a las columnas de los datos.
#'
#' @export
#'
#' @return Devuelve un dataframe con los datos extraidos.
#'
extract_air_quality_index_data <- function(connection, start_time, end_time, filter = TRUE, sufix_column_names = "") {
  vista <- ifelse(filter, "dbcemcaqgob.mapa_view", "dbcemcaqgob.mapa_sinfiltro_view")
  locations <- c("CAP", "COR", "EPG", "FEO", "JOV", "SJU")
  indexes <- list(
    list(Code = "NO2", Column = "ino2"),
    list(Code = "CO", Column = "ico"),
    list(Code = "O3", Column = "io31h"),
    list(Code = "SO2", Column = "iso2"),
    list(Code = "PM25", Column = "ipm25"),
    list(Code = "PM10", Column = "ipm10")
  )

  parameters <- "Date_Time"
  for (i in seq_len(length(indexes))) {
    for (j in seq_len(length(locations))) {
      parameters <- paste0(
        parameters, ",",
        indexes[[i]]$Column, "_",
        stringr::str_to_lower(locations[j]), " as ",
        locations[j], "_",
        indexes[[i]]$Code,
        sufix_column_names)
    }
  }

  query <- paste0("SELECT ", parameters, " FROM ", vista,
                  " WHERE Date_Time >= '", start_time, "' AND Date_Time <= '", end_time,
                  "' ORDER BY Date_Time ASC")
  result <- odbc::dbGetQuery(connection, query)
  return(result)
}