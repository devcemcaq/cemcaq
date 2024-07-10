#' Extraer las mediciones en crudo o prevalidadas almacenadas en la base de datos.
#'
#' @param connection Conexion a la base de datos
#' @param start_time Fecha y hora de inicio
#' @param end_time Fecha y hora de fin
#' @param prevalidated Indica si los datos son prevalidados o son crudos.
#'
#' @export
#'
#' @return Devuelve un dataframe con los datos extraidos.
#'
extract_measurements_data <- function(connection, start_time, end_time, prevalidated = TRUE) {
  table_name <- ifelse(prevalidated, "cemcaq.data_stations_validate_view", "cemcaq.data_stations_view")

  query <- paste0("SELECT * FROM ", table_name,
                  " WHERE Date_Time >= '", start_time, "' AND Date_Time <= '", end_time,
                  "' ORDER BY Date_Time ASC")
  result <- odbc::dbGetQuery(connection, query)
  return(result)
}
