#' Limpia un vector de datos aplicando banderas.
#'
#' @description Aplica una prevalidacion a un vector de dato.
#' A los valores iguales a -9999 se reemplazan por la bandera ND.
#' A los valores iguales a 0, se reemplazan por la bandera VZ.
#' A los valores menores a 0 (negativos) se aplica la bandera NN.
#' A los valores menores a min_value(si no es null), se reemplazan por la bandera IR.
#' A los valores mayores a max_value(si no es null), se reemplazan por la bandera IR.
#'
#' @param measurements Vector numerico con las mediciones a limpiar
#' @param min_value Valor numerico que indica el valor minimo permitido. NULL por defecto.
#' @param max_value Valor numerico que indica el valor maximo permitido. NULL por defecto.
#'
#' @return Vector de caracteres con los valores limpios y banderas correspondientes
#'
#' @export
#'
#' @examples
#'
#' x <- clear_measurements_data(c(1,NA,3,9), min_value=4, max_value=8)
#'
clear_measurements_data <- function(measurements, min_value = NULL, max_value = NULL) {
  measurements[suppressWarnings(as.numeric(measurements)) == -9999] <- "ND"

  if (!is.null(min_value) && min_value <= 0) {
    stop("min_value must be greater than zero")
  }

  if (!is.null(max_value) && max_value <= 0) {
    stop("max_value must be greater than zero")
  }

  if (!is.null(min_value))
  {

    measurements[suppressWarnings(as.numeric(measurements)) < min_value] <- "IR"
  }
  if (!is.null(max_value))
  {
    measurements[suppressWarnings(as.numeric(measurements)) > max_value] <- "IR"
  }
  measurements[suppressWarnings(as.numeric(measurements)) == 0] <- "VZ"
  measurements[suppressWarnings(as.numeric(measurements)) < 0] <- "NN"

  return(measurements)
}