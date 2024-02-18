describe("calculate_measurements_index()", {
  it("Obtener el indice horario de un valor escalar", {
    measurements <- runif(1, min = 1, max = 10)

    expect_equal(calculate_measurements_index(measurements, hours = 1), measurements)
  })

  it("Obtener el indice horario de un vector", {
    measurements <- runif(3, min = 1, max = 10)

    expect_equal(calculate_measurements_index(measurements, hours = 1), measurements[3])
  })

  it("Obtiene la concentracion promedio movil de 8 horas", {
    measurements <- runif(8, min = 1, max = 10)

    expect_equal(calculate_measurements_index(measurements, hours = 8), mean(measurements))
  })

  it("Obtiene la concentracion promedio movil de 8 horas con 75% de los datos", {
    measurements <- runif(6, min = 1, max = 10)

    expect_equal(calculate_measurements_index(measurements, hours = 8), mean(measurements))
  })

  it("Obteniendo la concentracion promedio movil de 8 horas con menos 75% de los datos devuelve NA", {
    measurements <- runif(5, min = 1, max = 10)

    expect_equal(calculate_measurements_index(measurements, hours = 8), NA)
  })

  it("Arroja un error cuando se establece el valor de hours menor a 0", {
    measurements <- runif(1, min = 1, max = 10)

    expect_error(calculate_measurements_index(measurements, hours = 0))
  })

  it("Calcular promedio movil ponderado 12 horas", {
    measurements <- c(50, 80, 75, 90, 82, 53, 64, 74, 21, 10, 16, 13)

    index <- calculate_measurements_index(measurements, hours = 12, weighted = TRUE)
    expect_equal(round(index), 17)
  })

  it("Calcular promedio movil ponderado 12 horas (2)", {
    measurements <- c(118, 97, 130, 142, 146, 144, 141, 139, 147, 150, 141, 103)

    index <- calculate_measurements_index(measurements, hours = 12, weighted = TRUE)
    expect_equal(round(index), 129)
  })

  it("Calcular promedio movil ponderado 12 horas con 2 de las 3 horas mas recientes", {
    measurements <- c(50, 80, 75, 90, 82, 53, 64, 74, 21, 10, NA, 13)
    expect_false(is.na(calculate_measurements_index(measurements, hours = 12, weighted = TRUE)))


    measurements <- c(50, 80, 75, 90, 82, 53, 64, 74, 21, NA, 16, 13)
    expect_false(is.na(calculate_measurements_index(measurements, hours = 12, weighted = TRUE)))

    measurements <- c(50, 80, 75, 90, 82, 53, 64, 74, 21, 10, 16, NA)
    expect_false(is.na(calculate_measurements_index(measurements, hours = 12, weighted = TRUE)))
  })

  it("Calcular promedio movil ponderado 12 horas con menos de 2 de las 3 horas mas recientes devuelve NA", {
    measurements <- c(50, 80, 75, 90, 82, 53, 64, 74, 21, NA, NA, 13)
    expect_true(is.na(calculate_measurements_index(measurements, hours = 12, weighted = TRUE)))

    measurements <- c(50, 80, 75, 90, 82, 53, 64, 74, 21, 10, NA, NA)
    expect_true(is.na(calculate_measurements_index(measurements, hours = 12, weighted = TRUE)))

    measurements <- c(50, 80, 75, 90, 82, 53, 64, 74, 21, NA, 16, NA)
    expect_true(is.na(calculate_measurements_index(measurements, hours = 12, weighted = TRUE)))

    measurements <- c(50, 80, 75, 90, 82, 53, 64, 74, 21, NA, NA, NA)
    expect_true(is.na(calculate_measurements_index(measurements, hours = 12, weighted = TRUE)))
  })

  it("Error si hours es distinto a el tamaño de measurements para el promedio movil ponderado", {
    measurements <- runif(4, min = 1, max = 10)

    expect_error(calculate_measurements_index(measurements, hours = 12, weighted = TRUE))
  })

  it("Error si relevant_gap no es un valor aceptado para el promedio movil ponderado", {
    measurements <- runif(12, min = 1, max = 10)
    expect_error(
      calculate_measurements_index(measurements, hours = 12, weighted = TRUE, relevant_gap = 0, min_relevant_gap_records = 0)
    )
    expect_error(
      calculate_measurements_index(measurements, hours = 12, weighted = TRUE, relevant_gap = 14)
    )
  })

  it("Error si min_relevant_gap no es un valor aceptado para el promedio movil ponderado", {
    measurements <- runif(12, min = 1, max = 10)
    expect_error(
      calculate_measurements_index(measurements, hours = 12, weighted = TRUE, relevant_gap = 3, min_relevant_gap_records = 0)
    )
    expect_error(
      calculate_measurements_index(measurements, hours = 12, weighted = TRUE, relevant_gap = 3, min_relevant_gap_records = 4)
    )
  })
})

describe("get_moving_concentration_weight()", {
  it("Obtener el factor de ponderacion de 0.5", {
    measurements <- c(50, 80, 75, 90, 82, 53, 64, 74, 21, 10, 16, 13)

    weight <- get_moving_concentration_weight(measurements)
    expect_equal(weight, 0.5)
  })

  it("Obtener el factor de ponderacion de 0.5 con un valor faltante", {
    measurements <- c(50, 80, 75, 90, 82, NA, 64, 74, 21, 10, 16, 13)

    weight <- get_moving_concentration_weight(measurements)
    expect_equal(weight, 0.5)
  })

  it("Obtener el factor de ponderacion mayor a 0.5", {
    measurements <- c(118, 97, 130, 142, 146, 144, 141, 139, 147, 150, 141, 103)

    weight <- get_moving_concentration_weight(measurements)
    expect_equal(weight, 0.65)
  })
})