test_that("get average hourly concentration from scalar value", {
  measurement <- runif(1, min = 1, max = 10)

  expect_equal(calculate_measurements_index(measurement, hours = 1), measurement)
})

test_that("get average hourly concentration from vector value", {
  measurements <- runif(3, min = 1, max = 10)

  expect_equal(calculate_measurements_index(measurements, hours = 1), measurements[1])
})

test_that("get 8-hour moving average concentration from vector value", {
  measurements <- runif(8, min = 1, max = 10)

  expect_equal(calculate_measurements_index(measurements, hours = 8), mean(measurements))
})

test_that("gets an and error when sending an hours value less than 1", {
  measurement <- runif(1, min = 1, max = 10)

  expect_error(calculate_measurements_index(measurement, hours = 0))
})

test_that("calculate pm10 index", {
  measurement <- c(50, 80, 75, 90, 82, 53, 64, 74, 21, 10, 16, 13)

  index <- calculate_measurements_index(measurement, hours = 12, weighted = TRUE)
  expect_equal(round(index), 17)

})

test_that("calculate pm10 index 2", {
  measurement <- c(118, 97, 130, 142, 146, 144, 141, 139, 147, 150, 141, 103)

  index <- calculate_measurements_index(measurement, hours = 12, weighted = TRUE)
  expect_equal(round(index), 129)

})

test_that("calculate pm10 index with a missing value", {
  measurement <- c(50, 80, 75, 90, 82, 53, 64, 74, 21, 10, NA, 13)

  index <- calculate_measurements_index(measurement, hours = 12, weighted = TRUE)
  expect_equal(round(index), 18)

})

test_that("gets the weight of the measurement", {
  measurements <- c(50, 80, 75, 90, 82, 53, 64, 74, 21, 10, 16, 13)

  weight <- obtener_pesos(measurements)
  expect_equal(weight, 0.5)
})

test_that("gets the weight of the measurement with an NA value", {
  measurements <- c(50, 80, 75, 90, 82, NA, 64, 74, 21, 10, 16, 13)

  weight <- obtener_pesos(measurements)
  expect_equal(weight, 0.5)
})

test_that("gets the weight of the measurement 2", {
  measurements <- c(118, 97, 130, 142, 146, 144, 141, 139, 147, 150, 141, 103)

  weight <- obtener_pesos(measurements)
  expect_equal(weight, 0.65)
})
