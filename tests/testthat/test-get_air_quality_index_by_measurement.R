describe("get_air_quality_index_by_measurement()", {
  parameters <- data.frame(
    Id = c(6,5),
    Code = c("PM10", "PM2.5"),
    DecimalDigits = c(0,0)
  )
  intervals <- data.frame(
    Id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    CategoryId = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5),
    FloorValue = c(0, 45, 70, 132, 213, 0, 15, 41, 79, 130),
    IndexCode = c("PM10", "PM10", "PM10", "PM10", "PM10", "PM2.5", "PM2.5", "PM2.5", "PM2.5", "PM2.5")
  )

  indexes <- data.frame(
    Id = c(1, 2),
    Code = c("PM10", "PM2.5"),
    ParameterCode = c("PM10", "PM2.5"),
    Hours = c(12, 12),
    Weighted = c(TRUE, TRUE),
    RelevantGap = c(3,3),
    MinRelevantGapRecords = c(2,2),
    ResultFactor = c(1,1)
  )

  categories <- data.frame(
    Id = c(1, 2, 3, 4),
    Quality = c("Buena", "Aceptable", "Mala", "Muy Mala"),
    RiskLevel = c("Bajo", "Moderado", "Alto", "Muy Alto"),
    Color = c("Verde", "Amarillo", "Naranja", "Rojo"),
    ColorHex = c("#00e400", "#ffff00", "#ff7e00", "#ff0000")
  )

  it("Devuelve el indice con buena calidad para PM10", {
    measurements <- c(118, 97, 130, 142, 146, 144, 141, 139, 147, 150, 141, 103)
    index <- get_air_quality_index_by_measurement(measurements, indexes[1,], intervals, categories, parameters)
    expect_true(index$index > 0)
    expect_equal(index$category[["Id"]], 3)
  })
})