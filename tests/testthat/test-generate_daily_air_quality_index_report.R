describe("generate_daily_air_quality_index_report()", {
  it("Genera un reporte", {
    measurements_data <- read.csv(system.file("extdata", "DatosCompletos.csv", package = "CEMCAQ"))
    control <- read.csv(system.file("extdata", "control.csv", package = "CEMCAQ"))
    limits <- read.csv(system.file("extdata", "limits.csv", package = "CEMCAQ"))
    intervals <- read.csv(system.file("extdata", "intervals.csv", package = "CEMCAQ"))
    locations <- read.csv(system.file("extdata", "locations.csv", package = "CEMCAQ"))
    parameters <- read.csv(system.file("extdata", "parameters.csv", package = "CEMCAQ"))
    categories <- read.csv(system.file("extdata", "categories.csv", package = "CEMCAQ"))
    indexes <- read.csv(system.file("extdata", "indexes_daily.csv", package = "CEMCAQ"))
    date_time <- as.POSIXct("2024-03-22 23:00:00")

    report <- suppressWarnings(
      generate_daily_air_quality_index_report(date_time, measurements_data, control, limits, intervals,
                                              locations, parameters, categories, indexes)
    )
    # write(jsonlite::toJSON(report, auto_unbox = TRUE, na = "null", pretty = TRUE), "test.json")
    expect_true(
      length(report$Indexes) > 0
    )
  })

  it("Genera un reporte sin aunque los datos esten fuera de rango", {
    measurements_data <- read.csv(system.file("extdata", "DatosCompletos.csv", package = "CEMCAQ"))
    control <- read.csv(system.file("extdata", "control.csv", package = "CEMCAQ"))
    limits <- read.csv(system.file("extdata", "limits.csv", package = "CEMCAQ"))
    intervals <- read.csv(system.file("extdata", "intervals.csv", package = "CEMCAQ"))
    locations <- read.csv(system.file("extdata", "locations.csv", package = "CEMCAQ"))
    parameters <- read.csv(system.file("extdata", "parameters.csv", package = "CEMCAQ"))
    categories <- read.csv(system.file("extdata", "categories.csv", package = "CEMCAQ"))
    indexes <- read.csv(system.file("extdata", "indexes_daily.csv", package = "CEMCAQ"))
    date_time <- as.POSIXct("2024-03-22 23:00:00")

    report <- suppressWarnings(generate_daily_air_quality_index_report(date_time, measurements_data, control, limits, intervals,
                                                                       locations, parameters, categories, indexes))
    expect_true(
      length(report$Indexes) > 0
    )
  })
})