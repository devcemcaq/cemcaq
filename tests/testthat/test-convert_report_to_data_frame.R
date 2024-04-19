describe("convert_report_to_csv", {
  measurements_data <- read.csv(system.file("extdata", "DatosCompletos.csv", package = "CEMCAQ"))
  control <- read.csv(system.file("extdata", "control.csv", package = "CEMCAQ"))
  limits <- read.csv(system.file("extdata", "limits.csv", package = "CEMCAQ"))
  intervals <- read.csv(system.file("extdata", "intervals.csv", package = "CEMCAQ"))
  locations <- read.csv(system.file("extdata", "locations.csv", package = "CEMCAQ"))
  parameters <- read.csv(system.file("extdata", "parameters.csv", package = "CEMCAQ"))
  categories <- read.csv(system.file("extdata", "categories.csv", package = "CEMCAQ"))
  indexes <- read.csv(system.file("extdata", "indexes.csv", package = "CEMCAQ"))
  date_time <- as.POSIXct("2024-03-22 23:00:00")

  report <- suppressWarnings(generate_hourly_air_quality_index_report(
    date_time, measurements_data, control, limits, intervals, locations, parameters, categories, indexes
  ))
  it("returns a dataframe with report results", {
    csv_report <- convert_report_to_data_frame(report)
    expect_true(length(csv_report) > 0)
  })
})