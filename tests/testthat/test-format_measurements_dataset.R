describe("get_init_date_time", {
  it("Devuelve correctamente la hora de hace 12 horas", {
    current_date_time <- as.POSIXct("2020-02-02 00:00:00")

    init_date_time <- get_init_date_time(current_date_time, max_hours = 12)
    expect_equal(
      init_date_time,
      as.POSIXct("2020-02-01 12:00:00")
    )
  })

  it("Devuelve correctamente la hora de hace 24 horas", {
    current_date_time <- as.POSIXct("2020-02-02 00:00:00")

    init_date_time <- get_init_date_time(current_date_time, max_hours = 24)
    expect_equal(
      init_date_time,
      as.POSIXct("2020-02-01 00:00:00")
    )
  })

  it("Genera error si max_hours es menor a 1", {
    current_date_time <- as.POSIXct("2020-02-02 00:00:00")

    expect_error(
      get_init_date_time(current_date_time, max_hours = 0),
      "max_hours must be greater than 0"
    )
  })
})

describe("sort_measurements_dataset_by_date_time", {
  it("Devuelve las mediciones en order cronologico de forma descendente", {
    measurements <- data.frame(
      Date_Time = c(as.POSIXct("2020-01-01 03:00:00"), as.POSIXct("2020-01-01 01:00:00")),
      X = c(1, 3),
      Y = c(2, 4)
    )

    measurements_data <- sort_measurements_dataset_by_date_time(measurements, "Date_Time")

    expect_equal(
      measurements_data$Date_Time,
      c(as.POSIXct("2020-01-01 01:00:00"), as.POSIXct("2020-01-01 03:00:00"))
    )
  })
})

describe("convert_datetime_column_to_date_type", {
  it("Devuelve correctamente la columna de fecha en tipo date time", {
    measurements <- data.frame(
      Date_Time = c("2020-01-01 00:00:00", "2020-01-01 03:00:00"),
      X = c(1, 3),
      Y = c(2, 4)
    )

    measurements_data <- convert_datetime_column_to_date_type(
      measurements,
      "Date_Time"
    )

    expect_s3_class(measurements_data$Date_Time, "POSIXct")
  })
})


describe("filter_by_date_time_interval", {
  it("Devuelve solo los registros dentro de un rango de fecha dados", {
    init_date_time <- as.POSIXct("2020-01-01 00:00:00")
    end_date_time <- as.POSIXct("2020-01-01 03:00:00")
    measurements <- data.frame(
      Date_Time = c(as.POSIXct("2020-01-01 00:00:00"), as.POSIXct("2020-01-01 01:00:00"), as.POSIXct("2020-01-01 03:00:00")),
      X = c(1, 3, 5),
      Y = c(2, 4, 6)
    )

    measurements_data <- filter_by_date_time_interval(
      measurements,
      init_date_time,
      end_date_time,
      "Date_Time"
    )

    expect_length(
      measurements_data$Date_Time,
      2
    )

    expect_contains(
      measurements_data$Date_Time,
      c(as.POSIXct("2020-01-01 01:00:00"), as.POSIXct("2020-01-01 03:00:00"))
    )
  })
})

describe("fill_missing_datetime_records", {
  it("Rellena el dataframe con 2 horas sin registrar", {
    init_datetime <- as.POSIXct("2020-01-01 00:00:00")
    end_datetime <- as.POSIXct("2020-01-01 03:00:00")

    measurements <- data.frame(
      Date_Time = end_datetime,
      X = 3,
      Y = 4
    )

    filled <- fill_missing_datetime_records(measurements, init_datetime, end_datetime)
    expect_equal(nrow(filled), 3)
  })

  it("Devuelve el dataframe original sino faltan horas", {
    init_datetime <- as.POSIXct("2020-01-01 00:00:00")
    end_datetime <- as.POSIXct("2020-01-01 01:00:00")

    measurements <- data.frame(
      Date_Time = c(init_datetime, end_datetime),
      X = c(1, 3)
    )

    filled <- fill_missing_datetime_records(measurements, init_datetime, end_datetime)
    expect_length(
      filled$Date_Time,
      length(filled$Date_Time)
    )
  })

  it("Puede el init_datetime y end_datetime ser iguales", {
    init_datetime <- as.POSIXct("2020-01-01 00:00:00")
    end_datetime <- as.POSIXct("2020-01-01 00:00:00")

    measurements <- data.frame(
      Date_Time = c(init_datetime, end_datetime),
      X = c(1, 3)
    )
    expect_no_error(fill_missing_datetime_records(measurements, init_datetime, init_datetime))
  })
})

describe("format_measurements_dataset", {
  it("Se puede dar formato a un dataset de mediciones", {
    measurements_data <- read.csv(system.file("extdata", "DatosCompletos.csv", package = "CEMCAQ"))
    current_date_time <- as.POSIXct("2024-02-22 10:00:00")
    
    expect_no_error(
      format_measurements_dataset(measurements_data, current_date_time, 12, "Date_Time")
    )
  })
})