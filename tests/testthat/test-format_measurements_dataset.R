describe("fill_missing_datetime_records", {
  it("Rellena el dataframe con 2 horas sin registrar", {
    init_datetime <- as.POSIXlt("2020-01-01 00:00:00")
    end_datetime <- as.POSIXct("2020-01-01 03:00:00")

    measurements <- data.frame(
      Date_Time = c(init_datetime, end_datetime),
      X = c(1, 3),
      Y = c(2, 4)
    )

    filled <- fill_missing_datetime_records(measurements, init_datetime, end_datetime)
    expect_equal(nrow(filled), 4)
  })

  it("Devuelve el dataframe original sino faltan horas", {
    init_datetime <- as.POSIXlt("2020-01-01 00:00:00")
    end_datetime <- as.POSIXct("2020-01-01 01:00:00")

    measurements <- data.frame(
      Date_Time = c(init_datetime, end_datetime),
      X = c(1, 3)
    )

    filled <- fill_missing_datetime_records(measurements, init_datetime, end_datetime)
    expect_equal(measurements, filled)
  })

  it("Arroja un error si el init_datetime es mayor al end_datetime", {
    init_datetime <- as.POSIXlt("2020-01-01 01:00:00")
    end_datetime <- as.POSIXct("2020-01-01 00:00:00")

    measurements <- data.frame(
      Date_Time = c(init_datetime, end_datetime),
      X = c(1, 3)
    )

    expect_error(
      fill_missing_datetime_records(measurements, init_datetime, end_datetime),
      "init_datetime must be before end_datetime"
    )
  })

  it("Puede el init_datetime y end_datetime ser iguales", {
    init_datetime <- as.POSIXlt("2020-01-01 01:00:00")
    end_datetime <- as.POSIXct("2020-01-01 00:00:00")

    measurements <- data.frame(
      Date_Time = c(init_datetime, end_datetime),
      X = c(1, 3)
    )
    expect_no_error(fill_missing_datetime_records(measurements, init_datetime, init_datetime))
  })
})