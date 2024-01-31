describe("get_air_quality_index_category_id()", {
  intervals <- data.frame(
    Id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    CategoryId = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5),
    FloorValue = c(0, 45, 70, 132, 213, 0, 15, 41, 79, 130),
    IndexCode = c("PM10", "PM10", "PM10", "PM10", "PM10", "PM2.5", "PM2.5", "PM2.5", "PM2.5", "PM2.5")
  )
  it("Obtiene la categoría 1 (calidad buena) para PM10", {
    categoryId <- get_air_quality_index_category_id(10, "PM10", intervals)
    expect_equal(categoryId, 1)
  })

  it("Obtiene la categoría 2 (calidad aceptable) para PM10", {
    categoryId <- get_air_quality_index_category_id(50, "PM10", intervals)
    expect_equal(categoryId, 2)
  })

  it("Obtiene la categoría 3 (calidad mala) para PM10", {
    categoryId <- get_air_quality_index_category_id(50, "PM10", intervals)
    expect_equal(categoryId, 2)
  })

  it("Obtiene la categoría 1 (calidad buena) para PM2.5", {
    categoryId <- get_air_quality_index_category_id(5, "PM2.5", intervals)
    expect_equal(categoryId, 1)
  })

  it("Devuelve NA si no hay intervalos definidos para el indice", {
    expect_true(is.na(
      get_air_quality_index_category_id(0, "PM20", intervals)
    ))
  })

  it("Devuelve NA si el indice es NULL o NA", {
    expect_true(is.na(
      get_air_quality_index_category_id(NA, "PM10", intervals)
    ))
  })
})

