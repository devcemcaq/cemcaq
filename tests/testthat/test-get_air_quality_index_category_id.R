describe("get_air_quality_index_category_id() para PM10", {
  intervals <- data.frame(
    Id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    CategoryId = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5),
    FloorValue = c(0, 45, 50, 132, 213, 0, 15, 25, 79, 130),
    IndexCode = c("PM10", "PM10", "PM10", "PM10", "PM10", "PM2.5", "PM2.5", "PM2.5", "PM2.5", "PM2.5")
  )
  it("Obtiene la categoría 1 (calidad buena) para PM10", {
    categoryId <- get_air_quality_index_category_id(45, "PM10", intervals)
    expect_equal(categoryId, 1)
  })

  it("Obtiene la categoría 2 (calidad aceptable) para PM10", {
    categoryId <- get_air_quality_index_category_id(46, "PM10", intervals)
    expect_equal(categoryId, 2)

    categoryId <- get_air_quality_index_category_id(50, "PM10", intervals)
    expect_equal(categoryId, 2)
  })

  it("Obtiene la categoría 3 (calidad mala) para PM10", {
    categoryId <- get_air_quality_index_category_id(51, "PM10", intervals)
    expect_equal(categoryId, 3)

    categoryId <- get_air_quality_index_category_id(132, "PM10", intervals)
    expect_equal(categoryId, 3)
  })

  it("Obtiene la categoría 4 (calidad muy mala) para PM10", {
    categoryId <- get_air_quality_index_category_id(133, "PM10", intervals)
    expect_equal(categoryId, 4)

    categoryId <- get_air_quality_index_category_id(213, "PM10", intervals)
    expect_equal(categoryId, 4)
  })

  it("Obtiene la categoría 5 (calidad extremadamente mala) para PM10", {
    categoryId <- get_air_quality_index_category_id(213.1, "PM10", intervals)
    expect_equal(categoryId, 5)
  })

  it("Obtiene la categoría 1 (calidad buena) para PM2.5", {
    categoryId <- get_air_quality_index_category_id(15, "PM2.5", intervals)
    expect_equal(categoryId, 1)
  })

  it("Devuelve 0 si el indice es NULL o NA", {
    expect_equal(
      get_air_quality_index_category_id(NA, "PM10", intervals),
      0
    )
  })

  it("Devuelve 0 si no se encuentra el parametro", {
    expect_equal(
      get_air_quality_index_category_id(10, "RANDOM", intervals),
      0
    )
  })

  it("Obtiene la categoría 1 (calidad buena) cuando el índice es 0", {
    categoryId <- get_air_quality_index_category_id(0, "PM10", intervals)
    expect_equal(categoryId, 1)
  })

  it("Devuelve 0 cuando el índice es negativo", {
    categoryId <- get_air_quality_index_category_id(-1, "PM10", intervals)
    expect_equal(categoryId, 0)
  })
})


