test_that("round measurement works", {
  expect_equal(round_value(0.0955, 3), 0.096)
  expect_equal(round_value(0.0954, 3), 0.095)
  expect_equal(round_value(9.4, 0), 9)
  expect_equal(round_value(9.5, 0), 10)
})
