test_that("dts conversion works", {
  x <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
  x_dts <- dts(x)
  expect_true(is_dts(x_dts))
  expect_equal(length(x_dts), length(x))
  expect_identical(x, dts_data(x_dts))

  x <- sample(-2:4, 50, replace = TRUE)
  x_dts <- dts(x)
  expect_true(is_dts(x_dts))
  expect_equal(length(x_dts), length(x))
  expect_identical(x, dts_data(x_dts))

  x <- sample(letters, 500, replace = TRUE)
  x_dts <- dts(x)
  expect_true(is_dts(x_dts))
  expect_equal(length(x_dts), length(x))
  expect_identical(x, dts_data(x_dts))

  x <- sample(c(TRUE, FALSE), 500, replace = TRUE)
  x_dts <- dts(x)
  expect_true(is_dts(x_dts))
  expect_equal(length(x_dts), length(x))
  expect_identical(x, dts_data(x_dts))

  x <- sample(as.factor(c("U", "V", "Z")), 500, replace = TRUE)
  x_dts <- dts(x)
  expect_true(is_dts(x_dts))
  expect_equal(length(x_dts), length(x))
  expect_identical(x, dts_data(x_dts))
})

test_that("dts printing works", {
  withr::local_seed(0)
  x <- sample(letters, 500, replace = TRUE)
  x_dts <- dts(x)
  expect_snapshot(print(x_dts))
  expect_snapshot(print(x_dts, n = 200))
})
