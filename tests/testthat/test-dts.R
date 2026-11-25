test_that("dts conversion works", {
  x <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
  x_dts <- dts(x)
  expect_true(is_dts(x_dts))
  expect_equal(length(x_dts), length(x))
  expect_identical(x, dts_data(x_dts))
  expect_identical(c(0, 1), states(x_dts))

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
  expect_identical(c(FALSE, TRUE), states(x_dts))

  x <- sample(as.factor(c("U", "V", "Z")), 500, replace = TRUE)
  x_dts <- dts(x)
  expect_true(is_dts(x_dts))
  expect_equal(length(x_dts), length(x))
  expect_identical(x, dts_data(x_dts))
  expect_identical(as.factor(c("U", "V", "Z")), states(x_dts))
})

test_that("errors are detected", {
  expect_error(dts(list()))
  expect_error(dts("W", vals = c("U", "V")))
})

test_that("dts printing works", {
  withr::local_seed(0)
  x <- sample(letters, 500, replace = TRUE)
  x_dts <- dts(x)
  expect_snapshot(print(x_dts))
  expect_snapshot(print(x_dts, n = 200))
  expect_snapshot(print(x_dts[1:5]))
})

test_that("reversing works", {
  x <- sample(as.factor(c("U", "V", "Z")), 200, replace = TRUE)
  x_dts <- dts(x)
  rev_x <- rev(x_dts)
  rev_x_direct <- dts(rev(x))
  expect_identical(rev_x, rev_x_direct)
})

test_that("indexing works", {
  x <- sample(as.factor(c("U", "V", "Z")), 200, replace = TRUE)
  x_dts <- dts(x)
  start <- sample(1:50, 20)
  end <- start + sample(1:50, length(start))
  for (k in seq_along(start)) {
    expect_equal(dts_data(x_dts[start[k]:end[k]]), x[start[k]:end[k]])
  }
  all_equal <- TRUE
  for (k in seq_along(x)) {
    if (x[k] != x_dts[[k]]) {
      all_equal <- FALSE
      break
    }
  }
  expect_true(all_equal)
})

test_that("dts rejects missing values and empty time series", {
  expect_error(dts(c(1, 2, NA, 1)), "missing values")
  expect_error(dts(factor(c("a", NA, "b"))), "missing values")
  expect_error(dts(c(1, 2, NA, 1), vals = c(1, 2)), "missing values")
  expect_error(dts(integer(0)), "empty")
  expect_error(dts(factor(character(0), levels = c("a", "b"))), "empty")
  for (backend in c("R", "C++")) {
    expect_error(vlmc(dts(c(1, 2, 1, 1, 2)), max_depth = 0, backend = backend) |>
      loglikelihood(newdata = c(1, NA, 2)), "missing values")
  }
})
