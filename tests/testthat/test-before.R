test_that("before(x) is before x", {
  withr::local_seed(0)
  vals <- rnorm(1000, sd = 1000)
  b_vals <- before(vals)
  expect_true(all(b_vals < vals))
})

test_that("after(x) is after x", {
  withr::local_seed(0)
  vals <- rnorm(1000, sd = 1000)
  a_vals <- after(vals)
  expect_true(all(a_vals > vals))
})

test_that("before(after(x))==x", {
  withr::local_seed(0)
  vals <- rnorm(1000, sd = 1000)
  b_vals <- before(vals)
  ab_vals <- after(b_vals)
  expect_true(all(ab_vals == vals))
  a_vals <- after(vals)
  ba_vals <- before(a_vals)
  expect_true(all(ba_vals == vals))
})
