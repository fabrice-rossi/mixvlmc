test_that("before(x) is before x", {
  withr::local_seed(0)
  vals <- rnorm(1000, sd = 1000)
  b_vals <- before(vals)
  expect_true(all(b_vals < vals))
})
