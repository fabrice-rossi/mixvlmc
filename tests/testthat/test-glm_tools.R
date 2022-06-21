test_that("the model matrix is well formed", {
  cov_demo <- matrix(1:100, ncol = 2, byrow = FALSE)
  ctx_pos <- seq(5, 20, by = 4)
  ctx_length <- 5
  mm <- prepare_covariate(cov_demo, ctx_pos, d = ctx_length, with_intercept = TRUE)
  expect_equal(dim(mm), c(length(ctx_pos), 1 + ncol(cov_demo) * ctx_length))
  expect_equal(mm[, 1], rep(1, length(ctx_pos)))
  for (ipos in seq_along(ctx_pos)) {
    the_col <- ncol(mm)
    for (f in 1:ctx_length) {
      for (d in ncol(cov_demo):1) {
        expect_equal(mm[ipos, the_col], cov_demo[ctx_pos[ipos] + f, d])
        the_col <- the_col - 1
      }
    }
  }
})

test_that("the model data frame is well formed", {
  cov_demo <- data.frame(x = 1:50, z = sample(c("x", "y", "z"), 50, replace = TRUE))
  ctx_pos <- seq(5, 20, by = 4)
  ctx_length <- 5
  mm <- prepare_covariate(cov_demo, ctx_pos, d = ctx_length)
  expect_equal(dim(mm), c(length(ctx_pos), ncol(cov_demo) * ctx_length))
  expect_equal(names(mm), paste(names(cov_demo), rep(1:ctx_length, each = ncol(cov_demo)), sep = "_"))
  for (ipos in seq_along(ctx_pos)) {
    the_col <- ncol(mm)
    for (f in 1:ctx_length) {
      for (d in ncol(cov_demo):1) {
        expect_identical(mm[ipos, the_col], cov_demo[ctx_pos[ipos] + f, d])
        the_col <- the_col - 1
      }
    }
  }
})
