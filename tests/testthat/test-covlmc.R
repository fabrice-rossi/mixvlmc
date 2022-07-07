test_that("covlmc is robust to degenerate cases with 2 states", {
  x <- rep(c(0, 1), 1000)
  y <- data.frame(y = rep(0, length(x)))
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    x_covlmc <- covlmc(x, y)
    expect_identical(depth(x_covlmc), 1)
    expect_identical(context_number(x_covlmc), 2)
    x_covlmc_ctx <- contexts(x_covlmc)
    expect_identical(length(x_covlmc_ctx), 2L)
    expect_identical(x_covlmc_ctx[[1]], "0")
    expect_identical(x_covlmc_ctx[[2]], "1")
  }
})

test_that("covlmc is robust to degenerate cases with more than 2 states", {
  x <- rep(c(0, 1, 2), 1000)
  y <- data.frame(y = rep(0, length(x)))
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    x_covlmc <- covlmc(x, y)
    expect_identical(depth(x_covlmc), 1)
    expect_identical(context_number(x_covlmc), 3)
    x_covlmc_ctx <- contexts(x_covlmc)
    expect_identical(length(x_covlmc_ctx), 3L)
    expect_identical(x_covlmc_ctx[[1]], "0")
    expect_identical(x_covlmc_ctx[[2]], "1")
    expect_identical(x_covlmc_ctx[[3]], "2")
  }
})
