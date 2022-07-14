test_that("covlmc simulation generates a consistent sample", {
  data_set <- build_data_set(500, seed = 0)
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    model <- covlmc(data_set$x, data_set$covariate, alpha = 0.2)
    xs <- simulate(model, 250, covariate = data_set$covariate[1:250, , drop = FALSE], seed = 1)
    expect_equal(length(xs), 250)
    expect_identical(levels(xs), states(model))
  }
  withr::local_seed(0)
  x <- sample(c("A", "B", "C"), 500, replace = TRUE)
  y <- ifelse(runif(length(x)) > 0.5, c(x[-1], sample(c("A", "B", "C"), 1)), c(x[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y <- as.factor(ifelse(runif(length(x)) > 0.2, y, sample(c("A", "B", "C"), 500, replace = TRUE)))
  df_y <- data.frame(y = y, z = runif(length(y)))
  new_cov <- df_y[sample(1:nrow(df_y), 250, replace = TRUE), ]
  for (engine in c("glm", "multinom")) {
    model <- covlmc(x, df_y, alpha = 1e-8)
    xs <- simulate(model, 250, covariate = new_cov, seed = 1)
    expect_equal(length(xs), 250)
    expect_identical(levels(xs), states(model))
  }
})

test_that("covlmc simulation generates always the same sample with the same seed", {
  for (k in 1:4) {
    data_set <- build_data_set(250, seed = k)
    for (engine in c("glm", "multinom")) {
      model <- covlmc(data_set$x, data_set$covariate, alpha = 0.1)
      xs <- simulate(model, 250, seed = 2 * k + 1, covariate = data_set$covariate[1:250, , drop = FALSE])
      xs2 <- simulate(model, 250, seed = 2 * k + 1, covariate = data_set$covariate[1:250, , drop = FALSE])
      expect_identical(xs2, xs)
    }
  }
})
