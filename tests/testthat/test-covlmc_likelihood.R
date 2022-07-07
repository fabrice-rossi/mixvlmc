test_that("the likelihood calculation is valid", {
  x <- sample(c("A", "B", "C"), 1000, replace = TRUE)
  y <- ifelse(runif(length(x)) > 0.5, c(x[-1], sample(c("A", "B", "C"), 1)), c(x[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y <- as.factor(ifelse(runif(length(x)) > 0.2, y, sample(c("A", "B", "C"), 1000, replace = TRUE)))
  df_y <- data.frame(y = y)
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    x_covlmc <- covlmc(x, df_y, alpha = 0.05)
    expect_equal(as.numeric(logLik(x_covlmc)), as.numeric(loglikelihood(x_covlmc)))
    expect_equal(loglikelihood(x_covlmc, x, newcov = df_y), loglikelihood(x_covlmc))
  }
})
