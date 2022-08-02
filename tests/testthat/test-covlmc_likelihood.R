test_that("the likelihood calculation is valid", {
  withr::local_seed(0)
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

test_that("likelihood calculation on real data", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
  dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
  m_cov <- covlmc(dts, dts_cov, min_size = 5)
  expect_identical(loglikelihood(m_cov, dts, dts_cov), loglikelihood(m_cov), tolerance = 1e-7)
})
