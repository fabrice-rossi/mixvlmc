test_that("the likelihood calculation is valid", {
  withr::local_seed(0)
  x <- sample(c("A", "B", "C"), 1000, replace = TRUE)
  y <- ifelse(runif(length(x)) > 0.5, c(x[-1], sample(c("A", "B", "C"), 1)), c(x[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y <- as.factor(ifelse(runif(length(x)) > 0.2, y, sample(c("A", "B", "C"), 1000, replace = TRUE)))
  df_y <- data.frame(y = y)
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    x_covlmc <- covlmc(x, df_y, min_size = 5, alpha = 0.01)
    for (initial in c("truncated", "specific")) {
      expect_equal(
        as.numeric(logLik(x_covlmc, initial = initial)),
        as.numeric(loglikelihood(x_covlmc, initial = initial))
      )
      expect_equal(
        loglikelihood(x_covlmc, initial = initial, newdata = x, newcov = df_y),
        loglikelihood(x_covlmc, initial = initial)
      )
    }
  }
})

test_that("likelihood calculation on real data", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
  dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
  m_cov <- covlmc(dts, dts_cov, min_size = 5)
  for (initial in c("truncated", "specific")) {
    expect_identical(loglikelihood(m_cov, initial = initial, newdata = dts, newcov = dts_cov),
      loglikelihood(m_cov, initial = initial),
      tolerance = 1e-7
    )
  }
})

test_that("likelihood calculation on real data with merged models", {
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    d_model <- build_degenerate_elec_model(FALSE)
    for (initial in c("truncated", "specific")) {
      expect_identical(
        loglikelihood(d_model$model,
          newdata = d_model$dts,
          newcov = d_model$cov, initial = initial
        ),
        loglikelihood(d_model$model, initial = initial),
        tolerance = 1e-7
      )
    }
  }
})
