test_that("loglikelihood computes the expected values", {
  withr::local_seed(0)
  x <- sample(c("A", "B", "C"), 1000, replace = TRUE)
  y <- ifelse(runif(length(x)) > 0.5, c(x[-1], sample(c("A", "B", "C"), 1)), c(x[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y <- as.factor(ifelse(runif(length(x)) > 0.2, y, sample(c("A", "B", "C"), 1000, replace = TRUE)))
  df_y <- data.frame(y = y)
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    x_covlmc <- covlmc(x, df_y, min_size = 5, alpha = 0.01)
    for (initial in c("truncated", "specific", "extended")) {
      fll <- loglikelihood(x_covlmc, initial = initial, newdata = x, newcov = df_y)
      sll <- co_slow_loglikelihood(x_covlmc, initial = initial, newdata = x, newcov = df_y)
      dll <- loglikelihood(x_covlmc, initial = initial)
      expect_equal(as.numeric(fll), as.numeric(sll))
      expect_equal(attr(fll, "df"), attr(sll, "df"))
      expect_equal(attr(fll, "nobs"), attr(sll, "nobs"))
      expect_equal(as.numeric(dll), as.numeric(sll))
      expect_equal(attr(dll, "df"), attr(sll, "df"))
      expect_equal(attr(dll, "nobs"), attr(sll, "nobs"))
    }
  }
})

test_that("the likelihood calculation is valid", {
  withr::local_seed(0)
  x <- sample(c("A", "B", "C"), 1000, replace = TRUE)
  y <- ifelse(runif(length(x)) > 0.5, c(x[-1], sample(c("A", "B", "C"), 1)), c(x[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y <- as.factor(ifelse(runif(length(x)) > 0.2, y, sample(c("A", "B", "C"), 1000, replace = TRUE)))
  df_y <- data.frame(y = y)
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    x_covlmc <- covlmc(x, df_y, min_size = 5, alpha = 0.01)
    for (initial in c("truncated", "specific", "extended")) {
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
  for (initial in c("truncated", "specific", "extended")) {
    expect_identical(loglikelihood(m_cov, initial = initial, newdata = dts, newcov = dts_cov),
      loglikelihood(m_cov, initial = initial),
      tolerance = 1e-7
    )
  }
})

test_that("likelihood calculation on real data with merged models", {
  skip_on_cran()
  skip_on_ci()
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    d_model <- build_degenerate_elec_model(FALSE)
    for (initial in c("truncated", "specific", "extended")) {
      fll <- loglikelihood(d_model$model,
        newdata = d_model$dts,
        newcov = d_model$cov,
        initial = initial
      )
      expect_identical(fll,
        loglikelihood(d_model$model, initial = initial),
        tolerance = 1e-7
      )
      sll <- co_slow_loglikelihood(d_model$model,
        initial = initial,
        newdata = d_model$dts,
        newcov = d_model$cov
      )
      expect_equal(as.numeric(fll), as.numeric(sll))
    }
  }
})

test_that("likelihood calculation artifical data with merged models", {
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    d_model <- create_demo_covlmc()
    for (initial in c("truncated", "specific", "extended")) {
      fll <- loglikelihood(d_model$model,
        newdata = d_model$dts,
        newcov = d_model$cov,
        initial = initial
      )
      expect_identical(fll,
        loglikelihood(d_model$model, initial = initial),
        tolerance = 1e-7
      )
      sll <- co_slow_loglikelihood(d_model$model,
        initial = initial,
        newdata = d_model$dts,
        newcov = d_model$cov
      )
      expect_equal(as.numeric(fll), as.numeric(sll))
    }
  }
})
