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
    expect_identical(as_sequence(x_covlmc_ctx[[1]]), 0)
    expect_identical(as_sequence(x_covlmc_ctx[[2]]), 1)
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
    expect_identical(as_sequence(x_covlmc_ctx[[1]]), 0)
    expect_identical(as_sequence(x_covlmc_ctx[[2]]), 1)
    expect_identical(as_sequence(x_covlmc_ctx[[3]]), 2)
  }
})

test_that("covlmc reports p-values correctly", {
  data_set <- build_data_set(1000, seed = 0)
  alpha <- 0.1
  model <- covlmc(data_set$x, data_set$covariate, alpha = alpha, min_size = 3)
  p_values <- extract_p_value(model)
  expect_false(anyNA(p_values$p_value[!is.na(p_values$nb_coeff) & p_values$nb_coeffs > 1]))
  expect_false(any(p_values$p_value[!is.na(p_values$nb_coeff) & p_values$nb_coeffs > 1] > alpha))
})

test_that("covlmc prune preserves p-values", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
  rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
  m_cov <- covlmc(rdts, rdts_cov, min_size = 5, keep_data = TRUE)
  p_values_m <- extract_p_value(m_cov)
  m_cov_cuts <- cutoff(m_cov)
  alpha <- m_cov_cuts[1] - 2 * .Machine$double.eps
  p_cov <- prune(m_cov, alpha)
  p_values <- extract_p_value(p_cov)
  expect_false(anyNA(p_values$p_value[is.na(p_values$nb_coeffs) | p_values$nb_coeffs > 1]))
  expect_false(any(p_values$p_value[is.na(p_values$nb_coeffs) | p_values$nb_coeffs > 1] > alpha))
})

test_that("covlmc prune works with more that 2 states", {
  withr::local_seed(0)
  x <- sample(c("A", "B", "C"), 500, replace = TRUE)
  y <- ifelse(runif(length(x)) > 0.5, c(x[-1], sample(c("A", "B", "C"), 1)), c(x[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y <- as.factor(ifelse(runif(length(x)) > 0.2, y, sample(c("A", "B", "C"), 500, replace = TRUE)))
  df_y <- data.frame(
    y = y, z = runif(length(y)),
    q = sample(c(TRUE, FALSE), length(y), replace = TRUE)
  )
  model <- covlmc(x, df_y, max_depth = 5, min_size = 3, alpha = 0.005)
  model_2 <- prune(model, 0.0001)
  model_3 <- covlmc(x, df_y, max_depth = 5, min_size = 3, alpha = 0.0001)
  expect_true(compare_covlmc(model_2, model_3))
})

test_that("covlmc sequential pruning does not produce NAs", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
  rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
  m_cov <- covlmc(rdts, rdts_cov, min_size = 3, max_depth = 6, alpha = 0.5, keep_data = TRUE)
  m_cov_cuts <- cutoff(m_cov)
  m_current <- m_cov
  for (k in seq_along(m_cov_cuts)) {
    expect_warning(
      {
        m_current <- prune(m_current, m_cov_cuts[k])
      },
      regexp = NA
    )
  }
})

test_that("covlmc sequential pruning does not produce NAs with more than 3 states", {
  withr::local_seed(0)
  x <- sample(c("A", "B", "C"), 500, replace = TRUE)
  y <- ifelse(runif(length(x)) > 0.5, c(x[-1], sample(c("A", "B", "C"), 1)), c(x[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y <- as.factor(ifelse(runif(length(x)) > 0.2, y, sample(c("A", "B", "C"), 500, replace = TRUE)))
  df_y <- data.frame(y = y, z = runif(length(y)))
  model <- covlmc(x, df_y, max_depth = 5, min_size = 2, alpha = 0.5)
  m_cuts <- cutoff(model)
  m_current <- model
  for (k in seq_along(m_cuts)) {
    expect_warning(
      {
        m_current <- prune(m_current, m_cuts[k])
      },
      regexp = NA
    )
  }
})

test_that("covlmc handles defficient rank covariates", {
  pc_week_5 <- powerconsumption[powerconsumption$week == 5, ]
  elec <- pc_week_5$active_power
  elec_rdts <- cut(elec, breaks = c(0, 0.4, 2, 8), labels = c("low", "typical", "high"))
  elec_cov <- data.frame(day = (pc_week_5$hour >= 7 & pc_week_5$hour <= 17))
  expect_error(covlmc(elec_rdts, elec_cov, min_size = 10), regexp = NA)
})

test_that("covlmc handles tibble correctly", {
  skip_if_not_installed("tibble")
  data_set <- build_data_set_2(seed = 0)
  alpha <- 0.5
  model_df <- covlmc(data_set$x, data_set$covariate, alpha = alpha, min_size = 3)
  data_set$covariate <- tibble::as_tibble(data_set$covariate)
  model_tb <- covlmc(data_set$x, data_set$covariate, alpha = alpha, min_size = 3)
  expect_true(compare_covlmc(model_df, model_tb))
})

test_that("covlmc results do not depend on the dts representation", {
  data_set <- build_data_set(1000, seed = 0)
  alpha <- 0.1
  model <- covlmc(data_set$x, data_set$covariate, alpha = alpha, min_size = 3)
  dts_model <- covlmc(dts(data_set$x), data_set$covariate, alpha = alpha, min_size = 3)
  expect_true(compare_covlmc(model, dts_model))
})
