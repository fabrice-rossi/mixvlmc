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
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
  dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
  m_cov <- covlmc(dts, dts_cov, min_size = 5, keep_data = TRUE)
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
  df_y <- data.frame(y = y, z = runif(length(y)))
  model <- covlmc(x, df_y, max_depth = 5, min_size = 3, alpha = 0.005)
  model_2 <- prune(model, 0.0001)
  model_3 <- covlmc(x, df_y, max_depth = 5, min_size = 3, alpha = 0.0001)
  expect_true(compare_covlmc(model_2, model_3))
})

test_that("covlmc sequential pruning does not produce NAs", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
  dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
  m_cov <- covlmc(dts, dts_cov, min_size = 3, max_depth = 6, alpha = 0.5, keep_data = TRUE)
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
  elec_dts <- cut(elec, breaks = c(0, 0.4, 2, 8), labels = c("low", "typical", "high"))
  elec_cov <- data.frame(day = (pc_week_5$hour >= 7 & pc_week_5$hour <= 17))
  expect_error(covlmc(elec_dts, elec_cov, min_size = 10), regexp = NA)
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

test_that("covlmc handles states missing in some contexts", {
  withr::local_seed(1)
  n <- 200
  ## b never follows a
  P <- matrix(c(
    .4, 0, .3, .3,
    .25, .25, .25, .25,
    .3, .3, .2, .2,
    .1, .4, .2, .3
  ), 4, byrow = TRUE)
  x <- integer(n)
  x[1] <- 1
  z <- rnorm(n)
  for (i in 2:n) {
    x[i] <- sample(1:4, 1, prob = P[x[i - 1], ])
  }
  x <- letters[x]
  cv <- data.frame(z = z)
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    expect_no_error(x_covlmc <- suppressWarnings(covlmc(x, cv, min_size = 2, alpha = 0.5)))
    x_ll <- loglikelihood(x_covlmc)
    expect_true(is.finite(x_ll))
    expect_equal(x_ll, loglikelihood(x_covlmc, newdata = x, newcov = cv), ignore_attr = TRUE)
  }
})

test_that("node_fit_glm_full_rank_with_data discards models with a NA likelihood", {
  withr::local_seed(0)
  n <- 100
  mm <- data.frame(z_1 = rnorm(n), z_2 = rnorm(n))
  target <- sample(0:1, n, replace = TRUE)
  local_mocked_bindings(glm_likelihood = function(model, mm, target) {
    if (ncol(mm) > 0) {
      NA_real_
    } else {
      -1
    }
  })
  res <- node_fit_glm_full_rank_with_data(mm, 2L, target, 1, 2, covlmc_control())
  expect_identical(res$hsize, 0L)
})

test_that("covlmc rejects missing values in covariates", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
  dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
  dts_cov$day_night[c(10, 50, 300)] <- NA
  expect_error(covlmc(dts, dts_cov, min_size = 5), "missing values")
})

test_that("likelihood ratio tests use the number of parameters of factors", {
  withr::local_seed(4)
  n <- 400
  f <- factor(sample(c("A", "B", "C", "D"), n, replace = TRUE))
  y <- rbinom(n, 1, 0.5)
  res <- node_fit_glm(0:(n - 2), 1L, y, data.frame(f = f), 0.05, 2,
    return_all = TRUE, control = covlmc_control()
  )
  df <- length(coef(res$H1_model$model)) - length(coef(res$H0_model$model))
  expect_equal(df, 3)
  expect_equal(res$p_value, stats::pchisq(res$lambda, df = df, lower.tail = FALSE))
})

test_that("covlmc gives the same results with factors and dummy variables", {
  withr::local_seed(1)
  n <- 600
  f <- factor(sample(c("A", "B", "C", "D"), n, replace = TRUE))
  x <- rep(0L, n)
  for (k in 2:n) {
    p <- c(0.3, 0.6, 0.5, 0.55)[as.integer(f[k - 1])]
    if (x[k - 1] == 1) {
      p <- 1 - p
    }
    x[k] <- stats::rbinom(1, 1, p)
  }
  cov_f <- data.frame(f = f)
  ## treatment contrasts
  cov_d <- as.data.frame(stats::model.matrix(~f, cov_f)[, -1])
  x3 <- x + (c(0L, x[-n]) == 1 & x == 1)
  for (dts in list(x, x3)) {
    m_f <- covlmc(dts, cov_f, alpha = 0.2, min_size = 3)
    m_d <- covlmc(dts, cov_d, alpha = 0.2, min_size = 3)
    expect_identical(context_number(m_f), context_number(m_d))
    expect_identical(covariate_depth(m_f), covariate_depth(m_d))
    expect_equal(extract_p_value(m_f), extract_p_value(m_d))
    expect_equal(loglikelihood(m_f), loglikelihood(m_d))
    a_cut <- cutoff(m_f)[2]
    expect_equal(cutoff(m_f), cutoff(m_d))
    p_f <- prune(m_f, a_cut)
    p_d <- prune(m_d, a_cut)
    expect_equal(extract_p_value(p_f), extract_p_value(p_d))
    expect_equal(loglikelihood(p_f), loglikelihood(p_d))
  }
})

test_that("unused levels of factors do not change the model", {
  pc <- powerconsumption[powerconsumption$week %in% 5:6, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
  dn <- ifelse(pc$hour >= 7 & pc$hour <= 17, "day", "night")
  cov_1 <- data.frame(dn = factor(dn, levels = c("day", "night")))
  cov_2 <- data.frame(dn = factor(dn, levels = c("day", "night", "unused")))
  m_1 <- covlmc(dts, cov_1, min_size = 5)
  m_2 <- covlmc(dts, cov_2, min_size = 5)
  expect_identical(context_number(m_2), context_number(m_1))
  expect_identical(covariate_depth(m_2), covariate_depth(m_1))
  expect_equal(extract_p_value(m_2), extract_p_value(m_1))
  expect_equal(loglikelihood(m_2), loglikelihood(m_1))
  ## new covariates with the original levels
  expect_equal(
    predict(m_2, dts[1:100], cov_2[1:100, , drop = FALSE], type = "probs"),
    predict(m_1, dts[1:100], cov_1[1:100, , drop = FALSE], type = "probs")
  )
  expect_equal(
    loglikelihood(m_2, newdata = dts, newcov = cov_2),
    loglikelihood(m_1, newdata = dts, newcov = cov_1)
  )
  expect_identical(
    simulate(m_2, 100, seed = 0, covariate = cov_2),
    simulate(m_1, 100, seed = 0, covariate = cov_1)
  )
  ## an actual use of a new level is rejected
  cov_3 <- cov_2
  cov_3$dn[10] <- "unused"
  expect_error(predict(m_2, dts, cov_3), "new level")
})

test_that("covlmc handles rank deficiency similarly with both engines", {
  data_set <- build_data_set(500, seed = 0)
  ## collinear covariates
  dts_cov <- data.frame(y = data_set$covariate$y, y2 = 2 * data_set$covariate$y)
  models <- list()
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    models[[engine]] <- covlmc(data_set$x, dts_cov, min_size = 5)
  }
  expect_equal(covariate_depth(models$glm), 0)
  expect_identical(covariate_depth(models$multinom), covariate_depth(models$glm))
  expect_identical(context_number(models$multinom), context_number(models$glm))
})
