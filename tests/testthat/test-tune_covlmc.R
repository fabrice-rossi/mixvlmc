test_that("tune_covlmc obeys is basic contract", {
  withr::local_seed(0)
  x <- sample(c("A", "B", "C"), 500, replace = TRUE)
  y <- ifelse(runif(length(x)) > 0.5, c(x[-1], sample(c("A", "B", "C"), 1)), c(x[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y <- as.factor(ifelse(runif(length(x)) > 0.2, y, sample(c("A", "B", "C"), 500, replace = TRUE)))
  df_y <- data.frame(y = y, z = runif(length(y)))
  t_covlmc <- tune_covlmc(x, df_y)
  expect_s3_class(t_covlmc, "tune_covlmc")
  expect_true(all(c("best_model", "criterion", "initial", "results") %in% names(t_covlmc)))
  expect_true(is_covlmc(t_covlmc$best_model))
  expect_true(t_covlmc$criterion == "BIC") ## default value
  expect_true(t_covlmc$initial == "truncated") ## default value
  expect_null(t_covlmc$saved_models)
  expect_s3_class(t_covlmc$results, "data.frame")
})

test_that("tune_covlmc selects the best model", {
  withr::local_seed(0)
  x <- sample(c("A", "B", "C"), 500, replace = TRUE)
  y <- ifelse(runif(length(x)) > 0.5, c(x[-1], sample(c("A", "B", "C"), 1)), c(x[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y <- as.factor(ifelse(runif(length(x)) > 0.2, y, sample(c("A", "B", "C"), 500, replace = TRUE)))
  df_y <- data.frame(y = y, z = runif(length(y)))
  bt_covlmc <- tune_covlmc(x, df_y, initial = "extended", criterion = "BIC")
  expect_equal(
    stats::BIC(stats::logLik(bt_covlmc$best_model, initial = "extended")),
    min(bt_covlmc$results$BIC)
  )
  at_covlmc <- tune_covlmc(x, df_y, initial = "extended", criterion = "AIC")
  expect_equal(
    stats::AIC(stats::logLik(at_covlmc$best_model, initial = "extended")),
    min(at_covlmc$results$AIC)
  )
})

test_that("tune_covlmc memorizes the models it is asked to memorize", {
  withr::local_seed(0)
  x <- sample(c("A", "B", "C"), 500, replace = TRUE)
  y <- ifelse(runif(length(x)) > 0.5, c(x[-1], sample(c("A", "B", "C"), 1)), c(x[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y <- as.factor(ifelse(runif(length(x)) > 0.2, y, sample(c("A", "B", "C"), 500, replace = TRUE)))
  df_y <- data.frame(y = y, z = runif(length(y)))
  bt_covlmc <- tune_covlmc(x, df_y, initial = "extended", criterion = "BIC", save = "all")
  expect_equal(length(bt_covlmc$saved_models$all) + 1L, nrow(bt_covlmc$results))
  best_BIC_idx <- which.min(bt_covlmc$results$BIC)
  ## do not forget the trimming!
  expect_identical(trim(bt_covlmc$best_model), bt_covlmc$saved_models$all[[best_BIC_idx - 1]])
  ## compare the result table and the models
  quantities <- list(
    "BIC" = \(x) stats::BIC(stats::logLik(x, initial = "extended")),
    "AIC" = \(x) stats::AIC(stats::logLik(x, initial = "extended")),
    "loglikelihood" = \(x) stats::logLik(x, initial = "extended"),
    "depth" = depth,
    "nb_contexts" = context_number,
    "cov_depth" = covariate_depth
  )
  for (quantity in names(quantities)) {
    all_quant <- c(
      quantities[[quantity]](bt_covlmc$saved_models$initial),
      sapply(bt_covlmc$saved_models$all, quantities[[quantity]])
    )
    expect_equal(all_quant, bt_covlmc$results[[quantity]])
  }
})

test_that("tune_covlmc find a large enough max_depth", {
  withr::local_seed(0)
  x <- sample(c("A", "B", "C"), 500, replace = TRUE)
  y <- ifelse(runif(length(x)) > 0.5, c(x[-1], sample(c("A", "B", "C"), 1)), c(x[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y <- as.factor(ifelse(runif(length(x)) > 0.2, y, sample(c("A", "B", "C"), 500, replace = TRUE)))
  df_y <- data.frame(y = y, z = runif(length(y)))
  bt_covlmc_auto <- tune_covlmc(x, df_y, criterion = "BIC", max_depth = 2)
  bt_covlmc <- tune_covlmc(x, df_y, criterion = "BIC", max_depth = 100)
  expect_equal(bt_covlmc, bt_covlmc_auto)
})

test_that("tune_covlmc is robust to rounding errors in refitted models", {
  skip_on_cran()
  ## some BLAS (e.g. MKL) give results that differ in the last digits when the
  ## same logistic model is fitted twice: this is emulated by a small random
  ## perturbation of the log likelihoods of the models
  exact_glm_likelihood <- glm_likelihood
  for (seed in 1:5) {
    withr::local_seed(seed)
    x <- sample(c("A", "B", "C"), 500, replace = TRUE)
    y <- ifelse(runif(length(x)) > 0.5, c(x[-1], sample(c("A", "B", "C"), 1)), c(x[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
    y <- as.factor(ifelse(runif(length(x)) > 0.2, y, sample(c("A", "B", "C"), 500, replace = TRUE)))
    df_y <- data.frame(y = y, z = runif(length(y)))
    bt_covlmc <- tune_covlmc(x, df_y, criterion = "BIC", max_depth = 2)
    local({
      local_mocked_bindings(glm_likelihood = function(...) {
        exact_glm_likelihood(...) + stats::runif(1, -1e-12, 1e-12)
      })
      bt_covlmc_noisy <- tune_covlmc(x, df_y, criterion = "BIC", max_depth = 2)
      expect_equal(bt_covlmc_noisy$results, bt_covlmc$results)
    })
  }
})

test_that("print works as expected", {
  withr::local_seed(0)
  x <- sample(c("A", "B", "C"), 500, replace = TRUE)
  y <- ifelse(runif(length(x)) > 0.5, c(x[-1], sample(c("A", "B", "C"), 1)), c(x[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y <- as.factor(ifelse(runif(length(x)) > 0.2, y, sample(c("A", "B", "C"), 500, replace = TRUE)))
  df_y <- data.frame(y = y, z = runif(length(y)))
  bt_covlmc <- tune_covlmc(x, df_y, criterion = "BIC")
  at_covlmc <- tune_covlmc(x, df_y, criterion = "AIC")
  expect_snapshot(print(bt_covlmc))
  expect_snapshot(print(at_covlmc))
})

test_that("summary works as expected", {
  skip_on_ci()
  skip_on_cran()
  withr::local_seed(0)
  x <- sample(c("A", "B", "C"), 500, replace = TRUE)
  y <- ifelse(runif(length(x)) > 0.5, c(x[-1], sample(c("A", "B", "C"), 1)), c(x[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y <- as.factor(ifelse(runif(length(x)) > 0.2, y, sample(c("A", "B", "C"), 500, replace = TRUE)))
  df_y <- data.frame(y = y, z = runif(length(y)))
  bt_covlmc <- tune_covlmc(x, df_y, criterion = "BIC")
  at_covlmc <- tune_covlmc(x, df_y, criterion = "AIC")
  expect_snapshot(print(summary(bt_covlmc)))
  expect_snapshot(print(summary(at_covlmc)))
})


test_that("tune_vlmc verbosity is adequate", {
  withr::local_seed(42)
  x <- sample(c("A", "B", "C"), 500, replace = TRUE)
  y <- ifelse(runif(length(x)) > 0.5, c(x[-1], sample(c("A", "B", "C"), 1)), c(x[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y <- as.factor(ifelse(runif(length(x)) > 0.2, y, sample(c("A", "B", "C"), 500, replace = TRUE)))
  df_y <- data.frame(y = y, z = runif(length(y)))
  expect_snapshot(tune_covlmc(x, df_y, criterion = "BIC", verbose = 1))
})

test_that("tune_covlmc trimming", {
  skip_on_cran()
  withr::local_seed(0)
  x <- sample(c("A", "B", "C"), 1000, replace = TRUE)
  y <- ifelse(runif(length(x)) > 0.5, c(x[-1], sample(c("A", "B", "C"), 1)), c(x[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y <- as.factor(ifelse(runif(length(x)) > 0.2, y, sample(c("A", "B", "C"), 500, replace = TRUE)))
  df_y <- data.frame(y = y, z = runif(length(y)))
  bt_covlmc <- tune_covlmc(x, df_y,
    criterion = "BIC", save = "all", trimming = "partial",
    best_trimming = "partial"
  )
  expect_equal(length(bt_covlmc$saved_models$all) + 1L, nrow(bt_covlmc$results))
  best_BIC_idx <- which.min(bt_covlmc$results$BIC)
  expect_identical(bt_covlmc$best_model, bt_covlmc$saved_models$all[[best_BIC_idx - 1]])
  for (model in bt_covlmc$saved_models$all) {
    expect_no_error(contexts(model, hsize = TRUE, model = "full"))
  }
  expect_no_error(contexts(bt_covlmc$saved_models$initial, hsize = TRUE, model = "full"))
  bt_covlmc <- tune_covlmc(x, df_y,
    criterion = "BIC", save = "all", trimming = "full",
    best_trimming = "full"
  )
  expect_equal(length(bt_covlmc$saved_models$all) + 1L, nrow(bt_covlmc$results))
  best_BIC_idx <- which.min(bt_covlmc$results$BIC)
  expect_identical(bt_covlmc$best_model, bt_covlmc$saved_models$all[[best_BIC_idx - 1]])
  for (model in bt_covlmc$saved_models$all) {
    expect_error(contexts(model, hsize = TRUE, model = "full"))
  }
  expect_error(contexts(bt_covlmc$saved_models$initial, hsize = TRUE, model = "full"))
})

test_that("tune_covlmc selects the best model", {
  withr::local_seed(0)
  x <- sample(c("A", "B", "C"), 500, replace = TRUE)
  y <- ifelse(runif(length(x)) > 0.5, c(x[-1], sample(c("A", "B", "C"), 1)), c(x[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y <- as.factor(ifelse(runif(length(x)) > 0.2, y, sample(c("A", "B", "C"), 500, replace = TRUE)))
  df_y <- data.frame(y = y, z = runif(length(y)))
  covlmc_alpha <- tune_covlmc(x, df_y, alpha_init = 0.1, initial = "extended", criterion = "BIC")
  expect_equal(covlmc_alpha$results$alpha[1], 0.1)
})

test_that("tune_covlmc reports the log likelihood used for model selection", {
  skip_on_cran()
  pc <- powerconsumption[powerconsumption$week %in% 10:11, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(1 / 2, 1))))
  dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
  for (initial in c("truncated", "extended")) {
    t_res <- tune_covlmc(dts, dts_cov, initial = initial, min_size = 5)
    crit <- t_res$results$BIC
    sel_ll <- t_res$results$loglikelihood[max(which(crit == min(crit)))]
    expected <- paste0("\"", initial, "\" (", format(sel_ll), ")")
    expect_output(print(t_res), expected, fixed = TRUE)
    expect_output(print(summary(t_res)), expected, fixed = TRUE)
  }
})

test_that("tune_covlmc increases a null max_depth without warning", {
  withr::local_seed(0)
  x <- sample(c("A", "B", "C"), 300, replace = TRUE)
  x <- c(x[1], ifelse(x[-1] == "A", x[-length(x)], x[-1]))
  y <- data.frame(y = rnorm(length(x)))
  expect_no_warning(tune_covlmc(x, y, max_depth = 0))
})

test_that("tune_covlmc uses its control parameters", {
  rdts <- rep(c(0, 1), 100)
  rdts_cov <- data.frame(y = rep(0, length(rdts)))
  default_tune <- tune_covlmc(rdts, rdts_cov)
  tune_10 <- tune_covlmc(rdts, rdts_cov, control = covlmc_control(pseudo_obs = 10))
  expect_identical(as_covlmc(tune_10)$control$pseudo_obs, 10)
  expect_identical(
    as_covlmc(tune_10)$control,
    covlmc(rdts, rdts_cov, control = covlmc_control(pseudo_obs = 10))$control
  )
  expect_lt(
    as.numeric(logLik(as_covlmc(tune_10))),
    as.numeric(logLik(as_covlmc(default_tune)))
  )
})
