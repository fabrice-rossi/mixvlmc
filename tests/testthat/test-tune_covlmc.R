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
  bt_covlmc <- tune_covlmc(x, df_y, criterion = "BIC")
  expect_equal(stats::BIC(bt_covlmc$best_model), min(bt_covlmc$results$BIC))
  at_covlmc <- tune_covlmc(x, df_y, criterion = "AIC")
  expect_equal(stats::AIC(at_covlmc$best_model), min(at_covlmc$results$AIC))
})

test_that("tune_covlmc memorizes the models it is asked to memorize", {
  withr::local_seed(0)
  x <- sample(c("A", "B", "C"), 500, replace = TRUE)
  y <- ifelse(runif(length(x)) > 0.5, c(x[-1], sample(c("A", "B", "C"), 1)), c(x[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y <- as.factor(ifelse(runif(length(x)) > 0.2, y, sample(c("A", "B", "C"), 500, replace = TRUE)))
  df_y <- data.frame(y = y, z = runif(length(y)))
  bt_covlmc <- tune_covlmc(x, df_y, criterion = "BIC", save = "all")
  expect_equal(length(bt_covlmc$saved_models$all) + 1L, nrow(bt_covlmc$results))
  best_BIC_idx <- which.min(bt_covlmc$results$BIC)
  ## do not forget the trimming!
  expect_identical(trim(bt_covlmc$best_model), bt_covlmc$saved_models$all[[best_BIC_idx - 1]])
  ## compare the result table and the models
  quantities <- list(
    "BIC" = stats::BIC,
    "AIC" = stats::AIC,
    "loglikelihood" = stats::logLik,
    "depth" = depth,
    "nb_contexts" = context_number
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
  expect_snapshot_output(tune_covlmc(x, df_y, criterion = "BIC", verbose = 1))
})
