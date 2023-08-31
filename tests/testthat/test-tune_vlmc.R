test_that("tune_vlmc obeys is basic contract", {
  data_set <- build_markov_chain(1000, 4, seed = 4)
  t_vlmc <- tune_vlmc(data_set$x)
  expect_s3_class(t_vlmc, "tune_vlmc")
  expect_true(all(c("best_model", "criterion", "initial", "results") %in% names(t_vlmc)))
  expect_true(is_vlmc(t_vlmc$best_model))
  expect_true(t_vlmc$criterion == "BIC") ## default value
  expect_true(t_vlmc$initial == "truncated") ## default value
  expect_null(t_vlmc$saved_models)
  expect_s3_class(t_vlmc$results, "data.frame")
})

test_that("tune_vlmc selects the best model", {
  data_set <- build_markov_chain(300, 3, seed = 3)
  bt_vlmc <- tune_vlmc(data_set$x, criterion = "BIC")
  expect_equal(stats::BIC(bt_vlmc$best_model), min(bt_vlmc$results$BIC))
  at_vlmc <- tune_vlmc(data_set$x, criterion = "AIC")
  expect_equal(stats::AIC(at_vlmc$best_model), min(at_vlmc$results$AIC))
})

test_that("tune_vlmc memorizes the models it is asked to memorize", {
  data_set <- build_markov_chain(500, 4, seed = 2)
  bt_vlmc <- tune_vlmc(data_set$x, criterion = "BIC", save = "all")
  expect_equal(length(bt_vlmc$saved_models$all) + 1L, nrow(bt_vlmc$results))
  best_BIC_idx <- which.min(bt_vlmc$results$BIC)
  expect_identical(bt_vlmc$best_model, bt_vlmc$saved_models$all[[best_BIC_idx - 1]])
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
      quantities[[quantity]](bt_vlmc$saved_models$initial),
      sapply(bt_vlmc$saved_models$all, quantities[[quantity]])
    )
    expect_equal(all_quant, bt_vlmc$results[[quantity]])
  }
})

test_that("tune_vlmc find a large enough max_depth", {
  for (k in 2:4) {
    data_set <- build_markov_chain(500, k, seed = 3 * k)
    t_vlmc_auto <- tune_vlmc(data_set$x, max_depth = 2)
    t_vlmc <- tune_vlmc(data_set$x, max_depth = 100)
    expect_equal(t_vlmc, t_vlmc_auto)
  }
})

test_that("print works as expected", {
  data_set <- build_markov_chain(500, 3, seed = 0)
  t_vlmc_auto <- tune_vlmc(data_set$x, max_depth = 2)
  expect_snapshot_output(print(t_vlmc_auto))
  t_vlmc_auto <- tune_vlmc(data_set$x,
    max_depth = 2,
    initial = "extended", criterion = "AIC"
  )
  expect_snapshot_output(print(t_vlmc_auto))
})

test_that("summary works as expected", {
  skip_on_ci()
  skip_on_cran()
  data_set <- build_markov_chain(500, 3, seed = 0)
  t_vlmc_auto <- tune_vlmc(data_set$x, max_depth = 2)
  expect_snapshot_output(print(summary(t_vlmc_auto)))
  t_vlmc_auto <- tune_vlmc(data_set$x,
    max_depth = 2,
    initial = "extended", criterion = "AIC"
  )
  expect_snapshot_output(print(summary(t_vlmc_auto)))
})
