test_that("glm_predict returns a vector in two levels cases", {
  data_set <- build_binary_dataset(0)
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    model <- fit_glm(data_set$target, data_set$mm, 2, covlmc_control())
    model_pred <- glm_predict(model, lev = 0:1)
    expect_vector(model_pred)
    expect_length(model_pred, length(data_set$target))
    expect_probabilities(model_pred)
    model_test <- glm_predict(model, data_set$test_mm, lev = 0:1)
    expect_vector(model_test)
    expect_length(model_test, length(data_set$test_target))
    expect_probabilities(model_test)
  }
})

test_that("glm_predict returns the probability of the positive class in degenerate cases", {
  data_set <- build_binary_dataset(0)
  precision <- .Machine$double.eps^0.5
  ## turn the data set into a degenerate one
  for (one_target in 0:1) {
    data_set$target <- rep(one_target, length(data_set$target))
    cc <- covlmc_control()
    for (engine in c("glm", "multinom")) {
      withr::local_options(mixvlmc.predictive = engine)
      model <- fit_glm(data_set$target, data_set$mm, 2, cc)
      model_pred <- glm_predict(model, lev = 0:1)
      expected_prob <- (sum(data_set$target == 1L) + cc$pseudo_obs) / (length(data_set$target) + 2L * cc$pseudo_obs)
      expect_vector(model_pred)
      expect_length(model_pred, length(data_set$target))
      expect_true(all(abs(model_pred - expected_prob) < precision))
      model_test <- glm_predict(model, data_set$test_mm, lev = 0:1)
      expect_vector(model_test)
      expect_length(model_test, length(data_set$test_target))
      expect_true(all(abs(model_test - expected_prob) < precision))
    }
  }
})

test_that("glm_predict returns a matrix of probabilities when there are 3 or more levels", {
  data_set <- build_multilevel_dataset(4, 0)
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    model <- fit_glm(data_set$target, data_set$mm, length(data_set$vals), covlmc_control())
    model_pred <- glm_predict(model, lev = data_set$vals)
    expect_identical(class(model_pred), c("matrix", "array"))
    expect_equal(dim(model_pred), c(length(data_set$target), length(data_set$vals)))
    expect_probabilities(model_pred)
    model_test <- glm_predict(model, data_set$test_mm, lev = 0:1)
    expect_identical(class(model_test), c("matrix", "array"))
    expect_equal(dim(model_test), c(length(data_set$target), length(data_set$vals)))
    expect_probabilities(model_test)
  }
})

test_that("glm_predict returns a matrix of probabilities when there are 3 or more levels in degenerate cases", {
  withr::local_seed(0)
  cc <- covlmc_control()
  precision <- .Machine$double.eps^0.5
  data_set <- build_multilevel_dataset(4, 0)
  saved_target <- data_set$target
  degenerate_sets <- c(
    lapply(1:(length(data_set$vals) - 1), \(x) 1:x),
    list(c(length(data_set$vals)))
  )
  for (k in seq_along(degenerate_sets)) {
    data_set$target <- saved_target
    to_remove <- data_set$vals[degenerate_sets[[k]]]
    replace_vals <- data_set$vals[-degenerate_sets[[k]]]
    to_remove_idx <- saved_target %in% to_remove
    data_set$target[to_remove_idx] <- sample(replace_vals, sum(to_remove_idx), replace = TRUE)
    for (engine in c("glm", "multinom")) {
      withr::local_options(mixvlmc.predictive = engine)
      model <- fit_glm(data_set$target, data_set$mm, length(data_set$vals), covlmc_control())
      model_pred <- glm_predict(model, lev = data_set$vals)
      expect_identical(class(model_pred), c("matrix", "array"))
      expect_equal(dim(model_pred), c(length(data_set$target), length(data_set$vals)))
      expect_probabilities(model_pred)
      model_test <- glm_predict(model, data_set$test_mm, lev = data_set$vals)
      expect_identical(class(model_test), c("matrix", "array"))
      expect_equal(dim(model_test), c(length(data_set$target), length(data_set$vals)))
      expect_probabilities(model_test)
      if (length(to_remove) < length(data_set$vals) - 1) {
        expect_true(all(model_pred[, degenerate_sets[[k]]] == 0))
        expect_true(all(model_test[, degenerate_sets[[k]]] == 0))
      } else {
        ## fully degenerate
        zero_prob <- cc$pseudo_obs / (length(data_set$target) + length(data_set$vals) * cc$pseudo_obs)
        expect_true(all(abs(model_pred[, 1:k] - zero_prob) < precision))
        expect_true(all(abs(model_test[, 1:k] - zero_prob) < precision))
      }
    }
  }
})
