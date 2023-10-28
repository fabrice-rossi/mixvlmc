test_that("models are correctly reported", {
  withr::local_seed(0)
  dts <- sample(c("A", "B", "C"), 1000, replace = TRUE)
  y <- ifelse(runif(length(dts)) > 0.5, c(dts[-1], sample(c("A", "B", "C"), 1)), c(dts[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y <- as.factor(ifelse(runif(length(dts)) > 0.2, y, sample(c("A", "B", "C"), length(dts), replace = TRUE)))
  df_y <- data.frame(y = y)
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    model <- covlmc(dts, df_y, alpha = 0.01, min_size = 3 / 2)
    ctxs_obj <- contexts(model)
    ctxs_df <- contexts(model, type = "data.frame", model = "coef")
    ctxs_coef <- lapply(ctxs_obj, model, type = "coef")
    expect_identical(ctxs_coef, unclass(ctxs_df$coef))
    ctxs_df_f <- contexts(model, type = "data.frame", model = "full")
    ctxs_full <- lapply(ctxs_obj, model, type = "full")
    expect_identical(ctxs_full, unclass(ctxs_df_f$model))
  }
})

test_that("metrics and hsize are correctly reported", {
  withr::local_seed(1)
  dts <- sample(c("A", "B", "C"), 1000, replace = TRUE)
  y <- ifelse(runif(length(dts)) > 0.5, c(dts[-1], sample(c("A", "B", "C"), 1)), c(dts[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y <- as.factor(ifelse(runif(length(dts)) > 0.2, y, sample(c("A", "B", "C"), length(dts), replace = TRUE)))
  df_y <- data.frame(y = y)
  model <- covlmc(dts, df_y, alpha = 0.01, min_size = 3 / 2)
  ctxs_obj <- contexts(model)
  ctxs_df <- contexts(model, type = "data.frame", metrics = TRUE, hsize = TRUE)
  ctxs_metrics <- lapply(ctxs_obj, metrics)
  expect_identical(sapply(ctxs_metrics, \(x) x$accuracy), ctxs_df$accuracy)
  expect_identical(sapply(ctxs_metrics, \(x) x$auc), ctxs_df$auc)
  expect_identical(sapply(ctxs_obj, covariate_memory), ctxs_df$hsize)
})

test_that("merged models are correctly reported", {
  d_model <- build_degenerate_elec_model(TRUE)
  ctxs_obj <- contexts(d_model$model)
  ctxs_df <- contexts(d_model$model, type = "data.frame", model = "full", merging = TRUE)
  ctxs_merged <- sapply(ctxs_obj, is_merged)
  expect_identical(ctxs_merged, ctxs_df$merged)
  for (k in seq_along(ctxs_obj)) {
    if (!is_merged(ctxs_obj[[k]])) {
      expect_null(merged_with(ctxs_obj[[k]]))
    } else {
      mwith <- merged_with(ctxs_obj[[k]])
      expect_equal(length(mwith), length(states(d_model$model)))
      expect_equal(names(mwith), as.character(states(d_model$model)))
      for (j in seq_along(mwith)) {
        if (!is.null(mwith[[j]])) {
          expect_identical(
            model(ctxs_obj[[k]], type = "full"),
            model(mwith[[j]], type = "full")
          )
        }
      }
    }
  }
})

test_that("sequences are properly extracted", {
  d_model <- build_degenerate_elec_model(TRUE)
  ctxs_obj <- contexts(d_model$model)
  for (k in seq_along(ctxs_obj)) {
    direct_seq <- find_sequence(d_model$model, as_sequence(ctxs_obj[[k]]))
    expect_identical(direct_seq, ctxs_obj[[k]])
  }
})
