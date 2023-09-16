test_that("glm_sample samples only values with non zero probabilities", {
  data_set <- build_multilevel_dataset(4, 0)
  saved_target <- data_set$target
  degenerate_sets <- c(
    as.list(2:length(data_set$vals)),
    lapply(1:(length(data_set$vals) - 1), \(x) 1:x)
  )
  cc <- covlmc_control()
  for (k in seq_along(degenerate_sets)) {
    data_set$target <- saved_target
    to_remove <- data_set$vals[degenerate_sets[[k]]]
    replace_vals <- data_set$vals[-degenerate_sets[[k]]]
    to_remove_idx <- saved_target %in% to_remove
    data_set$target[to_remove_idx] <- sample(replace_vals, sum(to_remove_idx), replace = TRUE)
    for (engine in c("glm", "multinom")) {
      withr::local_options(mixvlmc.predictive = engine)
      model <- fit_glm(data_set$target, data_set$mm, length(data_set$vals), cc)
      model_samp <- rep(NA, length(data_set$target))
      for (i in 1:nrow(data_set$mm)) {
        model_samp[i] <- 1 + glm_sample_one(model, data_set$mm[i, , drop = FALSE], data_set$vals)
      }
      expect_false(any(model_samp %in% degenerate_sets[[k]]))
    }
  }
})
