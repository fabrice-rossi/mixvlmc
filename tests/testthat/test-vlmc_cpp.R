test_that("the C++ context algorithm selects the same PST as the R implementation in a specific case", {
  withr::local_seed(0)
  x <- sample(c("A", "B", "C"), 10000, replace = TRUE)
  model <- vlmc(x, alpha = 0.05, backend = "C++")
  r_model <- vlmc(x, alpha = 0.05)
  ctx_cpp <- contexts(model)
  ctx_r <- contexts(r_model)
  expect_true(compare_ctx(ctx_r, ctx_cpp))
})

test_that("post pruning is equivalent to direct pruning (alpha, C++)", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  model <- vlmc(dts, alpha = 0.1, backend = "C++")
  cut_off <- cutoff(model)
  for (k in seq_along(cut_off)) {
    pruned_model <- prune(model, alpha = cut_off[k])
    direct_model <- vlmc(dts, alpha = cut_off[k], backend = "C++")
    expect_true(compare_vlmc_cpp(pruned_model, direct_model))
  }
})

test_that("post pruning is equivalent to direct pruning (cutoff, C++)", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  model <- vlmc(dts, alpha = 0.1, backend = "C++")
  cut_off <- cutoff(model, scale = "native")
  for (k in seq_along(cut_off)) {
    pruned_model <- prune(model, cutoff = cut_off[k])
    direct_model <- vlmc(dts, cutoff = cut_off[k], backend = "C++")
    expect_true(compare_vlmc_cpp(pruned_model, direct_model))
  }
})

test_that("alpha pruning is equivalent to cutoff pruning (C++)", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  model <- vlmc(dts, alpha = 0.1, backend = "C++")
  c_cut_off <- cutoff(model, scale = "native")
  a_cut_off <- cutoff(model, scale = "quantile")
  expect_length(a_cut_off, length(c_cut_off))
  for (k in seq_along(a_cut_off)) {
    a_pruned_model <- prune(model, alpha = a_cut_off[k])
    c_pruned_model <- prune(model, cutoff = c_cut_off[k])
    expect_true(compare_vlmc_cpp(a_pruned_model, c_pruned_model))
  }
})

test_that("cut off values do not depend (much) on the backend", {
  withr::local_seed(0)
  for (k in 1:9) {
    x <- sample(0:k, 5000 + 500 * k, replace = TRUE)
    r_tree <- vlmc(x, alpha = 0.1)
    cpp_tree <- vlmc(x, alpha = 0.1, backend = "C++")
    ## we compare only raw values to avoid introducing additional rounding
    ## errors
    expect_equal(
      cutoff(cpp_tree, raw = TRUE),
      cutoff(r_tree, raw = TRUE)
    )
    expect_equal(
      cutoff(cpp_tree, scale = "native", raw = TRUE),
      cutoff(r_tree, scale = "native", raw = TRUE)
    )
  }
})

test_that("printing works as expected", {
  withr::local_seed(0)
  x <- sample(c("A", "B", "C"), 1000, replace = TRUE)
  x_model <- vlmc(x, alpha = 0.05, backend = "C++")
  expect_snapshot(print(x_model))
})

test_that("vlmc returns an object with all the needed internal fields", {
  data_set <- build_markov_chain(500, 2, seed = 6)
  model <- vlmc(data_set$x, cutoff = 0.1, keep_match = TRUE, backend = "C++")
  expect_named(model, c(
    "root", "max_depth", "vals", "depth",
    "nb_ctx", "alpha", "cutoff", "ix", "extended_ll",
    "keep_match", "data_size", "restoration", "pruned"
  ), ignore.order = TRUE)
})

test_that("automatic C++ representation restoration works", {
  data_set <- build_markov_chain(500, 2, seed = 6)
  model <- vlmc(data_set$x, cutoff = 0.1, keep_match = TRUE, backend = "C++")
  expect_false(extptr_is_null(model$root$.pointer))
  model_path <- withr::local_tempfile(fileext = ".Rds")
  saveRDS(model, model_path)
  restored_model <- readRDS(model_path)
  expect_true(extptr_is_null(restored_model$root$.pointer))
  ctxs_orig <- contexts(model,
    frequency = "detailed", positions = TRUE,
    cutoff = "quantile", metrics = TRUE
  )
  ctxs_restored <- contexts(restored_model,
    frequency = "detailed", positions = TRUE,
    cutoff = "quantile", metrics = TRUE
  )
  expect_false(extptr_is_null(restored_model$root$.pointer))
  expect_true(compare_ctx(ctxs_orig, ctxs_restored))
})

test_that("automatic C++ representation restoration works for pruned models", {
  withr::local_seed(4)
  x <- sample(0:2, 500, replace = TRUE)
  model <- vlmc(x, min_size = 5, max_depth = 3, alpha = 0.5, backend = "C++")
  cut_off <- cutoff(model, scale = "native")
  model_path <- withr::local_tempfile(fileext = ".Rds")
  for (k in seq_along(cut_off)) {
    pruned_model <- prune(model, cutoff = cut_off[k])
    saveRDS(pruned_model, model_path)
    restored_model <- readRDS(model_path)
    expect_true(compare_vlmc_cpp(pruned_model, restored_model))
    expect_equal(loglikelihood(restored_model), loglikelihood(pruned_model))
  }
  ## pruning with a smaller cut off than the one of the model
  pruned_model <- prune(model, cutoff = cut_off[2])
  repruned_model <- prune(pruned_model, cutoff = cut_off[1])
  saveRDS(repruned_model, model_path)
  restored_model <- readRDS(model_path)
  expect_true(compare_vlmc_cpp(repruned_model, restored_model))
  ## tune_vlmc
  best_model <- as_vlmc(tune_vlmc(x, min_size = 5, max_depth = 3, backend = "C++"))
  saveRDS(best_model, model_path)
  restored_model <- readRDS(model_path)
  expect_true(compare_vlmc_cpp(best_model, restored_model))
})

test_that("the depth of the model does not include the sentinel", {
  withr::local_seed(1)
  for (k in 1:10) {
    x <- sample(0:2, sample(5:60, 1), replace = TRUE)
    model <- vlmc(x, min_size = 1, cutoff = 0, backend = "C++")
    r_model <- vlmc(x, min_size = 1, cutoff = 0, backend = "R")
    expect_equal(depth(model), depth(r_model))
    for (initial in c("truncated", "specific", "extended")) {
      expect_equal(
        loglikelihood(model, initial = initial),
        loglikelihood(r_model, initial = initial)
      )
    }
    expect_equal(predict(model, x, "probs"), predict(r_model, x, "probs"))
  }
})
