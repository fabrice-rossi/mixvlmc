test_that("vlmc estimation works on super simple case", {
  x <- rep(c(0, 1), 1000)
  x_vlmc <- vlmc(x)
  expect_identical(depth(x_vlmc), 1L)
  expect_identical(context_number(x_vlmc), 2L)
  x_vlmc_ctx <- contexts(x_vlmc)
  expect_identical(length(x_vlmc_ctx), 2L)
  expect_identical(as_sequence(x_vlmc_ctx[[1]]), 0)
  expect_identical(as_sequence(x_vlmc_ctx[[2]]), 1)
})

test_that("vlmc reports correctly the empty context if it exists", {
  withr::local_seed(0)
  dts <- sample(0:1, size = 100, replace = TRUE)
  empty_vlmc <- vlmc(dts, max_depth = 0)
  expect_equal(context_number(empty_vlmc), 1L)
})

test_that("post pruning is equivalent to direct pruning (alpha)", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  model <- vlmc(dts, alpha = 0.1, keep_match = TRUE)
  cut_off <- cutoff(model)
  for (k in seq_along(cut_off)) {
    pruned_model <- prune(model, alpha = cut_off[k])
    direct_model <- vlmc(dts, alpha = cut_off[k], keep_match = TRUE)
    expect_true(compare_vlmc(pruned_model, direct_model))
  }
})

test_that("post pruning is equivalent to direct pruning (cutoff)", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  model <- vlmc(dts, alpha = 0.1)
  cut_off <- cutoff(model, scale = "native")
  for (k in seq_along(cut_off)) {
    pruned_model <- prune(model, cutoff = cut_off[k])
    direct_model <- vlmc(dts, cutoff = cut_off[k])
    expect_true(compare_vlmc(pruned_model, direct_model))
  }
})

test_that("alpha pruning is equivalent to cutoff pruning", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  model <- vlmc(dts, alpha = 0.1)
  c_cut_off <- cutoff(model, scale = "native")
  a_cut_off <- cutoff(model, scale = "quantile")
  expect_length(a_cut_off, length(c_cut_off))
  for (k in seq_along(a_cut_off)) {
    a_pruned_model <- prune(model, alpha = a_cut_off[k])
    c_pruned_model <- prune(model, cutoff = c_cut_off[k])
    expect_equal(a_pruned_model, c_pruned_model)
  }
})

test_that("max_depth is always reported", {
  data_set <- build_markov_chain(500, 2, seed = 6)
  model <- vlmc(data_set$x, cutoff = 0.5 * log(length(data_set$x)), max_depth = 2)
  expect_true(model$max_depth)
})

test_that("vlmc always returns a vlmc object", {
  data_set <- build_markov_chain(500, 2, seed = 6)
  model <- vlmc(data_set$x, cutoff = 0.5 * log(length(data_set$x)), max_depth = 2, prune = FALSE)
  expect_s3_class(model, "vlmc")
})

test_that("vlmc returns an object with all the needed internal fields", {
  data_set <- build_markov_chain(500, 2, seed = 6)
  model <- vlmc(data_set$x, cutoff = 0.1, keep_match = TRUE)
  expect_named(model, c(
    "children", "f_by", "max_depth", "vals", "depth",
    "nb_ctx", "alpha", "cutoff", "ix", "extended_ll",
    "keep_match", "data_size"
  ), ignore.order = TRUE)
})
