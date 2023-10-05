test_that("as_vlmc.ctx_tree produces a valid vlmc object", {
  data_set <- build_markov_chain(1000, 4, seed = 4)
  data_tree <- ctx_tree(data_set$x, min_size = 4, max_depth = 10)
  vlmc_from_tree <- as_vlmc(data_tree)
  expect_named(vlmc_from_tree, c(
    "children", "f_by", "max_depth", "vals", "depth",
    "nb_ctx", "alpha", "cutoff", "ix", "extended_ll",
    "keep_match", "data_size"
  ), ignore.order = TRUE)
  ## degenerate case should add match
  withr::local_seed(0)
  data_set <- sample(c("A", "B", "C"), 500, replace = TRUE)
  data_tree <- ctx_tree(data_set, min_size = 4, max_depth = 10)
  vlmc_from_tree <- as_vlmc(data_tree, alpha = 0.01)
  expect_named(vlmc_from_tree, c(
    "f_by", "max_depth", "vals", "depth",
    "nb_ctx", "alpha", "cutoff", "ix", "extended_ll",
    "keep_match", "data_size", "match"
  ), ignore.order = TRUE)
})

test_that("as_vlmc.ctx_tree obeys is basic contract", {
  data_set <- build_markov_chain(1000, 4, seed = 4)
  data_tree <- ctx_tree(data_set$x, min_size = 4, max_depth = 10)
  vlmc_from_tree <- as_vlmc(data_tree)
  expect_true(is_vlmc(vlmc_from_tree))
  vlmc_direct <- vlmc(data_set$x,
    min_size = 4, max_depth = 10, cutoff = 0,
    keep_match = TRUE
  )
  expect_equal(
    contexts(vlmc_from_tree, type = "data.frame", frequency = "detailed", cutoff = "native", positions = TRUE),
    contexts(vlmc_direct, type = "data.frame", frequency = "detailed", cutoff = "native", positions = TRUE)
  )
  expect_equal(
    loglikelihood(vlmc_from_tree),
    loglikelihood(vlmc_direct)
  )
  expect_equal(
    loglikelihood(vlmc_from_tree, data_set$x, initial = "extended"),
    loglikelihood(vlmc_direct, data_set$x, initial = "extended")
  )
})

test_that("as_vlmc.ctx_tree rejects wrong parameters", {
  data_set <- build_markov_chain(100, 4, seed = 4)
  data_tree <- ctx_tree(data_set$x,
    min_size = 4, max_depth = 10,
    keep_position = FALSE
  )
  expect_error(as_vlmc(data_tree, alpha = "a"))
  expect_error(as_vlmc(data_tree, alpha = -1))
  expect_error(as_vlmc(data_tree, alpha = 1.5))
  expect_error(as_vlmc(data_tree, cutoff = -0.1))
  expect_error(as_vlmc(data_tree, cutoff = TRUE))
})

test_that("as_vlmc.ctx_tree applies pruning", {
  data_set <- build_markov_chain(1000, 4, seed = 4)
  data_tree <- ctx_tree(data_set$x,
    min_size = 4, max_depth = 10,
    keep_position = FALSE
  )
  vlmc_from_tree <- as_vlmc(data_tree, alpha = 0.1)
  expect_true(is_vlmc(vlmc_from_tree))
  vlmc_direct <- vlmc(data_set$x, min_size = 4, max_depth = 10, alpha = 0.1)
  expect_equal(
    contexts(vlmc_from_tree, type = "data.frame", frequency = "detailed", cutoff = "native"),
    contexts(vlmc_direct, type = "data.frame", frequency = "detailed", cutoff = "native")
  )
  expect_equal(
    loglikelihood(vlmc_from_tree),
    loglikelihood(vlmc_direct)
  )
  expect_equal(
    loglikelihood(vlmc_from_tree, data_set$x, initial = "extended"),
    loglikelihood(vlmc_direct, data_set$x, initial = "extended")
  )
  vlmc_from_tree <- as_vlmc(data_tree, cutoff = 2)
  expect_true(is_vlmc(vlmc_from_tree))
  vlmc_direct <- vlmc(data_set$x, min_size = 4, max_depth = 10, cutoff = 2)
  expect_equal(
    contexts(vlmc_from_tree, type = "data.frame", frequency = "detailed", cutoff = "native"),
    contexts(vlmc_direct, type = "data.frame", frequency = "detailed", cutoff = "native")
  )
  expect_equal(
    loglikelihood(vlmc_from_tree),
    loglikelihood(vlmc_direct)
  )
  expect_equal(
    loglikelihood(vlmc_from_tree, data_set$x, initial = "extended"),
    loglikelihood(vlmc_direct, data_set$x, initial = "extended")
  )
})

test_that("as_vlmc.tune_vlmc obeys is basic contract", {
  data_set <- build_markov_chain(1000, 4, seed = 4)
  tune_results <- tune_vlmc(data_set$x)
  model <- as_vlmc(tune_results)
  expect_true(is_vlmc(model))
})
