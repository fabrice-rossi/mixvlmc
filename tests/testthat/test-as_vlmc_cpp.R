test_that("as_vlmc.ctx_tree produces a valid vlmc object", {
  data_set <- build_markov_chain(1000, 4, seed = 4)
  data_tree <- ctx_tree(data_set$x, min_size = 4, max_depth = 10, backend = "C++")
  vlmc_from_tree <- as_vlmc(data_tree)
  expect_named(vlmc_from_tree, c(
    "root", "max_depth", "vals", "depth",
    "nb_ctx", "alpha", "cutoff", "ix", "extended_ll",
    "keep_match", "data_size"
  ), ignore.order = TRUE)
  ## degenerate case should _not_ add match
  withr::local_seed(0)
  data_set <- sample(c("A", "B", "C"), 500, replace = TRUE)
  data_tree <- ctx_tree(data_set, min_size = 4, max_depth = 10, backend = "C++")
  vlmc_from_tree <- as_vlmc(data_tree, alpha = 0.01)
  expect_named(vlmc_from_tree, c(
    "root", "vals", "depth", "max_depth",
    "nb_ctx", "alpha", "cutoff", "extended_ll",
    "keep_match", "data_size"
  ), ignore.order = TRUE)
})

test_that("as_vlmc.ctx_tree does not induce memory problems", {
  withr::local_seed(0)
  data_set <- sample(c("A", "B", "C"), 500, replace = TRUE)
  data_tree <- ctx_tree(data_set, min_size = 4, max_depth = 10, backend = "C++")
  vlmc_from_tree <- as_vlmc(data_tree, alpha = 0.01)
  rm(data_tree)
  gc()
  expect_no_error(contexts(vlmc_from_tree,
    frequency = "detailed", positions = TRUE,
    cutoff = "native", metrics = TRUE
  ))
  data_tree <- ctx_tree(data_set, min_size = 4, max_depth = 10, backend = "C++")
  vlmc_from_tree <- as_vlmc(data_tree, alpha = 0.01)
  rm(vlmc_from_tree)
  gc()
  expect_no_error(contexts(data_tree,
    frequency = "detailed", positions = TRUE
  ))
})

test_that("as_vlmc.ctx_tree_cpp obeys is basic contract", {
  data_set <- build_markov_chain(1000, 4, seed = 4)
  data_tree <- ctx_tree(data_set$x, min_size = 4, max_depth = 10, backend = "C++")
  vlmc_from_tree <- as_vlmc(data_tree)
  expect_true(is_vlmc(vlmc_from_tree))
  vlmc_direct <- vlmc(data_set$x,
    min_size = 4, max_depth = 10, cutoff = 0,
    keep_match = TRUE,
    backend = "C++"
  )
  expect_equal(
    contexts(vlmc_from_tree, frequency = "detailed", cutoff = "native", positions = TRUE, metrics = TRUE),
    contexts(vlmc_direct, frequency = "detailed", cutoff = "native", positions = TRUE, metrics = TRUE)
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
    keep_position = FALSE,
    backend = "C++"
  )
  expect_error(as_vlmc(data_tree, alpha = "a"))
  expect_error(as_vlmc(data_tree, alpha = -1))
  expect_error(as_vlmc(data_tree, alpha = 1.5))
  expect_error(as_vlmc(data_tree, cutoff = -0.1))
  expect_error(as_vlmc(data_tree, cutoff = TRUE))
})

test_that("as_vlmc.ctx_tree_cpp applies pruning", {
  data_set <- build_markov_chain(1000, 4, seed = 4)
  data_tree <- ctx_tree(data_set$x,
    min_size = 4, max_depth = 10,
    keep_position = FALSE,
    backend = "C++"
  )
  vlmc_from_tree <- as_vlmc(data_tree, alpha = 0.1)
  expect_true(is_vlmc(vlmc_from_tree))
  vlmc_direct <- vlmc(data_set$x,
    min_size = 4, max_depth = 10, alpha = 0.1,
    backend = "C++"
  )
  expect_true(
    compare_ctx(
      contexts(vlmc_from_tree, frequency = "detailed", cutoff = "native", metrics = TRUE),
      contexts(vlmc_direct, frequency = "detailed", cutoff = "native", metrics = TRUE)
    )
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
  vlmc_direct <- vlmc(data_set$x, min_size = 4, max_depth = 10, cutoff = 2, backend = "C++")
  expect_true(
    compare_ctx(
      contexts(vlmc_from_tree, frequency = "detailed", cutoff = "native", metrics = TRUE),
      contexts(vlmc_direct, frequency = "detailed", cutoff = "native", metrics = TRUE)
    )
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
