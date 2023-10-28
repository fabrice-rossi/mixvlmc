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
