test_that("tune_multi_vlmc selects the expected model", {
  TM <- build_transition_matrix(3, seed = 0)
  mdts <- vector(length = 15, mode = "list")
  for (k in 1:15) {
    mdts[[k]] <- simulate_markov_chain(100, TM, seed = k)
  }
  bt_vlmc <- tune_multi_vlmc(mdts, initial = "truncated", criterion = "BIC")
  best_model <- as_vlmc(bt_vlmc)
  expect_equal(depth(best_model), 1)
  expect_equal(context_number(best_model), 3)
})
