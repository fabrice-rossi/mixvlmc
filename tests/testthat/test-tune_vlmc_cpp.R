test_that("tune_vlmc results do not depend on the backend", {
  for (k in 2:4) {
    data_set <- build_markov_chain(1000, k, seed = 3 * k)
    for (initial in c("truncated", "specific", "extended")) {
      r_vlmc <- tune_vlmc(data_set$x, initial = initial, max_depth = 100)
      cpp_vlmc <- tune_vlmc(data_set$x, initial = initial, max_depth = 100, backend = "C++")
      expect_equal(r_vlmc$results, cpp_vlmc$results)
    }
  }
})
