test_that("tune_vlmc results do not depend on the backend", {
  for (k in 2:4) {
    data_set <- build_markov_chain(1000, k, seed = 3 * k)
    for (initial in c("truncated", "specific", "extended")) {
      r_vlmc <- tune_vlmc(data_set$x, initial = initial, max_depth = 100)
      cpp_vlmc <- tune_vlmc(data_set$x,
        initial = initial, max_depth = 100,
        backend = "C++"
      )
      expect_equal(r_vlmc$results, cpp_vlmc$results)
    }
  }
})

test_that("tune_vlmc increases max_depth with both backends", {
  x <- rep(c("B", "A", "C", "A", "D", "C", "B", "B", "A", "C"), 50)
  for (initial in c("truncated", "extended")) {
    t_r <- tune_vlmc(x, max_depth = 2, initial = initial)
    t_cpp <- tune_vlmc(x, max_depth = 2, initial = initial, backend = "C++")
    expect_gt(max(t_r$results$depth), 2)
    expect_equal(t_cpp$results, t_r$results)
    expect_equal(depth(as_vlmc(t_cpp)), depth(as_vlmc(t_r)))
  }
})
