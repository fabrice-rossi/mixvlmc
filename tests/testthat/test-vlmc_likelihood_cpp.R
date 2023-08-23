test_that("the likelihood calculations in logLik give identical results for R and C++ backends", {
  withr::local_seed(0)
  x <- sample(c("A", "B", "C"), 5000, replace = TRUE)
  r_vlmc <- vlmc(x, alpha = 0.05)
  cpp_vlmc <- vlmc(x, alpha = 0.05, backend = "C++")
  for (initial in c("truncated", "specific", "extended")) {
    expect_equal(logLik(r_vlmc, initial = initial), logLik(cpp_vlmc, initial = initial))
  }
  data_set <- build_markov_chain(5000, 3, seed = 0)
  r_vlmc <- vlmc(data_set$x)
  cpp_vlmc <- vlmc(data_set$x, backend = "C++")
  for (initial in c("truncated", "specific", "extended")) {
    expect_equal(logLik(r_vlmc, initial = initial), logLik(cpp_vlmc, initial = initial))
  }
})

test_that("loglikelihood computes the same values regardless of the backend", {
  withr::local_seed(0)
  for (k in 1:9) {
    x <- sample(0:k, 5000 + 500 * k, replace = TRUE)
    r_tree <- vlmc(x, alpha = 0.1)
    cpp_tree <- vlmc(x, alpha = 0.1, backend = "C++")
    for (initial in c("truncated", "specific", "extended")) {
      fll <- loglikelihood(cpp_tree, newdata = x, initial = initial)
      sll <- loglikelihood(r_tree, newdata = x, initial = initial)
      expect_equal(fll, sll)
    }
  }
})
