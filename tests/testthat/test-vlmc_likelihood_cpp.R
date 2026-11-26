test_that("the likelihood calculations in logLik give identical results for R and C++ backends", {
  withr::local_seed(0)
  x <- sample(c("A", "B", "C"), 5000, replace = TRUE)
  r_vlmc <- vlmc(x, alpha = 0.05, backend = "R")
  cpp_vlmc <- vlmc(x, alpha = 0.05, backend = "C++")
  for (initial in c("truncated", "specific", "extended")) {
    expect_equal(logLik(r_vlmc, initial = initial), logLik(cpp_vlmc, initial = initial))
  }
  data_set <- build_markov_chain(5000, 3, seed = 0)
  r_vlmc <- vlmc(data_set$x, backend = "R")
  cpp_vlmc <- vlmc(data_set$x, backend = "C++")
  for (initial in c("truncated", "specific", "extended")) {
    expect_equal(logLik(r_vlmc, initial = initial), logLik(cpp_vlmc, initial = initial))
  }
})

test_that("loglikelihood computes the same values regardless of the backend", {
  skip_on_cran()
  withr::local_seed(0)
  for (k in 1:9) {
    x <- sample(0:k, 5000 + 500 * k, replace = TRUE)
    r_tree <- vlmc(x, alpha = 0.1, backend = "R")
    cpp_tree <- vlmc(x, alpha = 0.1, backend = "C++")
    for (initial in c("truncated", "specific", "extended")) {
      fll <- loglikelihood(cpp_tree, newdata = x, initial = initial)
      sll <- loglikelihood(r_tree, newdata = x, initial = initial)
      expect_equal(fll, sll)
    }
  }
})

test_that("loglikelihood ignores the requested number of observation", {
  withr::local_seed(0)
  for (k in 1:9) {
    x <- sample(0:k, 1000 + 100 * k, replace = TRUE)
    x_tree <- vlmc(x, alpha = 0.1, backend = "C++")
    r_tree <- vlmc(x, alpha = 0.1, backend = "R")
    for (initial in c("truncated", "specific", "extended")) {
      to_ignore <- depth(x_tree) + sample(1:50, 1)
      fll <- loglikelihood(x_tree, newdata = x, initial = initial, ignore = to_ignore)
      sll <- slow_loglikelihood(r_tree, x, initial = initial, ignore = to_ignore)
      expect_equal(as.numeric(fll), as.numeric(sll))
      expect_equal(attr(fll, "nobs"), attr(sll, "nobs"))
    }
  }
})

test_that("C++ loglikelihood supports ignore without new data", {
  x <- rep(c("B", "A", "C", "A", "D", "C", "B", "B", "A", "C"), 50)
  m_r <- vlmc(x, backend = "R")
  m_cpp <- vlmc(x, backend = "C++")
  expect_gte(depth(m_cpp), 3)
  for (ignore in 0:depth(m_cpp)) {
    expect_equal(
      loglikelihood(m_cpp, initial = "extended", ignore = ignore),
      loglikelihood(m_r, initial = "extended", ignore = ignore)
    )
  }
})
