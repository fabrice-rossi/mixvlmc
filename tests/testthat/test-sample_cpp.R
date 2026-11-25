test_that("mixvlmc_sample2 gives identical results as sample in unbalanced cases", {
  for (k in 1:9) {
    withr::local_seed(k)
    base <- 0:k
    probs <- 0.1 + runif(k + 1, max = 0.9)
    freq <- as.integer(1000 * probs / sum(probs))
    withr::local_seed(0)
    sp_cpp <- mixvlmc_sample2(freq, 10000)
    withr::local_seed(0)
    sp_r <- sample(base, 10000, replace = TRUE, prob = freq)
    expect_equal(sp_cpp, sp_r)
  }
})

test_that("mixvlmc_sample2 gives different results as sample in balanced cases", {
  for (k in 1:9) {
    base <- 0:k
    freq <- rep(1, k + 1)
    withr::local_seed(0)
    sp_cpp <- mixvlmc_sample2(freq, 10000)
    withr::local_seed(0)
    sp_r <- sample(base, 10000, replace = TRUE, prob = freq)
    expect_false(identical(sp_cpp, sp_r))
  }
})

test_that("mixvlmc_sample considers the states in increasing order", {
  ## the result must not depend on the iteration order of the C++ containers
  for (k in 1:9) {
    withr::local_seed(k)
    freq <- sample(0:20, k + 1, replace = TRUE)
    freq[sample.int(k + 1, 1)] <- 21L
    withr::local_seed(0)
    sp_cpp <- mixvlmc_sample(freq, 1000)
    withr::local_seed(0)
    u <- runif(1000)
    sp_ref <- vapply(u, function(v) which(cumsum(freq) >= sum(freq) * v)[1] - 1L, 1L)
    expect_equal(sp_cpp, sp_ref)
  }
})
