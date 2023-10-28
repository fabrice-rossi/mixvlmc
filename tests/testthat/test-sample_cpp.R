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
