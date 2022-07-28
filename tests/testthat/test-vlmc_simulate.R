test_that("vlmc simulation generates a consistent sample", {
  for (k in 2:4) {
    data_set <- build_markov_chain(1000, k, seed = k)
    x_vlmc <- vlmc(data_set$x)
    xs <- simulate(x_vlmc, 50, seed = 2 * k + 1)
    expect_equal(length(xs), 50)
    expect_identical(sort(unique(xs)), states(x_vlmc))
  }
})

test_that("vlmc simulation generates always the same sample with the same seed", {
  for (k in 2:4) {
    data_set <- build_markov_chain(1000, k, seed = k)
    x_vlmc <- vlmc(data_set$x)
    xs <- simulate(x_vlmc, 50, seed = 2 * k + 1)
    xs2 <- simulate(x_vlmc, 50, seed = 2 * k + 1)
    expect_identical(xs2, xs)
  }
})
