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
    my_seed <- 2 * k + 1
    attr(my_seed, "kind") <- as.list(RNGkind())
    expect_equal(attr(xs, "seed"), my_seed)
    expect_equal(attr(xs2, "seed"), my_seed)
  }
})

test_that("vlmc simulates uses correctly the initial values", {
  for (k in 2:4) {
    data_set <- build_markov_chain(1000, k, seed = k)
    x_vlmc <- vlmc(data_set$x, alpha = 0.01)
    init <- sample(states(x_vlmc), 2 * k, replace = TRUE)
    rng <- .Random.seed
    xs <- simulate(x_vlmc, 100, init = init)
    expect_identical(xs[1:length(init)], init)
    expect_identical(attr(xs, "seed"), rng)
  }
})

test_that("vlmc simulate detects unadapted init values", {
  for (k in 2:4) {
    data_set <- build_markov_chain(1000, k, seed = k)
    x_vlmc <- vlmc(data_set$x, alpha = 0.01)
    expect_error(simulate(x_vlmc, nsim = 10, init = sample(x_vlmc$vals, 11, replace = TRUE)))
    expect_error(simulate(x_vlmc, nsim = 10, init = c(1.0, 2.0)))
  }
  rdts <- sample(c("A", "B", "C"), 100, replace = TRUE)
  model <- vlmc(rdts, alpha = 0.5)
  expect_error(simulate(model, nsim = 4, init = c("A", "D")))
})

test_that("vlmc simulate supports zero depth model", {
  withr::local_seed(0)
  data_set <- sample(1:5, 50, replace = TRUE)
  d_vlmc <- vlmc(data_set)
  expect_equal(depth(d_vlmc), 0L)
  expect_equal(length(simulate(d_vlmc, 50)), 50)
})

test_that("vlmc simulate respects the type of the data", {
  all_data <- list(
    int_data = sample(1:5, 200, replace = TRUE),
    float_data = sample(c(1.5, 3.5, -2.4), 200, replace = TRUE),
    chr_data = sample(c("A", "B", "C"), 200, replace = TRUE),
    fct_data = as.factor(sample(c("A", "B", "C"), 200, replace = TRUE)),
    lgl_data = sample(c(TRUE, FALSE), 200, replace = TRUE)
  )
  for (data in all_data) {
    d_model <- vlmc(data)
    expect_type(simulate(d_model, 50), typeof(data))
  }
})

test_that("vlmc simulate handles correctly burnin", {
  for (k in 2:4) {
    data_set <- build_markov_chain(1000, k, seed = k)
    x_vlmc <- vlmc(data_set$x, alpha = 0.05)
    xs <- simulate(x_vlmc, nsim = 250, seed = 5, burnin = 50)
    expect_length(xs, 250)
    xs_full <- simulate(x_vlmc, nsim = 300, seed = 5, burnin = 0)
    expect_length(xs_full, 300)
    expect_equal(as.vector(xs), xs_full[-(1:50)])
    xs_auto <- simulate(x_vlmc, nsim = 250, seed = 10, burnin = "auto")
    xs_auto_full <- simulate(x_vlmc, nsim = 250 + 64 * context_number(x_vlmc), seed = 10, burnin = 0)
    expect_equal(
      as.vector(xs_auto),
      xs_auto_full[(length(xs_auto_full) - 249):length(xs_auto_full)]
    )
  }
})

test_that("vlmc simulate handles correctly burnin with init values", {
  for (k in 2:4) {
    data_set <- build_markov_chain(1000, k, seed = k)
    x_vlmc <- vlmc(data_set$x, alpha = 0.05)
    xs_init <- sample(states(x_vlmc), 25, replace = TRUE)
    xs <- simulate(x_vlmc, nsim = 250, seed = 5, burnin = 50, init = xs_init)
    expect_length(xs, 250)
    xs_full <- simulate(x_vlmc, nsim = 300, seed = 5, burnin = 0, init = xs_init)
    expect_length(xs_full, 300)
    expect_equal(as.vector(xs), xs_full[-(1:50)])
    expect_equal(xs_full[1:25], xs_init)
  }
})
