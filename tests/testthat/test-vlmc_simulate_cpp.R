test_that("vlmc simulation generates a consistent sample (C++)", {
  for (k in 2:4) {
    data_set <- build_markov_chain(1000, k, seed = k)
    x_vlmc <- vlmc(data_set$x, backend = "C++")
    xs <- simulate(x_vlmc, 50, seed = 2 * k + 1, sample = "fast")
    expect_equal(length(xs), 50)
    expect_identical(sort(unique(xs)), states(x_vlmc))
  }
})

test_that("vlmc simulation generates always the same sample with the same seed (C++)", {
  for (k in 2:4) {
    data_set <- build_markov_chain(1000, k, seed = k)
    x_vlmc <- vlmc(data_set$x, backend = "C++")
    xs <- simulate(x_vlmc, 50, seed = 2 * k + 1, sample = "fast")
    xs2 <- simulate(x_vlmc, 50, seed = 2 * k + 1, sample = "fast")
    expect_identical(xs2, xs)
  }
})

test_that("vlmc simulates uses correctly the initial values (C++)", {
  for (k in 2:4) {
    data_set <- build_markov_chain(1000, k, seed = k)
    x_vlmc <- vlmc(data_set$x, alpha = 0.01, backend = "C++")
    init <- sample(states(x_vlmc), 2 * k, replace = TRUE)
    xs <- simulate(x_vlmc, 100, init = init, sample = "fast")
    expect_identical(xs[1:length(init)], init)
  }
})

test_that("vlmc simulate detects unadapted init values (C++)", {
  for (k in 2:4) {
    data_set <- build_markov_chain(1000, k, seed = k)
    x_vlmc <- vlmc(data_set$x, alpha = 0.01, backend = "C++")
    expect_error(simulate(x_vlmc, nsim = 10, init = sample(x_vlmc$vals, 11, replace = TRUE)))
    expect_error(simulate(x_vlmc, nsim = 10, init = c(1.0, 2.0)))
  }
  dts <- sample(c("A", "B", "C"), 100, replace = TRUE)
  model <- vlmc(dts, alpha = 0.5)
  expect_error(simulate(model, nsim = 4, init = c("A", "D")))
})

test_that("vlmc simulate supports zero depth model (C++)", {
  withr::local_seed(0)
  data_set <- sample(1:5, 50, replace = TRUE)
  d_vlmc <- vlmc(data_set, backend = "C++")
  expect_equal(depth(d_vlmc), 0L)
  expect_equal(length(simulate(d_vlmc, 50, sample = "fast")), 50)
})

test_that("vlmc simulate respects the type of the data (C++)", {
  all_data <- list(
    int_data = sample(1:5, 200, replace = TRUE),
    float_data = sample(c(1.5, 3.5, -2.4), 200, replace = TRUE),
    chr_data = sample(c("A", "B", "C"), 200, replace = TRUE),
    fct_data = as.factor(sample(c("A", "B", "C"), 200, replace = TRUE)),
    lgl_data = sample(c(TRUE, FALSE), 200, replace = TRUE)
  )
  for (data in all_data) {
    d_model <- vlmc(data, backend = "C++")
    expect_type(simulate(d_model, 50, sample = "fast"), typeof(data))
  }
})

test_that("vlmc simulate results do not depend on the backend (for the R method)", {
  for (k in 2:10) {
    data_set <- build_markov_chain(5000, k, seed = k)
    cpp_vlmc <- vlmc(data_set$x, alpha = 0.1, backend = "C++")
    r_vlmc <- vlmc(data_set$x, alpha = 0.1, backend = "R")
    expect_identical(
      simulate(cpp_vlmc, 1000, seed = k, sample = "R"),
      simulate(r_vlmc, 1000, seed = k)
    )
  }
})

test_that("vlmc simulate results do not depend on the backend for the slow method in simple cases", {
  skip_on_cran()
  skip_on_ci()
  for (k in 2:4) {
    data_set <- build_markov_chain(1000, k, seed = k)
    cpp_vlmc <- tune_vlmc(data_set$x, backend = "C++")
    r_vlmc <- tune_vlmc(data_set$x, backend = "R")
    expect_identical(
      simulate(as_vlmc(cpp_vlmc), 1000, seed = k, sample = "slow"),
      simulate(as_vlmc(r_vlmc), 1000, seed = k)
    )
  }
})


