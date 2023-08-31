test_that("covlmc simulation generates a consistent sample", {
  data_set <- build_data_set(500, seed = 0)
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    model <- covlmc(data_set$x, data_set$covariate, alpha = 0.2)
    xs <- simulate(model, 250, covariate = data_set$covariate[1:250, , drop = FALSE], seed = 1)
    expect_equal(length(xs), 250)
    expect_identical(sort(unique(xs)), states(model))
  }
  withr::local_seed(0)
  x <- sample(c("A", "B", "C"), 500, replace = TRUE)
  y <- ifelse(runif(length(x)) > 0.5, c(x[-1], sample(c("A", "B", "C"), 1)), c(x[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y <- as.factor(ifelse(runif(length(x)) > 0.2, y, sample(c("A", "B", "C"), 500, replace = TRUE)))
  df_y <- data.frame(y = y, z = runif(length(y)))
  new_cov <- df_y[sample(1:nrow(df_y), 250, replace = TRUE), ]
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    model <- covlmc(x, df_y, alpha = 1e-8)
    xs <- simulate(model, 250, covariate = new_cov, seed = 1)
    expect_equal(length(xs), 250)
    expect_identical(sort(unique(xs)), states(model))
  }
})

test_that("covlmc simulation generates always the same sample with the same seed", {
  for (k in 1:4) {
    data_set <- build_data_set(250, seed = k)
    for (engine in c("glm", "multinom")) {
      withr::local_options(mixvlmc.predictive = engine)
      model <- covlmc(data_set$x, data_set$covariate, alpha = 0.1)
      xs <- simulate(model, 250, seed = 2 * k + 1, covariate = data_set$covariate[1:250, , drop = FALSE])
      xs2 <- simulate(model, 250, seed = 2 * k + 1, covariate = data_set$covariate[1:250, , drop = FALSE])
      expect_identical(xs2, xs)
    }
  }
})

test_that("covlmc simulates uses correctly the initial values", {
  data_set <- build_data_set(500, seed = 0)
  model <- covlmc(data_set$x, data_set$covariate, alpha = 0.2)
  init <- sample(states(model), 10, replace = TRUE)
  xs <- simulate(model, 100, covariate = data_set$covariate[1:250, , drop = FALSE], init = init)
  expect_identical(xs[1:length(init)], init)
  withr::local_seed(0)
  x <- sample(c("A", "B", "C"), 500, replace = TRUE)
  y <- ifelse(runif(length(x)) > 0.5, c(x[-1], sample(c("A", "B", "C"), 1)), c(x[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y <- as.factor(ifelse(runif(length(x)) > 0.2, y, sample(c("A", "B", "C"), 500, replace = TRUE)))
  df_y <- data.frame(y = y, z = runif(length(y)))
  new_cov <- df_y[sample(1:nrow(df_y), 250, replace = TRUE), ]
  model <- covlmc(x, df_y, alpha = 1e-8)
  init <- sample(states(model), 15, replace = TRUE)
  xs <- simulate(model, 250, covariate = new_cov, init = init)
  expect_identical(xs[1:length(init)], init)
})

test_that("covlmc simulate detects unadapted init values", {
  data_set <- build_data_set(500, seed = 0)
  model <- covlmc(data_set$x, data_set$covariate, alpha = 0.2)
  expect_error(simulate(model, 100, covariate = data_set$covariate[1:250, , drop = FALSE], init = c(0L, 1L)))
  withr::local_seed(0)
  x <- sample(c("A", "B", "C"), 500, replace = TRUE)
  y <- ifelse(runif(length(x)) > 0.5, c(x[-1], sample(c("A", "B", "C"), 1)), c(x[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y <- as.factor(ifelse(runif(length(x)) > 0.2, y, sample(c("A", "B", "C"), 500, replace = TRUE)))
  df_y <- data.frame(y = y, z = runif(length(y)))
  new_cov <- df_y[sample(1:nrow(df_y), 250, replace = TRUE), ]
  model <- covlmc(x, df_y, alpha = 1e-8)
  expect_error(simulate(model, 250, covariate = new_cov, init = c("A", "D")))
  init <- sample(states(model), 15, replace = TRUE)
  expect_error(simulate(model, 10, covariate = new_cov, init = init))
})

test_that("covlmc simulate handles missing factors in subsets", {
  withr::local_seed(0)
  x <- sample(c(0, 1), 200, replace = TRUE)
  xl1 <- forward_match_all_ctx_counts(x, 2)
  xl2_0 <- forward_match_all_ctx_counts(x, 2, 1, xl1$positions[[1]])
  xl2_1 <- forward_match_all_ctx_counts(x, 2, 1, xl1$positions[[2]])
  y <- rep(1, length(x))
  y[xl2_0$positions[[1]] + 1] <- sample(2:4, length(xl2_0$positions[[1]]), replace = TRUE)
  y[xl2_0$positions[[2]] + 1] <- sample(c(1, 3:4), length(xl2_0$positions[[2]]), replace = TRUE)
  y[xl2_1$positions[[1]] + 1] <- sample(c(1:2, 4), length(xl2_1$positions[[1]]), replace = TRUE)
  y[xl2_1$positions[[2]] + 1] <- sample(1:3, length(xl2_1$positions[[2]]), replace = TRUE)
  y <- as.factor(y)
  z <- runif(length(x)) + c(x[-1], 0) / 4
  dts_cov <- data.frame(y = y, z = z)
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    m_cov <- covlmc(x = x, covariate = dts_cov, min_size = 5, alpha = 0.5)
    expect_error(simulate(m_cov, 100, seed = 0, covariate = dts_cov), regexp = NA)
  }
})

test_that("covlmc simulate detects new levels in factors", {
  data <- build_data_set_2(0)
  model <- covlmc(data$x, data$covariate, min_size = 5, alpha = 0.1)
  new_cov <- data$covariate
  new_cov$y <- as.integer(new_cov$y)
  new_cov$y[1] <- 5
  expect_error(simulate(model, 100, covariate = new_cov), regexp = "Factor y has new level 5")
  new_cov$y[2] <- 6
  expect_error(simulate(model, 100, covariate = new_cov), regexp = "Factor y has new levels 5, 6")
})

test_that("covlmc simulate works correctly with degenerate models", {
  d_model <- build_degenerate_elec_model(TRUE)
  expect_no_error(result <- simulate(d_model$model,
    nsim = nrow(d_model$new_cov),
    covariate = d_model$new_cov, seed = 0
  ))
  expect_equal(length(result), nrow(d_model$new_cov))
})
