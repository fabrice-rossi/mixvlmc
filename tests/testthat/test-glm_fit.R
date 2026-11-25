test_that("glm contrast error is captured", {
  ## mm has a factor variable with only one active level
  nb_data <- 50
  mm <- data.frame(
    x = rnorm(nb_data),
    z = rep(factor("a", levels = c("a", "b"))), nb_data
  )
  target <- sample(factor(0:1), nb_data, replace = TRUE)
  ## trigger an error with stats::glm
  expect_error(d_model <- stats::glm(target ~ ., data = mm, family = stats::binomial()))
  ## the error is captured by fit_glm
  expect_no_error(model <- fit_glm(target, mm, 2, covlmc_control()))
  ## returns a constant model
  expect_s3_class(model, "constant_model")
  expect_equal(model$rank, 0)
})

test_that("vglm rank deficiency error is captured", {
  withr::local_seed(0)
  withr::local_options(mixvlmc.predictive = "glm")
  nb_data <- 50
  z <- rnorm(nb_data)
  ## collinear covariates
  mm <- data.frame(a = z, b = z)
  target <- factor(sample(c("a", "b", "c"), nb_data, replace = TRUE))
  expect_no_error(model <- fit_glm(target, mm, 3, covlmc_control()))
  ## returns a constant model
  expect_s3_class(model, "constant_model")
  expect_equal(model$rank, 0)
  expect_true(is_glm_low_rank(model))
})

test_that("multinom handles models with many weights", {
  withr::local_seed(0)
  withr::local_options(mixvlmc.predictive = "multinom")
  nb_data <- 300
  ## (250 + 2) * 4 > 1000 weights (nnet default limit)
  mm <- as.data.frame(matrix(rnorm(nb_data * 250), nb_data))
  target <- factor(sample(letters[1:4], nb_data, replace = TRUE))
  expect_no_error(model <- fit_glm(target, mm, 4, covlmc_control()))
  expect_s3_class(model, "multinom")
})

test_that("multinom rank deficiency is detected", {
  withr::local_seed(0)
  withr::local_options(mixvlmc.predictive = "multinom")
  nb_data <- 200
  z <- rnorm(nb_data)
  ## collinear covariates
  mm <- data.frame(a = z, b = 2 * z, c = rnorm(nb_data))
  for (nb_vals in 2:3) {
    target <- factor(sample(letters[1:nb_vals], nb_data, replace = TRUE))
    model <- fit_glm(target, mm, nb_vals, covlmc_control())
    expect_true(is_glm_low_rank(model))
    model <- fit_glm(target, mm[, -1], nb_vals, covlmc_control())
    expect_false(is_glm_low_rank(model))
  }
})

test_that("multinom and glm detect rank deficiency consistently with factors", {
  withr::local_seed(0)
  nb_data <- 200
  target <- sample(0:1, nb_data, replace = TRUE)
  ## a factor with an unused level
  f_unused <- factor(sample(c("a", "b"), nb_data, replace = TRUE), levels = c("a", "b", "c"))
  ## a factor with a single used level
  f_single <- factor(rep("a", nb_data), levels = c("a", "b"))
  for (mm in list(
    data.frame(f = f_unused, z = rnorm(nb_data)),
    data.frame(f = f_single, z = rnorm(nb_data))
  )) {
    low_rank <- sapply(c("glm", "multinom"), function(engine) {
      withr::local_options(mixvlmc.predictive = engine)
      is_glm_low_rank(fit_glm(target, mm, 2, covlmc_control()))
    })
    expect_identical(low_rank[["multinom"]], low_rank[["glm"]])
  }
})
