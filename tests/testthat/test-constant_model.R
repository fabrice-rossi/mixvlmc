test_that("constant model gives the expected predictions on the learning set", {
  covariates <- data.frame(
    x = as.factor(sample(c("a", "b"), 100, replace = TRUE)),
    y = rnorm(100),
    z = as.factor(sample(1:4, 100, replace = TRUE))
  )
  target <- factor(rep("2", 100), levels = as.character(1:3))
  model <- constant_model(target, covariates, 3)
  learn_probs <- predict(model)
  expect_equal(dim(learn_probs), c(100L, 3L))
  expect_equal(apply(learn_probs, 1, which.max), rep(2L, length(target)))
  expect_equal(nrow(unique(learn_probs)), 1)

  bin_target <- rep(1, 100)
  bin_model <- constant_model(bin_target, covariates, 2)
  bin_learn_probs <- predict(bin_model)
  expect_vector(bin_learn_probs, ptype = double(), size = 100)
  expect_true(all(bin_learn_probs > 0.5))
  expect_length(unique(bin_learn_probs), 1)
})

test_that("constant model gives the expected predictions on the test set", {
  covariates <- data.frame(
    x = as.factor(sample(c("a", "b"), 100, replace = TRUE)),
    y = rnorm(100),
    z = as.factor(sample(1:4, 100, replace = TRUE))
  )
  target <- factor(rep("2", 100), levels = as.character(1:3))
  model <- constant_model(target, covariates, 3)
  new_covariates <- data.frame(
    x = as.factor(sample(c("a", "b"), 200, replace = TRUE)),
    y = rnorm(200),
    z = as.factor(sample(1:4, 200, replace = TRUE))
  )
  learn_probs <- predict(model)
  test_probs <- predict(model, new_covariates)
  expect_equal(dim(test_probs), c(200L, 3L))
  expect_equal(test_probs[1, ], learn_probs[1, ])
  expect_equal(nrow(unique(test_probs)), 1)
  new_covariates$u <- rnorm(nrow(new_covariates))
  expect_error(predict(model, new_covariates), NA)
  broken_new_covariates <- new_covariates
  broken_new_covariates$x <- sample(1:4, 200, replace = TRUE)
  expect_error(predict(model, broken_new_covariates))

  new_covariates$u <- NULL
  bin_target <- rep(1, 100)
  bin_model <- constant_model(bin_target, covariates, 2)
  bin_learn_probs <- predict(bin_model)
  bin_test_probs <- predict(bin_model, new_covariates)
  expect_vector(bin_test_probs, ptype = double(), size = 200)
  expect_equal(bin_test_probs[1], bin_learn_probs[1])
  expect_length(unique(bin_test_probs), 1)
  new_covariates$u <- rnorm(nrow(new_covariates))
  expect_error(predict(bin_model, new_covariates), NA)
  broken_new_covariates <- new_covariates
  broken_new_covariates$x <- sample(1:4, 200, replace = TRUE)
  expect_error(predict(bin_model, broken_new_covariates))
})

test_that("constant model metrics works as expected", {
  covariates <- data.frame(
    x = as.factor(sample(c("a", "b"), 100, replace = TRUE)),
    y = rnorm(100),
    z = as.factor(sample(1:4, 100, replace = TRUE))
  )
  target <- factor(rep("2", 100), levels = as.character(1:3))
  model <- constant_model(target, covariates, 3)
  m_model <- glm_metrics(model, covariates, target)
  expect_equal(m_model$accuracy, 1)
  expect_equal(m_model$auc, NA)
  cm <- table(target, target)
  cm_dm <- dimnames(cm)
  names(cm_dm) <- c("predicted value", "true value")
  dimnames(cm) <- cm_dm
  expect_equal(m_model$conf_mat, cm)
})
