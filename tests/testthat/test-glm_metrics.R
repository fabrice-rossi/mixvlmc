test_that("metrics for glm work as expected", {
  withr::local_seed(0)
  beta <- c(-2, 2)
  x <- matrix(rnorm(500), ncol = 2)
  score <- x %*% beta
  prob <- 1 / (1 + exp(-score[, 1]))
  target <- as.numeric(stats::runif(length(prob)) < prob)
  model <- stats::glm(target ~ ., data = data.frame(x), family = stats::binomial())
  mm <- glm_metrics(model, data.frame(x), target)
  m_probs <- stats::predict(model, type = "response")
  cm <- metrics_fix_names(table(as.numeric(m_probs >= 0.5), target))
  expect_identical(mm$conf_mat, cm)
  expect_equal(mm$accuracy, sum(diag(cm)) / length(target))
  expect_equal(mm$auc, as.numeric(pROC::auc(target, m_probs, levels = c(0, 1), direction = "<")))
})

test_that("metrics for constant_model work as expected", {
  withr::local_seed(0)
  x <- matrix(stats::rnorm(500), ncol = 2)
  x <- data.frame(x)
  target <- rep(1, nrow(x))
  f_target <- factor(target, levels = c(0, 1))
  model <- constant_model(target, x, 2)
  mm <- glm_metrics(model, x, target)
  cm <- metrics_fix_names(table(f_target, f_target))
  expect_identical(mm$conf_mat, cm)
  expect_equal(mm$accuracy, sum(diag(cm)) / length(target))
  expect_equal(mm$auc, as.numeric(NA))

  target2 <- sample(c(0, 1), nrow(x), replace = TRUE)
  mm <- glm_metrics(model, x, target2)
  cm <- metrics_fix_names(table(f_target, target2))
  expect_identical(mm$conf_mat, cm)
  expect_equal(mm$accuracy, sum(diag(cm)) / length(target))
  m_probs <- stats::predict(model, x)
  expect_equal(mm$auc, as.numeric(pROC::auc(target2, m_probs, levels = c(0, 1), direction = "<")))

  target <- factor(rep("a", nrow(x)), levels = c("a", "b", "c"))
  model <- constant_model(target, x, 3)
  mm <- glm_metrics(model, x, target)
  cm <- metrics_fix_names(table(target, target))
  expect_identical(mm$conf_mat, cm)
  expect_equal(mm$accuracy, sum(diag(cm)) / length(target))
  expect_equal(mm$auc, as.numeric(NA))

  target2 <- factor(sample(c("a", "b", "c"), nrow(x), replace = TRUE), levels = c("a", "b", "c"))
  mm <- glm_metrics(model, x, target2)
  cm <- metrics_fix_names(table(target, target2))
  expect_identical(mm$conf_mat, cm)
  expect_equal(mm$accuracy, sum(diag(cm)) / length(target))
  m_probs <- predict(model, x)
  colnames(m_probs) <- levels(target2)
  expect_equal(mm$auc, as.numeric(pROC::auc(pROC::multiclass.roc(target2, m_probs))))
})

test_that("metrics for vglm work as expected", {
  withr::local_seed(0)
  beta1 <- c(-2, 2, 0)
  beta2 <- c(0, 2, -2)
  x <- matrix(stats::rnorm(750), ncol = 3)
  score1 <- (x %*% beta1)[, 1]
  score2 <- (x %*% beta2)[, 1]
  probs <- matrix(0, ncol = 3, nrow = nrow(x))
  probs[, 1] <- 1 / (1 + exp(score1) + exp(score2))
  probs[, 2] <- exp(score1) * probs[, 1]
  probs[, 3] <- exp(score2) * probs[, 1]
  target <- as.factor(apply(probs, 1, function(x) sample(c("a", "b", "c"), 1, prob = x)))
  model <- VGAM::vglm(target ~ ., data = data.frame(x), family = VGAM::multinomial(refLevel = 1))
  mm <- glm_metrics(model, data.frame(x), target)
  m_probs <- predict(model, type = "response")
  cm <- metrics_fix_names(table(apply(m_probs, 1, which.max), target))
  expect_true(all(mm$conf_mat == cm))
  expect_equal(mm$accuracy, sum(diag(cm)) / length(target))
  expect_equal(mm$auc, as.numeric(pROC::auc(pROC::multiclass.roc(target, m_probs))))
})

test_that("metrics for multinom work as expected", {
  withr::local_seed(0)
  beta1 <- c(-2, 2, 0)
  beta2 <- c(0, 2, -2)
  x <- matrix(stats::rnorm(750), ncol = 3)
  score1 <- (x %*% beta1)[, 1]
  score2 <- (x %*% beta2)[, 1]
  probs <- matrix(0, ncol = 3, nrow = nrow(x))
  probs[, 1] <- 1 / (1 + exp(score1) + exp(score2))
  probs[, 2] <- exp(score1) * probs[, 1]
  probs[, 3] <- exp(score2) * probs[, 1]
  target <- as.factor(apply(probs, 1, function(x) sample(c("a", "b", "c"), 1, prob = x)))
  model <- nnet::multinom(target ~ ., data = data.frame(x), trace = FALSE)
  mm <- glm_metrics(model, data.frame(x), target)
  m_probs <- predict(model, type = "probs")
  cm <- metrics_fix_names(table(apply(m_probs, 1, which.max), target))
  expect_true(all(mm$conf_mat == cm))
  expect_equal(mm$accuracy, sum(diag(cm)) / length(target))
  expect_equal(mm$auc, as.numeric(pROC::auc(pROC::multiclass.roc(target, m_probs))))
})
