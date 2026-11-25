test_that("the rank based AUC is the AUC computed by pROC", {
  withr::local_seed(0)
  for (rep in 1:5) {
    ## binary case, with ties
    target <- sample(0:1, 200, replace = TRUE)
    probs <- round(runif(200) * 0.5 + 0.3 * target, 1)
    expect_equal(
      main_metrics(target, probs, roc = FALSE)$auc,
      main_metrics(target, probs)$auc
    )
    f_target <- factor(c("a", "b")[target + 1], levels = c("a", "b"))
    expect_equal(
      main_metrics(f_target, probs, roc = FALSE)$auc,
      main_metrics(f_target, probs)$auc
    )
    ## multiclass case, with ties
    target <- factor(sample(c("a", "b", "c", "d"), 300, replace = TRUE))
    raw <- matrix(round(runif(4 * 300), 1), ncol = 4)
    true_class <- cbind(seq_along(target), as.integer(target))
    raw[true_class] <- raw[true_class] + 0.3
    probs <- raw / rowSums(raw)
    expect_equal(
      main_metrics(target, probs, roc = FALSE)$auc,
      main_metrics(target, probs)$auc
    )
  }
})

test_that("main_metrics returns the same metrics without ROC curves", {
  withr::local_seed(1)
  target <- factor(sample(c("a", "b", "c"), 100, replace = TRUE))
  probs <- matrix(runif(300), ncol = 3)
  probs <- probs / rowSums(probs)
  full <- main_metrics(target, probs)
  light <- main_metrics(target, probs, roc = FALSE)
  expect_null(light$roc)
  full$roc <- NULL
  expect_equal(light, full)
})

test_that("covlmc models do not store ROC curves", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  elec <- cut(pc$active_power, breaks = c(0, 0.4, 2, 8), labels = c("low", "typical", "high"))
  elec_cov <- data.frame(day = (pc$hour >= 7 & pc$hour <= 17))
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    model <- covlmc(elec, elec_cov, min_size = 5)
    ctxs <- contexts(model)
    for (ctx in ctxs) {
      ctx_metrics <- metrics(ctx)
      expect_false("roc" %in% names(ctx_metrics))
      expect_true(is.na(ctx_metrics$auc) || (ctx_metrics$auc >= 0 && ctx_metrics$auc <= 1))
    }
  }
})
