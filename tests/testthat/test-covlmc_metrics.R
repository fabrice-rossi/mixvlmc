test_that("metrics.covlmc obey its contract", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
  m_cov <- covlmc(rdts, rdts_cov, min_size = 4 / 3, keep_data = TRUE, alpha = 0.5)
  m_metrics <- metrics(m_cov)
  ## class
  expect_s3_class(m_metrics, "metrics.covlmc")
  ## names
  expect_true(all(c("accuracy", "conf_mat", "auc") %in% names(m_metrics)))
  ## confusion matrix
  expect_true(inherits(m_metrics$conf_mat, "table"))
  expect_equal(dim(m_metrics$conf_mat), c(length(states(m_cov)), length(states(m_cov))))
  expect_equal(colnames(m_metrics$conf_mat), as.character(states(m_cov)))
  expect_equal(rownames(m_metrics$conf_mat), as.character(states(m_cov)))
  expect_lte(sum(m_metrics$conf_mat), length(rdts))
  expect_true(all(colSums(m_metrics$conf_mat) <= table(rdts)))
})

test_that("metrics.covlmc objects print as expected without AUC", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  rdts <- cut(pc$active_power, breaks = c(0, 0.4, 2, 8), labels = c("low", "typical", "high"))
  rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
  m_cov <- covlmc(rdts, rdts_cov, min_size = 2, keep_data = TRUE, alpha = 0.5)
  m_metrics <- metrics(m_cov)
  ## AUC depends on the underlying blas/lapack
  ## we round it to prevent the test from failing
  m_metrics$auc <- signif(m_metrics$auc, 1)
  expect_snapshot(print(m_metrics))
})

test_that("metrics.covlmc objects print as expected", {
  skip_on_ci()
  skip_if(
    !grepl("openblas-openmp", sessionInfo()$BLAS),
    "Numeric instability prevents testing in this configuration"
  )
  pc <- powerconsumption[powerconsumption$week == 5, ]
  rdts <- cut(pc$active_power, breaks = c(0, 0.4, 2, 8), labels = c("low", "typical", "high"))
  rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
  m_cov <- covlmc(rdts, rdts_cov, min_size = 2, keep_data = TRUE, alpha = 0.5)
  m_metrics <- metrics(m_cov)
  expect_snapshot(print(m_metrics))
})

test_that("metrics.covlmc works as expected on two state chains", {
  pc <- powerconsumption[powerconsumption$week %in% 5:7, ]
  rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
  rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
  m_cov <- covlmc(rdts, rdts_cov, min_size = 10, keep_data = TRUE)
  m_metrics <- metrics(m_cov)
  ## class
  expect_s3_class(m_metrics, "metrics.covlmc")
  ## names
  expect_true(all(c("accuracy", "conf_mat", "auc") %in% names(m_metrics)))
  ## confusion matrix
  expect_true(inherits(m_metrics$conf_mat, "table"))
  expect_equal(dim(m_metrics$conf_mat), c(length(states(m_cov)), length(states(m_cov))))
  expect_equal(colnames(m_metrics$conf_mat), as.character(states(m_cov)))
  expect_equal(rownames(m_metrics$conf_mat), as.character(states(m_cov)))
  expect_lte(sum(m_metrics$conf_mat), length(rdts))
  expect_true(all(colSums(m_metrics$conf_mat) <= table(rdts)))
  m_predict <- predict(m_cov, rdts, rdts_cov, final_pred = FALSE)
  predict_table <- table(m_predict, rdts)
  names(dimnames(predict_table)) <- c("predicted value", "true value")
  expect_identical(m_metrics$conf_mat, predict_table)
})

test_that("metrics.covlmc works as expected on degenerate models", {
  d_model <- build_degenerate_elec_model(FALSE)
  expect_no_error(metrics(d_model$model))
})

test_that("metrics.covlmc and predict.covlmc return consistent results", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  rdts <- cut(pc$active_power, breaks = c(0, 0.4, 2, 8), labels = c("low", "typical", "high"))
  rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
  m_cov <- covlmc(rdts, rdts_cov, min_size = 2, keep_data = TRUE, alpha = 0.5)
  m_metrics <- metrics(m_cov)
  m_predict <- predict(m_cov, rdts, rdts_cov, final_pred = FALSE)
  predict_table <- table(m_predict, rdts)
  names(dimnames(predict_table)) <- c("predicted value", "true value")
  expect_identical(m_metrics$conf_mat, predict_table)
})

test_that("metrics.covlmc works with constant models and more than 2 states", {
  withr::local_seed(0)
  x <- rep(c("a", "b", "c", "c"), 100)
  x_cov <- data.frame(y = rnorm(length(x)))
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    m_cov <- covlmc(x, x_cov)
    expect_true(any(sapply(contexts(m_cov), function(ctx) {
      inherits(model(ctx, type = "full"), "constant_model")
    })))
    expect_no_error(m_metrics <- metrics(m_cov))
    m_predict <- predict(m_cov, x, x_cov, final_pred = FALSE)
    predict_table <- table(factor(m_predict, levels = states(m_cov)), x)
    names(dimnames(predict_table)) <- c("predicted value", "true value")
    expect_identical(m_metrics$conf_mat, predict_table)
  }
})

test_that("metrics.covlmc works on depth 0 models", {
  pc <- powerconsumption[powerconsumption$week %in% 6:7, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
  dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
  m_cov <- covlmc(dts, dts_cov, min_size = 1e5)
  expect_equal(depth(m_cov), 0)
  expect_no_error(m_metrics <- metrics(m_cov))
  m_predict <- predict(m_cov, dts, dts_cov, final_pred = FALSE)
  predict_table <- table(m_predict, dts)
  names(dimnames(predict_table)) <- c("predicted value", "true value")
  expect_identical(m_metrics$conf_mat, predict_table)
})

test_that("metrics of trimmed models are consistent with the ones of the models", {
  pc <- powerconsumption[powerconsumption$week %in% 5:6, ]
  cov <- data.frame(day = (pc$hour >= 7 & pc$hour <= 17))
  series <- list(
    cut(pc$active_power, breaks = c(0, 0.4, 2, 8)),
    cut(pc$active_power, breaks = c(0, 1.2, 8))
  )
  for (x in series) {
    for (engine in c("glm", "multinom")) {
      withr::local_options(mixvlmc.predictive = engine)
      model <- covlmc(x, cov, min_size = 5)
      expect_gt(depth(model), 0)
      ref <- metrics(model)
      for (keep_model in c(FALSE, TRUE)) {
        trimmed <- trim(model, keep_model = keep_model)
        expect_warning(t_metrics <- metrics(trimmed), "AUC")
        expect_identical(t_metrics$auc, NA_real_)
        expect_equal(t_metrics$accuracy, ref$accuracy)
        expect_equal(
          unclass(t_metrics$conf_mat), unclass(ref$conf_mat),
          ignore_attr = TRUE
        )
        expect_identical(dimnames(t_metrics$conf_mat), dimnames(ref$conf_mat))
      }
    }
  }
})

test_that("metrics of trimmed models support character time series", {
  withr::local_seed(0)
  x <- sample(c("A", "B", "C"), 500, replace = TRUE)
  cov <- data.frame(z = stats::runif(length(x)))
  model <- covlmc(x, cov, alpha = 0.5)
  expect_gt(depth(model), 0)
  ref <- metrics(model)
  expect_warning(t_metrics <- metrics(trim(model)), "AUC")
  expect_equal(t_metrics$accuracy, ref$accuracy)
  expect_equal(unclass(t_metrics$conf_mat), unclass(ref$conf_mat), ignore_attr = TRUE)
})
