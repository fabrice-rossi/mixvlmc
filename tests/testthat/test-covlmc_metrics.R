test_that("metrics.covlmc obey its contract", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
  m_cov <- covlmc(dts, dts_cov, min_size = 4 / 3, keep_data = TRUE, alpha = 0.5)
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
  expect_lte(sum(m_metrics$conf_mat), length(dts))
  expect_true(all(colSums(m_metrics$conf_mat) <= table(dts)))
})

test_that("metrics.covlmc objects print as expected without AUC", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, 0.4, 2, 8), labels = c("low", "typical", "high"))
  dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
  m_cov <- covlmc(dts, dts_cov, min_size = 2, keep_data = TRUE, alpha = 0.5)
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
  dts <- cut(pc$active_power, breaks = c(0, 0.4, 2, 8), labels = c("low", "typical", "high"))
  dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
  m_cov <- covlmc(dts, dts_cov, min_size = 2, keep_data = TRUE, alpha = 0.5)
  m_metrics <- metrics(m_cov)
  expect_snapshot(print(m_metrics))
})

test_that("metrics.covlmc works as expected on two state chains", {
  pc <- powerconsumption[powerconsumption$week %in% 5:7, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
  dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
  m_cov <- covlmc(dts, dts_cov, min_size = 10, keep_data = TRUE)
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
  expect_lte(sum(m_metrics$conf_mat), length(dts))
  expect_true(all(colSums(m_metrics$conf_mat) <= table(dts)))
  m_predict <- predict(m_cov, dts, dts_cov, final_pred = FALSE)
  predict_table <- table(m_predict, dts)
  names(dimnames(predict_table)) <- c("predicted value", "true value")
  expect_identical(m_metrics$conf_mat, predict_table)
})

test_that("metrics.covlmc works as expected on degenerate models", {
  d_model <- build_degenerate_elec_model(FALSE)
  expect_no_error(metrics(d_model$model))
})

test_that("metrics.covlmc and predict.covlmc return consistent results", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, 0.4, 2, 8), labels = c("low", "typical", "high"))
  dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
  m_cov <- covlmc(dts, dts_cov, min_size = 2, keep_data = TRUE, alpha = 0.5)
  m_metrics <- metrics(m_cov)
  m_predict <- predict(m_cov, dts, dts_cov, final_pred = FALSE)
  predict_table <- table(m_predict, dts)
  names(dimnames(predict_table)) <- c("predicted value", "true value")
  expect_identical(m_metrics$conf_mat, predict_table)
})
