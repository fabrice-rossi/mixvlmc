test_that("metrics.vlmc obey its contract", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  model <- vlmc(dts, alpha = 0.1)
  m_metrics <- metrics(model)
  ## class
  expect_s3_class(m_metrics, "metrics.vlmc")
  ## names
  expect_true(all(c("accuracy", "conf_mat", "auc") %in% names(m_metrics)))
  ## confusion matrix
  expect_true(inherits(m_metrics$conf_mat, "table"))
  expect_equal(dim(m_metrics$conf_mat), c(length(states(model)), length(states(model))))
  expect_equal(colnames(m_metrics$conf_mat), as.character(states(model)))
  expect_equal(rownames(m_metrics$conf_mat), as.character(states(model)))
  expect_lte(sum(m_metrics$conf_mat), length(dts))
  expect_true(all(colSums(m_metrics$conf_mat) <= table(dts)))
})

test_that("metrics.vlmc objects print as expecte", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  model <- vlmc(dts, alpha = 0.1)
  m_metrics <- metrics(model)
  expect_snapshot(print(m_metrics))
})
