test_that("metrics.vlmc results do not depend on the backend", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  r_model <- vlmc(dts, alpha = 0.1)
  cpp_model <- vlmc(dts, alpha = 0.1, backend = "C++")
  r_metrics <- metrics(r_model)
  cpp_metrics <- metrics(cpp_model)
  expect_equal(cpp_metrics$accuracy, r_metrics$accuracy)
  expect_equal(cpp_metrics$conf_mat, r_metrics$conf_mat)
  expect_equal(cpp_metrics$auc, r_metrics$auc)
})
