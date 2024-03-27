test_that("as_vlmc.tune_covlmc obeys is basic contract", {
  pc <- powerconsumption[powerconsumption$week %in% 5:7, ]
  rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
  rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
  rdts_best_model_tune <- tune_covlmc(rdts, rdts_cov)
  model <- as_covlmc(rdts_best_model_tune)
  expect_true(is_covlmc(model))
})
