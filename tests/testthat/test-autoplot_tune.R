test_that("plot.tune_vlmc works as expected", {
  pc <- powerconsumption[powerconsumption$week == 10, ]
  rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
  tune_result <- tune_vlmc(rdts)
  vdiffr::expect_doppelganger(
    "Base autoplot",
    \() print(ggplot2::autoplot(tune_result))
  )
  tune_result_aic <- tune_vlmc(rdts, criterion = "AIC")
  vdiffr::expect_doppelganger(
    "Native cut off autoplot",
    \() print(ggplot2::autoplot(tune_result_aic, cutoff = "native"))
  )
})

test_that("plot.tune_covlmc works as expected", {
  pc <- powerconsumption[powerconsumption$week == 10, ]
  rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
  rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
  tune_result <- tune_covlmc(rdts, rdts_cov)
  vdiffr::expect_doppelganger(
    "COVLMC base autoplot",
    \() print(ggplot2::autoplot(tune_result))
  )
})
