test_that("plot.tune_vlmc works as expected", {
  pc <- powerconsumption[powerconsumption$week == 10, ]
  rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
  tune_result <- tune_vlmc(rdts)
  vdiffr::expect_doppelganger("Bic base plot", \() plot(tune_result))
  vdiffr::expect_doppelganger(
    "Log likelihood base plot",
    \() plot(tune_result, value = "likelihood")
  )
  tune_result_aic <- tune_vlmc(rdts, criterion = "AIC")
  vdiffr::expect_doppelganger(
    "Aic native base plot",
    \() plot(tune_result_aic, cutoff = "native")
  )
  vdiffr::expect_doppelganger(
    "Custom plot",
    \() plot(tune_result_aic,
      cutoff = "native",
      lwd = 2, type = "b", col = 2,
      xlab = "Something",
      ylab = "Something else",
      main = "Full custom plot"
    )
  )
})

test_that("plot.tune_covlmc works as expected", {
  pc <- powerconsumption[powerconsumption$week == 10, ]
  rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
  rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
  tune_result <- tune_covlmc(rdts, rdts_cov)
  vdiffr::expect_doppelganger("COVLMC Bic base plot", \() plot(tune_result))
  vdiffr::expect_doppelganger(
    "COVLMC Log likelihood base plot",
    \() plot(tune_result, value = "likelihood")
  )
  tune_result_aic <- tune_covlmc(rdts, rdts_cov, criterion = "AIC")
  vdiffr::expect_doppelganger(
    "COVLMC Aic base plot",
    \() plot(tune_result_aic)
  )
  vdiffr::expect_doppelganger(
    "COVLMC Custom plot",
    \() plot(tune_result_aic,
      lwd = 2, type = "b", col = 2,
      xlab = "Something",
      ylab = "Something else",
      main = "Full custom plot"
    )
  )
})

test_that("plot.tune_covlmc rejects native scale", {
  pc <- powerconsumption[powerconsumption$week == 10, ]
  rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
  rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
  tune_result <- tune_covlmc(rdts, rdts_cov)
  expect_error(plot(tune_result, cutoff = "native"))
})
