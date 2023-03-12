test_that("that vlmc trimming does nothing", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  model <- vlmc(dts)
  expect_identical(trim(model), model)
})
