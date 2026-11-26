test_that("the logistic models do not keep the calling frames of the fit", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  x <- cut(pc$active_power,
    breaks = c(0, 0.4, 2, 8),
    labels = c("low", "typical", "high")
  )
  cov <- data.frame(day = (pc$hour >= 7 & pc$hour <= 17))
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    model <- covlmc(x, cov, min_size = 5)
    ## the environments of the formulas of the logistic models used to keep
    ## the frames of covlmc() and of its callers, making the serialised model
    ## several times larger than the model itself
    expect_lt(
      length(serialize(model, NULL)),
      2 * as.numeric(utils::object.size(model))
    )
    best <- tune_covlmc(x, cov)$best_model
    expect_lt(
      length(serialize(best, NULL)),
      2 * as.numeric(utils::object.size(best))
    )
  }
})
