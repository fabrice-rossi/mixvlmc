
test_that("prune() uses the engine of the model", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  x <- cut(pc$active_power,
    breaks = c(0, 0.4, 2, 8),
    labels = c("low", "typical", "high")
  )
  cov <- data.frame(day = (pc$hour >= 7 & pc$hour <= 17))
  withr::local_options(mixvlmc.predictive = "glm")
  model <- covlmc(x, cov, alpha = 0.5)
  expect_identical(model$engine, "glm")
  pruned <- prune(model, alpha = 0.01)
  expect_identical(pruned$engine, "glm")
  withr::local_options(mixvlmc.predictive = "multinom")
  expect_warning(pruned_m <- prune(model, alpha = 0.01), "glm")
  expect_identical(pruned_m$engine, "glm")
  expect_identical(getOption("mixvlmc.predictive"), "multinom")
  expect_equal(loglikelihood(pruned_m), loglikelihood(pruned))
  expect_identical(draw(pruned_m), draw(pruned))
  ## the current engine is used for models without a recorded engine
  model$engine <- NULL
  expect_no_warning(pruned_old <- prune(model, alpha = 0.01))
  expect_identical(pruned_old$engine, "multinom")
})
