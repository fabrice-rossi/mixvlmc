test_that("vlmc trimming does nothing in the default case", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  model <- vlmc(rdts)
  expect_identical(trim(model), model)
})

test_that("vlmc trimming preserve core information when needed", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  model <- vlmc(rdts, keep_match = TRUE)
  trimmed_model <- trim(model)
  expect_identical(
    contexts(trimmed_model, type = "data.frame", frequency = "detailed"),
    contexts(model, type = "data.frame", frequency = "detailed")
  )
  expect_true(object.size(trimmed_model) < object.size(model))
})

test_that("vlmc trimming removes positions", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  for (backend in c("R", "C++")) {
    withr::local_options("mixvlmc.backend" = backend)
    model <- vlmc(rdts, keep_match = TRUE)
    expect_no_error(contexts(model, type = "data.frame", positions = TRUE))
    trimmed_model <- trim(model)
    expect_error(contexts(trimmed_model, type = "data.frame", positions = TRUE))
  }
})
