test_that("vlmc trimming does nothing in the default case (C++)", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  model <- vlmc(dts, backend = "C++")
  expect_true(compare_vlmc(model, trim(model)))
})

test_that("vlmc trimming does create memory issues (C++)", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  model <- vlmc(dts, backend = "C++")
  trimmed_model <- trim(model)
  rm(model)
  gc()
  expect_no_error(contexts(trimmed_model, frequency = "detailed", cutoff = "quantile", metrics = TRUE))
  rm(trimmed_model)
  gc()
  model <- vlmc(dts, backend = "C++", keep_match = TRUE)
  trimmed_model <- trim(model)
  rm(model)
  gc()
  expect_no_error(contexts(trimmed_model, frequency = "detailed", cutoff = "quantile", metrics = TRUE))
})

test_that("vlmc trimming preserve core information when needed (C++)", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  model <- vlmc(dts, keep_match = TRUE, backend = "C++")
  trimmed_model <- trim(model)
  expect_true(compare_ctx(
    contexts(trimmed_model, frequency = "detailed"),
    contexts(model, frequency = "detailed")
  ))
})

test_that("vlmc trimming removes positions (C++)", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  model <- vlmc(dts, keep_match = TRUE, backend = "C++")
  expect_no_error(contexts(model, positions = TRUE))
  trimmed_model <- trim(model)
  expect_error(contexts(trimmed_model, positions = TRUE))
})
