test_that("vlmc trimming does nothing in the default case (C++)", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  model <- vlmc(rdts, backend = "C++")
  expect_true(compare_vlmc_cpp(model, trim(model)))
})

test_that("vlmc trimming does create memory issues (C++)", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  model <- vlmc(rdts, backend = "C++")
  trimmed_model <- trim(model)
  rm(model)
  gc()
  expect_no_error(contexts(trimmed_model, frequency = "detailed", cutoff = "quantile", metrics = TRUE))
  rm(trimmed_model)
  gc()
  model <- vlmc(rdts, backend = "C++", keep_match = TRUE)
  trimmed_model <- trim(model)
  rm(model)
  gc()
  expect_no_error(contexts(trimmed_model, frequency = "detailed", cutoff = "quantile", metrics = TRUE))
})

test_that("vlmc trimming preserve core information when needed (C++)", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  model <- vlmc(rdts, keep_match = TRUE, backend = "C++")
  trimmed_model <- trim(model)
  expect_true(compare_ctx(
    contexts(trimmed_model, frequency = "detailed"),
    contexts(model, frequency = "detailed")
  ))
})

test_that("vlmc trimming works after serialization (C++)", {
  withr::local_seed(4)
  x <- sample(c(0, 1, 2), 300, replace = TRUE)
  model <- vlmc(x, min_size = 3, max_depth = 4, keep_match = TRUE, backend = "C++")
  model_path <- withr::local_tempfile(fileext = ".Rds")
  saveRDS(model, model_path)
  restored_model <- readRDS(model_path)
  expect_no_error(trimmed_model <- trim(restored_model))
  expect_true(compare_ctx(
    contexts(trimmed_model, frequency = "detailed"),
    contexts(model, frequency = "detailed")
  ))
})

test_that("trimmed models do not regain positions after restoration (C++)", {
  withr::local_seed(4)
  x <- sample(c(0, 1, 2), 300, replace = TRUE)
  model <- vlmc(x, min_size = 3, max_depth = 4, keep_match = TRUE, backend = "C++")
  trimmed_model <- trim(model)
  model_path <- withr::local_tempfile(fileext = ".Rds")
  saveRDS(trimmed_model, model_path)
  restored_model <- readRDS(model_path)
  expect_true(compare_vlmc_cpp(trimmed_model, restored_model))
  expect_error(
    positions(find_sequence(restored_model, 0)),
    "Cannot report positions if they were not saved"
  )
})
