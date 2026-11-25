test_that("the progress of tune_vlmc() and tune_covlmc() is reported as messages", {
  withr::local_seed(0)
  x <- sample(c("A", "B", "C"), 200, replace = TRUE)
  for (backend in c("R", "C++")) {
    output <- capture.output(msgs <- testthat::capture_messages(
      tune_vlmc(x, verbose = 1, backend = backend)
    ))
    expect_length(output, 0)
    expect_true(any(grepl("Pruning vlmc", msgs)))
  }
  y <- data.frame(y = rnorm(length(x)))
  output <- capture.output(msgs <- testthat::capture_messages(
    tune_covlmc(x, y, verbose = 1)
  ))
  expect_length(output, 0)
  expect_true(any(grepl("Pruning covlmc", msgs)))
})
