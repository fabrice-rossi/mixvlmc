test_that("summary reports correctly its content", {
  withr::local_seed(0)
  rdts <- sample(c("A", "B", "C"), 100, replace = TRUE)
  model <- vlmc(rdts, alpha = 0.5, backend = "R")
  expect_snapshot(summary(model))
  model2 <- vlmc(rdts, cutoff = 0.5 + runif(1) / 2, backend = "R")
  expect_snapshot(summary(model2))
})

test_that("summary reports the lengths of the contexts on both backends", {
  withr::local_seed(0)
  x <- sample(0:3, 2000, replace = TRUE)
  for (backend in c("R", "C++")) {
    model <- vlmc(x, alpha = 0.5, backend = backend)
    ctx_length <- lengths(contexts(model, frequency = "detailed")$context)
    model_summary <- summary(model)
    expect_identical(model_summary$nb, length(ctx_length))
    expect_identical(model_summary$nb, context_number(model))
    expect_identical(model_summary$depth, max(ctx_length))
    expect_identical(model_summary$depth, depth(model))
    expect_equal(model_summary$avg_depth, mean(ctx_length))
  }
})
