test_that("summary reports correctly its content (C++)", {
  withr::local_seed(0)
  rdts <- sample(c("A", "B", "C"), 100, replace = TRUE)
  model <- vlmc(rdts, alpha = 0.5, backend = "C++")
  expect_snapshot(summary(model))
  model2 <- vlmc(rdts, cutoff = 0.5 + runif(1) / 2, backend = "C++")
  expect_snapshot(summary(model2))
})
