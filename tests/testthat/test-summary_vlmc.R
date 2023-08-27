test_that("summary reports correctly its content", {
  withr::local_seed(0)
  dts <- sample(c("A", "B", "C"), 100, replace = TRUE)
  model <- vlmc(dts, alpha = 0.5)
  expect_snapshot(summary(model))
  model2 <- vlmc(dts, cutoff = 0.5 + runif(1) / 2)
  expect_snapshot(summary(model2))
})
