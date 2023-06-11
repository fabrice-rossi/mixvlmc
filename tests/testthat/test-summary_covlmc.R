test_that("summary reports correctly its content", {
  withr::local_seed(0)
  dts <- sample(c("A", "B", "C"), 1000, replace = TRUE)
  y <- ifelse(runif(length(dts)) > 0.5, c(dts[-1], sample(c("A", "B", "C"), 1)), c(dts[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y <- as.factor(ifelse(runif(length(dts)) > 0.2, y, sample(c("A", "B", "C"), length(dts), replace = TRUE)))
  df_y <- data.frame(y = y)
  model <- covlmc(dts, df_y, alpha = 0.01, min_size = 3 / 2)
  expect_snapshot(summary(model))
})
