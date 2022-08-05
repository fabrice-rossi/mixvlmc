test_that("context format is consistent", {
  withr::local_seed(0)
  dts <- sample(c("A", "B", "C"), 500, replace = TRUE)
  y <- ifelse(runif(length(dts)) > 0.5, c(dts[-1], sample(c("A", "B", "C"), 1)), c(dts[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y <- as.factor(ifelse(runif(length(dts)) > 0.2, y, sample(c("A", "B", "C"), length(dts), replace = TRUE)))
  df_y <- data.frame(y = y)
  model <- covlmc(dts, df_y, alpha = 0.75)
  raw_ctx <- contexts(model, type = "data.frame")
  expect_named(raw_ctx, c("context"))
  freq_ctx <- contexts(model, frequency = "total")
  expect_named(freq_ctx, c("context", "freq"))
  full_ctx <- contexts(model, frequency = "detailed")
  expect_named(full_ctx, c("context", "freq", "A", "B", "C"))
  full_ctx_model <- contexts(model, frequency = "detailed", model = "coef")
  expect_named(full_ctx_model, c("context", "freq", "A", "B", "C", "coef"))
  ctx_model <- contexts(model, model = "full")
  expect_named(ctx_model, c("context", "model"))
})

test_that("models are consistent", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
  dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    m_cov <- covlmc(dts, dts_cov, min_size = 5, keep_data = TRUE)
    ctx_m_cov_m <- contexts(m_cov, model = "full")
    ctx_m_cov_c <- contexts(m_cov, model = "coef")
    expect_equal(ctx_m_cov_c$coef, lapply(contexts(m_cov, type = "data.frame", model = "full")$model, coef), ignore_attr = TRUE)
  }
})

test_that("context reporting is consistent", {
  withr::local_seed(0)
  dts <- sample(c("A", "B", "C"), 500, replace = TRUE)
  y <- ifelse(runif(length(dts)) > 0.5, c(dts[-1], sample(c("A", "B", "C"), 1)), c(dts[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y <- as.factor(ifelse(runif(length(dts)) > 0.2, y, sample(c("A", "B", "C"), length(dts), replace = TRUE)))
  df_y <- data.frame(y = y)
  model <- covlmc(dts, df_y, alpha = 0.75)
  ctx_m_cov <- contexts(model, type = "data.frame")
  expect_equal(nrow(ctx_m_cov), context_number(model))
  expect_equal(length(contexts(model)), context_number(model))
})

test_that("contexts do not depend on the format", {
  withr::local_seed(0)
  dts <- sample(c("A", "B", "C"), 500, replace = TRUE)
  y <- ifelse(runif(length(dts)) > 0.5, c(dts[-1], sample(c("A", "B", "C"), 1)), c(dts[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y <- as.factor(ifelse(runif(length(dts)) > 0.2, y, sample(c("A", "B", "C"), length(dts), replace = TRUE)))
  df_y <- data.frame(y = y)
  model <- covlmc(dts, df_y, alpha = 0.75)
  ctx_m_cov <- contexts(model, type = "data.frame")
  expect_equal(unclass(ctx_m_cov$context), contexts(model))
})
