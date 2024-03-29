test_that("context format is consistent", {
  withr::local_seed(0)
  rdts <- sample(c("A", "B", "C"), 500, replace = TRUE)
  y <- ifelse(runif(length(rdts)) > 0.5, c(rdts[-1], sample(c("A", "B", "C"), 1)), c(rdts[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y <- as.factor(ifelse(runif(length(rdts)) > 0.2, y, sample(c("A", "B", "C"), length(rdts), replace = TRUE)))
  df_y <- data.frame(y = y)
  model <- covlmc(rdts, df_y, alpha = 0.75)
  raw_ctx <- contexts(model, sequence = TRUE)
  expect_named(raw_ctx, c("context"))
  freq_ctx <- contexts(model, frequency = "total")
  expect_named(freq_ctx, c("context", "freq"))
  hsize_ctx <- contexts(model, hsize = TRUE)
  expect_named(hsize_ctx, c("context", "hsize"))
  full_ctx <- contexts(model, frequency = "detailed")
  expect_named(full_ctx, c("context", "freq", "A", "B", "C"))
  full_ctx_model <- contexts(model, frequency = "detailed", model = "coef")
  expect_named(full_ctx_model, c("context", "freq", "A", "B", "C", "coef"))
  full_ctx_model_hsize <- contexts(model, frequency = "detailed", model = "coef", hsize = TRUE)
  expect_named(full_ctx_model_hsize, c("context", "freq", "A", "B", "C", "coef", "hsize"))
  super_full_ctx_model_hsize <- contexts(model, frequency = "detailed", positions = TRUE, model = "coef", hsize = TRUE)
  expect_named(super_full_ctx_model_hsize, c("context", "freq", "A", "B", "C", "positions", "coef", "hsize"))
  ctx_model <- contexts(model, model = "full")
  expect_named(ctx_model, c("context", "model"))
  ctx_model_with_metrics <- contexts(model, model = "full", metrics = TRUE)
  expect_named(ctx_model_with_metrics, c("context", "model", "accuracy", "auc"))
  ctx_model_with_merging <- contexts(model, model = "full", merging = TRUE)
  expect_named(ctx_model_with_merging, c("context", "model", "merged"))
})

test_that("models are consistent", {
  my_ncol <- function(x) {
    if (is.matrix(x)) {
      ncol(x)
    } else {
      length(x)
    }
  }
  pc <- powerconsumption[powerconsumption$week == 5, ]
  rdts <- cut(pc$active_power, breaks = c(0, 0.4, 2, 8), labels = c("low", "typical", "high"))
  rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
  for (engine in c("glm", "multinom")) {
    withr::local_options(mixvlmc.predictive = engine)
    m_cov <- covlmc(rdts, rdts_cov, min_size = 4, keep_data = TRUE, alpha = 0.5)
    ctx_m_cov_m <- contexts(m_cov, model = "full")
    ctx_m_cov_c <- contexts(m_cov, model = "coef", hsize = TRUE)
    expect_equal(ctx_m_cov_c$coef, lapply(contexts(m_cov, model = "full")$model, glm_coef, rdts_cov), ignore_attr = TRUE)
    expect_equal(sapply(ctx_m_cov_c$coef, my_ncol), ctx_m_cov_c$hsize + 1)
  }
})

test_that("context reporting is consistent", {
  withr::local_seed(0)
  rdts <- sample(c("A", "B", "C"), 500, replace = TRUE)
  y <- ifelse(runif(length(rdts)) > 0.5, c(rdts[-1], sample(c("A", "B", "C"), 1)), c(rdts[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y <- as.factor(ifelse(runif(length(rdts)) > 0.2, y, sample(c("A", "B", "C"), length(rdts), replace = TRUE)))
  df_y <- data.frame(y = y)
  model <- covlmc(rdts, df_y, alpha = 0.75)
  ctx_m_cov <- contexts(model, sequence = TRUE)
  expect_equal(nrow(ctx_m_cov), context_number(model))
  expect_equal(length(contexts(model)), context_number(model))
})

test_that("contexts do not depend on the format", {
  withr::local_seed(0)
  rdts <- sample(c("A", "B", "C"), 500, replace = TRUE)
  y <- ifelse(runif(length(rdts)) > 0.5, c(rdts[-1], sample(c("A", "B", "C"), 1)), c(rdts[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
  y <- as.factor(ifelse(runif(length(rdts)) > 0.2, y, sample(c("A", "B", "C"), length(rdts), replace = TRUE)))
  df_y <- data.frame(y = y)
  model <- covlmc(rdts, df_y, alpha = 0.75)
  ctx_m_cov <- contexts(model, sequence = TRUE)
  expect_equal(unclass(ctx_m_cov$context), lapply(contexts(model), as_sequence))
})

test_that("covariate depth is reported consistently", {
  withr::local_seed(0)
  for (k in 1:5) {
    rdts <- sample(c("A", "B", "C"), 500, replace = TRUE)
    y <- ifelse(runif(length(rdts)) > 0.5, c(rdts[-1], sample(c("A", "B", "C"), 1)), c(rdts[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE)))
    y <- as.factor(ifelse(runif(length(rdts)) > 0.2, y, sample(c("A", "B", "C"), length(rdts), replace = TRUE)))
    df_y <- data.frame(y = y)
    model <- covlmc(rdts, df_y, alpha = 0.9, min_size = 3)
    ctx_m_cov <- contexts(model, hsize = TRUE)
    expect_equal(covariate_depth(model), max(ctx_m_cov$hsize))
  }
})
