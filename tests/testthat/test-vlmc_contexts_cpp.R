test_that("context format is consistent", {
  dts <- sample(c("A", "B", "C"), 100, replace = TRUE)
  model <- vlmc(dts, alpha = 0.5, keep_match = TRUE, backend = "C++")
  raw_ctx <- contexts(model, type = "data.frame")
  expect_named(raw_ctx, c("context"))
  freq_ctx <- contexts(model, frequency = "total")
  expect_named(freq_ctx, c("context", "freq"))
  full_ctx <- contexts(model, frequency = "detailed")
  expect_named(full_ctx, c("context", "freq", "A", "B", "C"))
  full_ctx_co <- contexts(model, frequency = "detailed", cutoff = "native")
  expect_named(full_ctx_co, c("context", "freq", "A", "B", "C", "cutoff"))
  ctx_co <- contexts(model, cutoff = "quantile")
  expect_named(ctx_co, c("context", "cutoff"))
  m_full_ctx_co <- contexts(model, frequency = "detailed", cutoff = "native", metrics = TRUE)
  expect_named(m_full_ctx_co, c("context", "freq", "A", "B", "C", "cutoff", "accuracy", "auc"))
  m_super_full_ctx_co <- contexts(model, frequency = "detailed", positions = TRUE, cutoff = "native", metrics = TRUE)
  expect_named(m_super_full_ctx_co, c("context", "freq", "A", "B", "C", "positions", "cutoff", "accuracy", "auc"))
})

test_that("context cut off are consistent", {
  dts <- sample(c("A", "B", "C"), 100, replace = TRUE)
  model <- vlmc(dts, alpha = 0.5, backend = "C++")
  ctx_co_native <- contexts(model, frequency = "detailed", cutoff = "native")
  ctx_co_quantile <- contexts(model, cutoff = "quantile")
  expect_equal(ctx_co_quantile$cutoff, stats::pchisq(2 * ctx_co_native$cutoff, df = 2, lower.tail = FALSE))
})

test_that("contexts do not depend on the backend", {
  withr::local_seed(0)
  for (k in 1:9) {
    x <- sample(0:k, 500, replace = TRUE)
    cpp_model <- vlmc(x, alpha = 0.2, backend = "C++", keep_match = TRUE)
    cpp_ctxs <- contexts(cpp_model, frequency = "detailed", position = TRUE, cutoff = "native", metrics = TRUE)
    r_model <- vlmc(x, alpha = 0.2, keep_match = TRUE)
    r_ctxs <- contexts(r_model, frequency = "detailed", position = TRUE, cutoff = "native", metrics = TRUE)
    expect_true(compare_ctx(r_ctxs, cpp_ctxs))
  }
})
