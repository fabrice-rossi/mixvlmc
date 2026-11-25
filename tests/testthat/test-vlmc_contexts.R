test_that("context format is consistent", {
  dts <- sample(c("A", "B", "C"), 100, replace = TRUE)
  for (backend in c("R", "C++")) {
    withr::local_options("mixvlmc.backend" = backend)
    model <- vlmc(dts, alpha = 0.5, keep_match = TRUE)
    raw_ctx <- contexts(model, sequence = TRUE)
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
  }
})

test_that("context cut off are consistent", {
  dts <- sample(c("A", "B", "C"), 100, replace = TRUE)
  for (backend in c("R", "C++")) {
    withr::local_options("mixvlmc.backend" = backend)
    model <- vlmc(dts, alpha = 0.5)
    ctx_co_native <- contexts(model, frequency = "detailed", cutoff = "native")
    ctx_co_quantile <- contexts(model, cutoff = "quantile")
    expect_equal(ctx_co_quantile$cutoff, stats::pchisq(2 * ctx_co_native$cutoff, df = 2, lower.tail = FALSE))
  }
})

test_that("local counts do not change positions", {
  withr::local_seed(0)
  x <- sample(c("a", "b", "c"), 300, replace = TRUE)
  for (backend in c("R", "C++")) {
    model <- vlmc(x, alpha = 0.5, keep_match = TRUE, backend = backend)
    c_glob <- contexts(model, frequency = "detailed", positions = TRUE)
    c_loc <- contexts(model, frequency = "detailed", positions = TRUE, local = TRUE)
    ## some contexts are internal nodes
    expect_true(any(c_loc$freq < c_glob$freq))
    expect_equal(c_loc$freq, unname(rowSums(c_loc[, 3:5])))
    expect_identical(c_loc$positions, c_glob$positions)
  }
})

test_that("metrics can be computed with local counts", {
  withr::local_seed(0)
  x <- sample(c("a", "b", "c"), 300, replace = TRUE)
  for (backend in c("R", "C++")) {
    model <- vlmc(x, alpha = 0.5, backend = backend)
    m_glob <- contexts(model, metrics = TRUE)
    expect_no_error(m_loc <- contexts(model, metrics = TRUE, local = TRUE))
    expect_equal(m_loc$accuracy, m_glob$accuracy)
  }
})
