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
