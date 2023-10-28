test_that("reverse links are computed correctly", {
  withr::local_seed(100)
  for (k in 1:4) {
    x <- sample(0:k, 5000, replace = TRUE)
    r_model <- vlmc(x, alpha = 0.1)
    cpp_model <- vlmc(x, alpha = 0.1, backend = "C++")
    ctxs <- lapply(contexts(cpp_model, reverse = TRUE), as_sequence)
    all_valid <- TRUE
    for (i in seq_along(ctxs)) {
      for (l in 0:k) {
        fmatch <- cpp_model$root$extend_left(ctxs[[i]], l)
        all_valid <- length(fmatch) == match_context(r_model, c(l + 1L, ctxs[[i]] + 1L))$depth
        if (!all_valid) {
          break
        }
      }
      if (!all_valid) {
        break
      }
    }
    expect_true(all_valid)
  }
})
