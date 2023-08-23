test_that("the C++ context algorithm selects the same PST as the R implementation in a specific case", {
  withr::local_seed(0)
  x <- sample(c("A", "B", "C"), 10000, replace = TRUE)
  model <- vlmc(x, alpha = 0.05, backend = "C++")
  r_model <- vlmc(x, alpha = 0.05)
  ctx_cpp <- contexts(model)
  ctx_r <- contexts(r_model)
  expect_equal(length(ctx_cpp), length(ctx_r))
  if (length(ctx_cpp) != length(ctx_r)) {
    ## no need to match in this case
    return()
  }
  all_valid <- TRUE
  for (l in seq_along(ctx_cpp)) {
    all_valid <- Position(\(x) identical(x, ctx_cpp[[l]]), ctx_r, nomatch = 0) > 0
    if (!all_valid) {
      break
    }
  }
  expect_true(all_valid)
})
