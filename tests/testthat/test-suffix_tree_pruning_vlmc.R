test_that("the C++ context algorithm selects the same PST as the R implementation", {
  withr::local_seed(10)
  for (k in 1:9) {
    x <- sample(0:k, 10000, replace = TRUE)
    model <- vlmc(x)
    x_rev <- rev(x)
    tree <- build_suffix_tree(x_rev[-1], k + 1)
    tree$compute_counts(x_rev[1], FALSE)
    tree$prune_context(2, length(x) / 10, model$cutoff)
    ctx_cpp <- tree$contexts(1, -1)
    ctx_r <- contexts(model)
    expect_equal(length(ctx_cpp), length(ctx_r))
    if (length(ctx_cpp) != length(ctx_r)) {
      ## no need to match in this case
      break
    }
    all_valid <- TRUE
    for (l in seq_along(ctx_cpp)) {
      all_valid <- Position(\(x) identical(x, ctx_cpp[[l]]), ctx_r, nomatch = 0) > 0
      if (!all_valid) {
        break
      }
    }
    expect_true(all_valid)
  }
})
