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
    expect_true(compare_ctx(ctx_r, ctx_cpp))
  }
})
