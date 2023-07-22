test_that("the suffix tree pruning is equivalent to context selection", {
  withr::local_seed(10)
  for (k in 1:9) {
    x <- sample(0:k, 10000, replace = TRUE)
    x_rev <- rev(x)
    tree <- build_suffix_tree(x_rev[-1])
    tree$compute_counts(x_rev[1])
    ctx_before <- tree$contexts(2, length(x) / 10)
    tree$prune(2, length(x) / 10)
    ctx_after <- tree$contexts(1, -1)
    expect_equal(length(ctx_after), length(ctx_before))
    if (length(ctx_after) != length(ctx_before)) {
      ## avoid super slow expect_identical in some cases
      break
    }
    expect_identical(ctx_after, ctx_before)
    ## test also post pruning
    ctx_before <- tree$contexts(2, 10)
    tree$prune(2, 10)
    ctx_after <- tree$contexts(1, -1)
    expect_equal(length(ctx_after), length(ctx_before))
    if (length(ctx_after) != length(ctx_before)) {
      ## avoid super slow expect_identical in some cases
      break
    }
    expect_identical(ctx_after, ctx_before)
  }
})

test_that("the suffix tree pruning reports the correct number of contexts", {
  withr::local_seed(10)
  for (k in 1:9) {
    x <- sample(0:k, 10000, replace = TRUE)
    x_rev <- rev(x)
    tree <- build_suffix_tree(x_rev[-1])
    nb_ctx <- tree$prune(2, length(x) / 10)
    ctx <- tree$contexts(1, -1)
    expect_equal(nb_ctx, length(ctx))
  }
})
