test_that("the suffix tree extracts contexts of the correct length", {
  withr::local_seed(10)
  for (k in 1:9) {
    x <- sample(0:k, 1000, replace = TRUE)
    x_rev <- rev(x)
    tree <- build_suffix_tree(x_rev[-1], k + 1)
    tree$compute_counts(x_rev[1], FALSE)
    ctxs <- tree$contexts(2, 10)
    expect_true(all(sapply(ctxs, length) <= 10))
  }
})


test_that("the suffix tree extracts all contexts and only them", {
  withr::local_seed(1)
  for (k in 1:9) {
    x <- sample(0:k, 1000, replace = TRUE)
    x_rev <- rev(x)
    tree <- build_suffix_tree(x_rev[-1], k + 1)
    tree$compute_counts(x_rev[1], FALSE)
    ctxs <- tree$contexts(2, 10)
    r_tree <- ctx_tree(x, min_size = 2, max_depth = 10)
    r_tree_ctxs <- contexts(r_tree, reverse = TRUE)
    expect_equal(length(ctxs), length(r_tree_ctxs))
    all_valid <- TRUE
    for (l in seq_along(ctxs)) {
      all_valid <- Position(\(x) identical(x, ctxs[[l]]), r_tree_ctxs, nomatch = 0) > 0
      if (!all_valid) {
        break
      }
    }
    expect_true(all_valid)
  }
})
