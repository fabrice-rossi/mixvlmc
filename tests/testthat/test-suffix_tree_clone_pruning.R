test_that("the suffix tree clone pruning gives two independant suffix trees", {
  withr::local_seed(10)
  for (k in 1:9) {
    x <- sample(0:k, 10000, replace = TRUE)
    x_rev <- rev(x)
    tree <- build_suffix_tree(x_rev[-1], k + 1)
    tree$compute_counts(x_rev[1], FALSE)
    pruned_tree <- tree$clone_prune(2, length(x) / 10)
    expect_error(tree$nb_contexts(), "nb_contexts is only avaiable if the tree was pruned")
    expect_no_error(pruned_tree$nb_contexts())
    expect_lt(pruned_tree$depth(), tree$depth())
    before_pruned_ctx <- pruned_tree$contexts(1, -1)
    ## none of the following operations should have an effect
    ## on pruned_tree
    tree$prune(3, -1)
    rm(tree)
    gc()
    pruned_ctx <- pruned_tree$contexts(1, -1)
    expect_identical(before_pruned_ctx, pruned_ctx)
  }
})

test_that("the suffix tree clone pruning gives an equivalent result as direct pruning", {
  withr::local_seed(10)
  for (k in 1:9) {
    x <- sample(0:k, 10000, replace = TRUE)
    x_rev <- rev(x)
    tree <- build_suffix_tree(x_rev[-1], k + 1)
    tree$compute_counts(x_rev[1], FALSE)
    pruned_tree <- tree$clone_prune(10, length(x) / 10)
    tree$prune(10, length(x) / 10)
    clone_ctxs <- pruned_tree$contexts(1, -1)
    direct_ctxs <- tree$contexts(1, -1)
    expect_equal(length(clone_ctxs), length(direct_ctxs))
    all_valid <- TRUE
    for (l in seq_along(clone_ctxs)) {
      all_valid <- Position(\(x) identical(x, clone_ctxs[[l]]), direct_ctxs, nomatch = 0) > 0
      if (!all_valid) {
        break
      }
    }
    expect_true(all_valid)
  }
})
