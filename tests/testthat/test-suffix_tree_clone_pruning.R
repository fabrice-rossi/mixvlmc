test_that("the suffix tree clone pruning gives two independant suffix trees", {
  withr::local_seed(10)
  for (k in 1:9) {
    x <- sample(0:k, 10000, replace = TRUE)
    x_rev <- rev(x)
    tree <- build_suffix_tree(x_rev[-1], k + 1)
    tree$compute_counts(x_rev[1], FALSE)
    pruned_tree <- tree$clone_prune(2, length(x) / 10)
    expect_error(tree$nb_contexts(), "nb_contexts is only available if the tree was pruned")
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
    expect_true(compare_ctx(clone_ctxs, direct_ctxs))
  }
})

test_that("the suffix tree clone pruning keeps the root when needed", {
  x <- c(0, 1, 1, 0, 1, 0)
  x_rev <- rev(x)
  tree <- build_suffix_tree(x_rev[-1], 2)
  tree$compute_counts(x_rev[1], FALSE)
  pruned_tree <- tree$clone_prune(10, -1)
  expect_equal(pruned_tree$nb_contexts(), 1)
  expect_equal(pruned_tree$depth(), 0)
  expect_identical(pruned_tree$contexts(1, -1), list(integer()))
  tree$prune(10, -1)
  expect_equal(tree$nb_contexts(), 1)
  expect_equal(tree$depth(), 0)
  expect_identical(tree$contexts(1, -1), list(integer()))
})

test_that("the suffix tree clone pruning limits the depth as direct pruning", {
  withr::local_seed(10)
  for (k in 1:5) {
    x <- sample(0:k, 2000, replace = TRUE)
    x_rev <- rev(x)
    for (d in 1:4) {
      tree <- build_suffix_tree(x_rev[-1], k + 1)
      tree$compute_counts(x_rev[1], FALSE)
      pruned_tree <- tree$clone_prune(2, d)
      tree$prune(2, d)
      expect_true(compare_ctx(pruned_tree$contexts(1, -1), tree$contexts(1, -1)))
      expect_equal(pruned_tree$depth(), tree$depth())
      expect_equal(pruned_tree$nb_contexts(), tree$nb_contexts())
    }
  }
})

test_that("the context clone pruning limits the depth as a direct estimation", {
  withr::local_seed(10)
  for (k in 1:4) {
    x <- sample(0:k, 2000, replace = TRUE)
    full_model <- vlmc(x, min_size = 2, max_depth = 20, cutoff = 0.01, backend = "C++")
    for (d in 1:4) {
      for (cutoff in c(0.5, 2, 4)) {
        pruned_tree <- full_model$root$clone_prune_context(2, d, cutoff)
        direct_model <- vlmc(x,
          min_size = 2, max_depth = d, cutoff = cutoff,
          backend = "C++"
        )
        expect_true(compare_ctx(
          pruned_tree$contexts(1, -1),
          direct_model$root$contexts(1, -1)
        ))
        expect_equal(pruned_tree$nb_contexts(), context_number(direct_model))
      }
    }
  }
})
