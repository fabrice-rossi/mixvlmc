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
    r_tree_ctxs <- unclass(contexts(r_tree, reverse = TRUE, sequence = TRUE)$context)
    expect_true(compare_ctx(r_tree_ctxs, ctxs))
  }
})

test_that("the suffix tree extracts all contexts and only them (more tests)", {
  withr::local_seed(2)
  rdts <- sample(letters[1:4], 100, replace = TRUE)
  rdts_tr <- to_dts(rdts)
  x <- rev(rdts_tr$ix)
  tree <- build_suffix_tree(x[-1], 4)
  tree$compute_counts(x[1], FALSE)
  ctxs <- tree$contexts(1, 5)
  r_tree <- ctx_tree(rdts_tr$ix, min_size = 1, max_depth = 5)
  r_tree_ctxs <- unclass(contexts(r_tree, reverse = TRUE, sequence = TRUE)$context)
  expect_true(compare_ctx(r_tree_ctxs, ctxs))
})
