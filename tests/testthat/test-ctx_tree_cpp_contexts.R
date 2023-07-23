test_that("the C++ backend find the same contexts as the R one", {
  withr::local_seed(0)
  for (k in 1:9) {
    x <- sample(0:k, 1000, replace = TRUE)
    cpp_tree <- ctx_tree(x, min_size = 2, max_depth = 5 + k, keep_position = FALSE, backend = "C++")
    cpp_ctxs <- contexts(cpp_tree)
    r_tree <- ctx_tree(x, min_size = 2, max_depth = 5 + k)
    r_ctxs <- contexts(r_tree, reverse = TRUE)
    expect_equal(length(cpp_ctxs), length(r_ctxs))
    all_valid <- TRUE
    for (l in seq_along(cpp_ctxs)) {
      all_valid <- Position(\(x) identical(x, cpp_ctxs[[l]]), r_ctxs, nomatch = 0) > 0
      if (!all_valid) {
        break
      }
    }
    expect_true(all_valid)
  }
})
