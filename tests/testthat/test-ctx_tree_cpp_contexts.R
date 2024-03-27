test_that("the C++ backend find the same contexts as the R one", {
  withr::local_seed(0)
  for (k in 1:9) {
    x <- sample(0:k, 1000, replace = TRUE)
    cpp_tree <- ctx_tree(x, min_size = 2, max_depth = 5 + k, keep_position = FALSE, backend = "C++")
    cpp_ctxs <- contexts(cpp_tree, sequence = TRUE)
    r_tree <- ctx_tree(x, min_size = 2, max_depth = 5 + k)
    r_ctxs <- contexts(r_tree, sequence = TRUE)
    expect_true(compare_ctx(r_ctxs, cpp_ctxs))
  }
})

test_that("the C++ backend find the same detailed contexts as the R one (up to ordering)", {
  withr::local_seed(0)
  for (k in 1:9) {
    x <- sample(0:k, 500, replace = TRUE)
    cpp_tree <- ctx_tree(x, min_size = 2, max_depth = 5 + k, backend = "C++")
    cpp_ctxs <- contexts(cpp_tree, frequency = "detailed", position = TRUE)
    r_tree <- ctx_tree(x, min_size = 2, max_depth = 5 + k)
    r_ctxs <- contexts(r_tree, frequency = "detailed", position = TRUE)
    expect_true(compare_ctx(r_ctxs, cpp_ctxs))
  }
})

test_that("the C++ and R backend agree", {
  withr::local_seed(2)
  rdts <- sample(letters[1:4], 100, replace = TRUE)
  rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 5, backend = "C++")
  rdts_rtree <- ctx_tree(rdts, min_size = 1, max_depth = 5, backend = "R")
  cpp_ctxs <- contexts(rdts_ctree, frequency = "detailed", position = FALSE)
  r_ctxs <- contexts(rdts_rtree, frequency = "detailed", position = FALSE)
  expect_true(compare_ctx(r_ctxs, cpp_ctxs))
})
