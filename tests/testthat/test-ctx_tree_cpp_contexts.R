test_that("the C++ backend find the same contexts as the R one", {
  withr::local_seed(0)
  for (k in 1:9) {
    x <- sample(0:k, 1000, replace = TRUE)
    cpp_tree <- ctx_tree(x, min_size = 2, max_depth = 5 + k, keep_position = FALSE, backend = "C++")
    cpp_ctxs <- contexts(cpp_tree, sequence = TRUE)
    r_tree <- ctx_tree(x, min_size = 2, max_depth = 5 + k, backend = "R")
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
    r_tree <- ctx_tree(x, min_size = 2, max_depth = 5 + k, backend = "R")
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

test_that("the C++ and R backends agree with min_size = 1 and positions", {
  ## regression test: the end of the time series was matched incorrectly
  x <- c(0, 1, 1, 0, 1)
  expect_no_warning(cpp_tree <- ctx_tree(x, min_size = 1, backend = "C++"))
  r_tree <- ctx_tree(x, min_size = 1, backend = "R")
  expect_equal(context_number(cpp_tree), context_number(r_tree))
  expect_true(compare_ctx(
    contexts(r_tree, frequency = "detailed", positions = TRUE),
    contexts(cpp_tree, frequency = "detailed", positions = TRUE)
  ))
  withr::local_seed(0)
  for (k in 1:20) {
    x <- sample(0:2, sample(5:50, 1), replace = TRUE)
    expect_no_warning(cpp_tree <- ctx_tree(x, min_size = 1, backend = "C++"))
    r_tree <- ctx_tree(x, min_size = 1, backend = "R")
    expect_equal(context_number(cpp_tree), context_number(r_tree))
    expect_true(compare_ctx(
      contexts(r_tree, frequency = "detailed", positions = TRUE),
      contexts(cpp_tree, frequency = "detailed", positions = TRUE)
    ))
  }
})
