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

test_that("C++ contexts return the appropriate data format", {
  dts <- sample(c("A", "B", "C"), 100, replace = TRUE)
  dts_tree <- ctx_tree(dts, max_depth = 4, backend = "C++")
  expect_type(contexts(dts_tree), "list")
  expect_s3_class(contexts(dts_tree, sequence = TRUE), "data.frame")
})

test_that("C++ contexts reversing reverses", {
  dts <- sample(c("A", "B", "C"), 100, replace = TRUE)
  dts_tree <- ctx_tree(dts, max_depth = 4, backend = "C++")
  def_ctx <- contexts(dts_tree)
  rev_ctx <- contexts(dts_tree, reverse = TRUE)
  expect_length(rev_ctx, length(def_ctx))
  for (k in seq_along(def_ctx)) {
    expect_identical(as_sequence(rev_ctx[[k]]), as_sequence(rev(def_ctx[[k]])))
  }
})

test_that("C++ context format is consistent", {
  dts <- sample(c("A", "B", "C"), 100, replace = TRUE)
  dts_tree <- ctx_tree(dts, max_depth = 4, backend = "C++")
  raw_ctx <- contexts(dts_tree, sequence = TRUE)
  expect_named(raw_ctx, c("context"))
  freq_ctx <- contexts(dts_tree, frequency = "total")
  expect_named(freq_ctx, c("context", "freq"))
  full_ctx <- contexts(dts_tree, frequency = "detailed")
  expect_named(full_ctx, c("context", "freq", "A", "B", "C"))
})

test_that("C++ basic count consistency", {
  dts <- sample(c("A", "B", "C"), 100, replace = TRUE)
  dts_tree_cpp <- ctx_tree(dts, max_depth = 4, backend = "C++")
  full_ctx_cpp <- contexts(dts_tree_cpp, frequency = "detailed")
  expect_equal(nrow(full_ctx_cpp), context_number(dts_tree_cpp))
  totals <- apply(full_ctx_cpp[3:5], 1, sum)
  names(totals) <- NULL
  expect_equal(full_ctx_cpp$freq, totals)
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
  dts <- sample(letters[1:4], 100, replace = TRUE)
  dts_ctree <- ctx_tree(dts, min_size = 1, max_depth = 5, backend = "C++")
  dts_rtree <- ctx_tree(dts, min_size = 1, max_depth = 5, backend = "R")
  cpp_ctxs <- contexts(dts_ctree, frequency = "detailed", position = FALSE)
  r_ctxs <- contexts(dts_rtree, frequency = "detailed", position = FALSE)
  expect_true(compare_ctx(r_ctxs, cpp_ctxs))
})
