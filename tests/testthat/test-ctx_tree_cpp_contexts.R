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

test_that("C++ contexts return the appropriate data format", {
  dts <- sample(c("A", "B", "C"), 100, replace = TRUE)
  dts_tree <- ctx_tree(dts, max_depth = 4, backend = "C++")
  expect_type(contexts(dts_tree), "list")
  expect_s3_class(contexts(dts_tree, type = "data.frame"), "data.frame")
})

test_that("C++ contexts reversing reverses", {
  dts <- sample(c("A", "B", "C"), 100, replace = TRUE)
  dts_tree <- ctx_tree(dts, max_depth = 4, backend = "C++")
  def_ctx <- contexts(dts_tree)
  rev_ctx <- contexts(dts_tree, reverse = FALSE)
  expect_length(rev_ctx, length(def_ctx))
  for (k in seq_along(def_ctx)) {
    expect_identical(rev_ctx[[k]], rev(def_ctx[[k]]))
  }
})

test_that("C++ context format is consistent", {
  dts <- sample(c("A", "B", "C"), 100, replace = TRUE)
  dts_tree <- ctx_tree(dts, max_depth = 4, backend = "C++")
  raw_ctx <- contexts(dts_tree, type = "data.frame")
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
    expect_equal(nrow(cpp_ctxs), nrow(r_ctxs))
    all_valid <- TRUE
    for (l in seq_along(cpp_ctxs$context)) {
      pos_in_r <- Position(\(x) identical(x, cpp_ctxs$context[[l]]), r_ctxs$context, nomatch = 0)
      if (pos_in_r == 0) {
        all_valid <- FALSE
        break
      }
      all_valid <- all.equal(cpp_ctxs[l, ], r_ctxs[pos_in_r, ], check.attributes = FALSE)
      if (!all_valid) {
        break
      }
    }
    expect_true(all_valid)
  }
})
