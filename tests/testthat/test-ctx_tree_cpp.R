test_that("printing works as expected", {
  rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
  withr::local_options("mixvlmc.backend" = "C++")
  rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 2)
  expect_snapshot_output(print(rdts_ctree))
})

test_that("automatic C++ representation restoration works", {
  rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
  rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 3, backend = "C++")
  expect_false(extptr_is_null(rdts_ctree$root$.pointer))
  tree_path <- withr::local_tempfile(fileext = ".Rds")
  saveRDS(rdts_ctree, tree_path)
  restored_tree <- readRDS(tree_path)
  expect_true(extptr_is_null(restored_tree$root$.pointer))
  ctxs_orig <- contexts(rdts_ctree, frequency = "detailed", positions = TRUE)
  ctxs_restored <- contexts(restored_tree, frequency = "detailed", positions = TRUE)
  expect_false(extptr_is_null(restored_tree$root$.pointer))
  expect_true(compare_ctx(ctxs_orig, ctxs_restored))
})

test_that("the depth of the tree does not include the sentinel", {
  withr::local_seed(1)
  for (k in 1:10) {
    dts <- sample(0:2, sample(5:60, 1), replace = TRUE)
    for (max_depth in c(3, 100)) {
      dts_ctree <- ctx_tree(dts, min_size = 1, max_depth = max_depth, backend = "C++")
      dts_rtree <- ctx_tree(dts, min_size = 1, max_depth = max_depth, backend = "R")
      expect_equal(depth(dts_ctree), depth(dts_rtree))
    }
  }
})

test_that("a context tree reduced to its root is built as in the R backend", {
  for (dts in list(c(0), c(0, 0), c(1, 0), c(0, 1, 2, 0))) {
    dts_rtree <- ctx_tree(dts, min_size = 5, backend = "R")
    expect_no_error(dts_ctree <- ctx_tree(dts, min_size = 5, backend = "C++"))
    expect_equal(context_number(dts_ctree), context_number(dts_rtree))
    expect_equal(depth(dts_ctree), depth(dts_rtree))
    expect_true(compare_ctx(
      contexts(dts_rtree, frequency = "detailed", positions = TRUE),
      contexts(dts_ctree, frequency = "detailed", positions = TRUE)
    ))
  }
})

test_that("max_depth = 0 gives a context tree reduced to its root", {
  withr::local_seed(0)
  dts <- sample(0:1, 100, replace = TRUE)
  dts_ctree <- ctx_tree(dts, max_depth = 0, backend = "C++")
  dts_rtree <- ctx_tree(dts, max_depth = 0, backend = "R")
  expect_equal(context_number(dts_ctree), context_number(dts_rtree))
  expect_equal(depth(dts_ctree), depth(dts_rtree))
  expect_true(compare_ctx(
    contexts(dts_rtree, frequency = "detailed", positions = TRUE),
    contexts(dts_ctree, frequency = "detailed", positions = TRUE)
  ))
  tree_path <- withr::local_tempfile(fileext = ".Rds")
  saveRDS(dts_ctree, tree_path)
  restored_tree <- readRDS(tree_path)
  expect_true(compare_ctx(
    contexts(dts_rtree, frequency = "detailed", positions = TRUE),
    contexts(restored_tree, frequency = "detailed", positions = TRUE)
  ))
})

test_that("trimming removes positions from C++ context trees", {
  dts <- c(0, 0, 1, 1, 0, 1)
  dts_ctree <- ctx_tree(dts, min_size = 1, max_depth = 2, backend = "C++")
  orig_positions <- positions(find_sequence(dts_ctree, 0))
  trimmed_ctree <- trim(dts_ctree)
  expect_true(compare_ctx(
    contexts(dts_ctree, frequency = "detailed"),
    contexts(trimmed_ctree, frequency = "detailed")
  ))
  for (ctx in list(numeric(), 0, c(1, 0))) {
    expect_error(
      positions(find_sequence(trimmed_ctree, ctx)),
      "Cannot report positions if they were not saved"
    )
  }
  ## the original tree is not modified
  expect_identical(positions(find_sequence(dts_ctree, 0)), orig_positions)
})

test_that("trimmed C++ context trees do not regain positions after restoration", {
  dts <- c(0, 0, 1, 1, 0, 1)
  trimmed_ctree <- trim(ctx_tree(dts, min_size = 1, max_depth = 2, backend = "C++"))
  tree_path <- withr::local_tempfile(fileext = ".Rds")
  saveRDS(trimmed_ctree, tree_path)
  restored_tree <- readRDS(tree_path)
  expect_true(compare_ctx(
    contexts(trimmed_ctree, frequency = "detailed"),
    contexts(restored_tree, frequency = "detailed")
  ))
  expect_error(
    positions(find_sequence(restored_tree, 0)),
    "Cannot report positions if they were not saved"
  )
})
