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
