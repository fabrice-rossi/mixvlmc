test_that("printing", {
  dts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
  dts_ctree <- ctx_tree(dts, min_size = 1, max_depth = 2, backend = "C++")
  expect_snapshot_output(print(dts_ctree))
})
