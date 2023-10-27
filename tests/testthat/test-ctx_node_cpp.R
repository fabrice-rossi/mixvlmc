test_that("context objects are printed as expected", {
  dts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
  dts_ctree <- ctx_tree(dts, min_size = 1, max_depth = 3, backend = "C++")
  expect_snapshot(print(find_sequence(dts_ctree, c(0, 0))))
  expect_snapshot(print(find_sequence(dts_ctree, c(1, 0))))
  expect_snapshot(print(rev(find_sequence(dts_ctree, c(0, 0)))))
  expect_snapshot(print(rev(find_sequence(dts_ctree, c(1, 0)))))
})
