test_that("printing works as expected", {
  dts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
  withr::local_options("mixvlmc.backend" = "C++")
  dts_ctree <- ctx_tree(dts, min_size = 1, max_depth = 2)
  expect_snapshot_output(print(dts_ctree))
})
