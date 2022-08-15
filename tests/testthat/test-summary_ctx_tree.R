test_that("summary reports correctly its content", {
  withr::local_seed(0)
  dts <- sample(c(0L, 1L), 100, replace = TRUE)
  dts_tree <- ctx_tree(dts, max_depth = 5, min_size = 2)
  expect_snapshot(summary(dts_tree))
})
