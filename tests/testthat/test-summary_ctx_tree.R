test_that("summary reports correctly its content", {
  withr::local_seed(0)
  rdts <- sample(c(0L, 1L), 100, replace = TRUE)
  rdts_tree <- ctx_tree(rdts, max_depth = 5, min_size = 2)
  expect_snapshot(summary(rdts_tree))
})
