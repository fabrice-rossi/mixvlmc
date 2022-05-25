test_that("constructor", {
  vals <- c("1", "A", "Q")
  expect_equal(states(new_ctx_tree(vals)), vals)
})

test_that("zero depth", {
  expect_equal(depth(new_ctx_tree(c(0, 1))), 0)
})
