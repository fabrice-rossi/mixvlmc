test_that("the constructor keeps the state space", {
  vals <- c("1", "A", "Q")
  expect_identical(states(new_ctx_tree(vals)), vals)
})

test_that("the empty constructor returns a zero depth tree", {
  expect_identical(depth(new_ctx_tree(c(0, 1))), 0)
})
