test_that("the constructor keeps the state space", {
  vals <- c("1", "A", "Q")
  expect_identical(states(new_ctx_tree(vals)), vals)
})

test_that("the empty constructor returns a zero depth tree", {
  expect_identical(depth(new_ctx_tree(c(0, 1))), 0)
})

test_that("the depth is calculated correctly", {
  expect_identical(depth(build_demo_tree(1:3, 4)), 4)
  expect_identical(depth(build_demo_tree(1:4, 2)), 2)
  expect_identical(depth(build_demo_tree(c("a", "b"), 3)), 3)
})

test_that("the context number is calculted correctly", {
  expect_identical(context_number(build_demo_tree(1:3, 4)), 3^4)
  expect_identical(context_number(build_demo_tree(1:4, 2)), 4^2)
  expect_identical(context_number(build_demo_tree(c("a", "b"), 3)), 2^3)
})

test_that("the context tree has the correct class", {
  dts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
  dts_ctree <- ctx_tree(dts, min_size = 1, max_depth = 2)
  expect_s3_class(dts_ctree, "ctx_tree", exact = TRUE)
})

test_that("the context tree construction finds the specified contexts", {
  dts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
  dts_ctree <- ctx_tree(dts, min_size = 1, max_depth = 2)
  contexts <- list(c("0", "0"), c("0", "1"), c("1", "0"), c("1", "1"))
  expect_identical(contexts(dts_ctree), contexts)
})

test_that("a warning is given when the state space is large", {
  dts <- rep(1:20, 2)
  expect_warning({
    dts_ctree <- ctx_tree(dts, min_size = 1, max_depth = 2)
  })
})
