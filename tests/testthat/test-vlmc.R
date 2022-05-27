test_that("the likelihood calculation is valid", {
  x <- sample(c("A", "B", "C"), 1000, replace=TRUE)
  x_tree <- vlmc(x, alpha=0.05)
  expect_identical(loglikelihood(x_tree, x), loglikelihood(x_tree))
})
