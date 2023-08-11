test_that("the likelihood calculation is valid", {
  x <- sample(c("A", "B", "C"), 1000, replace = TRUE)
  x_tree <- vlmc(x, alpha = 0.05)
  expect_identical(loglikelihood(x_tree, x), loglikelihood(x_tree))
})

test_that("the logLik output format is valid", {
  x <- sample(c(TRUE, FALSE), 1000, replace = TRUE)
  x_tree <- vlmc(x, alpha = 0.05)
  x_tree_ll <- logLik(x_tree)
  expect_s3_class(x_tree_ll, "logLik")
  expect_named(attributes(x_tree_ll), c("df", "nobs", "class"))
})

test_that("the logLik results are correct", {
  withr::local_seed(0)
  for (k in 1:9) {
    x <- sample(0:k, 1000 + 100 * k, replace = TRUE)
    x_tree <- vlmc(x, alpha = 0.05)
    x_tree_ll <- logLik(x_tree)
    expect_equal(attr(x_tree_ll, "nobs"), length(x))
    expect_equal(attr(x_tree_ll, "df"), k * context_number(x_tree))
  }
})

test_that("logLik and loglikelihood report the same value when they should", {
  withr::local_seed(0)
  for (k in 1:9) {
    x <- sample(0:k, 1000 + 100 * k, replace = TRUE)
    x_tree <- vlmc(x, alpha = 0.05)
    expect_equal(as.numeric(logLik(x_tree)), as.numeric(loglikelihood(x_tree)))
  }
})
