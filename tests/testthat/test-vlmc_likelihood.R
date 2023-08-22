test_that("loglikelihood computes the expected values", {
  withr::local_seed(0)
  for (k in 1:9) {
    x <- sample(0:k, 1000 + 100 * k, replace = TRUE)
    x_tree <- vlmc(x, alpha = 0.1)
    for (initial in c("truncated", "specific", "extended")) {
      fll <- loglikelihood(x_tree, newdata = x, initial = initial)
      sll <- slow_loglikelihood(x_tree, x, initial = initial)
      expect_equal(as.numeric(fll), as.numeric(sll))
      expect_equal(attr(fll, "nobs"), attr(sll, "nobs"))
    }
  }
})

test_that("the loglikelihood output format is valid", {
  x <- sample(c("A", "B", "C"), 1000, replace = TRUE)
  x_tree <- vlmc(x, alpha = 0.05)
  for (initial in c("truncated", "specific", "extended")) {
    x_tree_ll <- loglikelihood(x_tree, initial = initial)
    expect_s3_class(x_tree_ll, "logLikMixVLMC")
    expect_s3_class(x_tree_ll, "logLik")
    expect_named(attributes(x_tree_ll), c("df", "nobs", "class", "initial"),
      ignore.order = TRUE
    )
  }
})

test_that("the logLik output format is valid", {
  x <- sample(c(TRUE, FALSE), 1000, replace = TRUE)
  x_tree <- vlmc(x, alpha = 0.05)
  x_tree_ll <- logLik(x_tree)
  expect_s3_class(x_tree_ll, "logLik")
  expect_named(attributes(x_tree_ll), c("df", "nobs", "class", "initial"),
    ignore.order = TRUE
  )
})

test_that("logLik and loglikelihood report the same value when they should", {
  withr::local_seed(0)
  for (k in 1:9) {
    x <- sample(0:k, 1000 + 100 * k, replace = TRUE)
    x_tree <- vlmc(x, alpha = 0.05)
    for (initial in c("truncated", "specific", "extended")) {
      expect_equal(
        as.numeric(logLik(x_tree, initial = initial)),
        as.numeric(loglikelihood(x_tree, initial = initial))
      )
    }
  }
})

test_that("loglikelihood supports short sequences", {
  withr::local_seed(0)
  x <- sample(c("A", "B", "C"), 1000, replace = TRUE)
  x_tree <- vlmc(x, alpha = 0.05)
  expect_no_warning(loglikelihood(x_tree, newdata = x[1]))
})
