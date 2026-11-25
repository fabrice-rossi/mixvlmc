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

test_that("loglikelihood supports short sequences for extended/specific functions", {
  withr::local_seed(0)
  x <- sample(c("A", "B", "C"), 1000, replace = TRUE)
  x_tree <- vlmc(x, alpha = 0.05)
  for (initial in c("specific", "extended")) {
    expect_no_warning(loglikelihood(x_tree, newdata = x[1], initial = initial))
  }
})

test_that("loglikelihood results print as expected", {
  withr::local_seed(0)
  x <- sample(c("A", "B", "C"), 1000, replace = TRUE)
  x_tree <- vlmc(x, alpha = 0.05)
  for (initial in c("truncated", "specific", "extended")) {
    x_tree_ll <- loglikelihood(x_tree, initial = initial)
    expect_snapshot(print(x_tree_ll))
  }
})

test_that("loglikelihood ignores the requested number of observation", {
  withr::local_seed(0)
  for (k in 1:9) {
    x <- sample(0:k, 1000 + 100 * k, replace = TRUE)
    x_tree <- vlmc(x, alpha = 0.1)
    for (initial in c("truncated", "specific", "extended")) {
      to_ignore <- depth(x_tree) + sample(1:50, 1)
      fll <- loglikelihood(x_tree, newdata = x, initial = initial, ignore = to_ignore)
      sll <- slow_loglikelihood(x_tree, x, initial = initial, ignore = to_ignore)
      expect_equal(as.numeric(fll), as.numeric(sll))
      expect_equal(attr(fll, "nobs"), attr(sll, "nobs"))
    }
  }
})

test_that("impossible transitions in new data have a null likelihood", {
  x <- rep(c("B", "A", "C", "A", "D", "C", "B", "B", "A", "C"), 50)
  m <- vlmc(x)
  m_cpp <- vlmc(x, backend = "C++")
  expect_gte(depth(m), 2)
  ## D is always followed by C in x: impossible transition at position 12
  body_imp <- c(x[1:10], "D", "B", x[1:10])
  ## A is never followed by A in x: impossible transition at position 2, among
  ## the initial observations for which contexts are extended
  init_imp <- c("A", "A", x[1:30])
  for (model in list(m, m_cpp)) {
    for (initial in c("truncated", "specific", "extended")) {
      expect_identical(
        as.numeric(loglikelihood(model, newdata = body_imp, initial = initial)),
        -Inf
      )
    }
    expect_identical(
      as.numeric(loglikelihood(model, newdata = init_imp, initial = "extended")),
      -Inf
    )
    ## ignored observations do not contribute to the likelihood (the impossible
    ## transition also impacts the contexts of the following depth(m) values)
    expect_true(is.finite(loglikelihood(model,
      newdata = body_imp, initial = "extended",
      ignore = 12 + depth(model)
    )))
  }
  for (initial in c("truncated", "specific")) {
    ll <- loglikelihood(m, newdata = init_imp, initial = initial)
    expect_true(is.finite(ll))
    expect_equal(ll, loglikelihood(m_cpp, newdata = init_imp, initial = initial))
  }
  for (ignore in 2:12) {
    expect_equal(
      loglikelihood(m, newdata = body_imp, initial = "extended", ignore = ignore),
      loglikelihood(m_cpp, newdata = body_imp, initial = "extended", ignore = ignore)
    )
  }
})
