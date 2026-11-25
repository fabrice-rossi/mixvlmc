test_that("vlmc estimation works on super simple case", {
  x <- rep(c(0, 1), 1000)
  x_vlmc <- vlmc(x)
  expect_identical(depth(x_vlmc), 1L)
  expect_identical(context_number(x_vlmc), 2L)
  x_vlmc_ctx <- contexts(x_vlmc)
  expect_identical(length(x_vlmc_ctx), 2L)
  expect_identical(as_sequence(x_vlmc_ctx[[1]]), 0)
  expect_identical(as_sequence(x_vlmc_ctx[[2]]), 1)
})

test_that("vlmc reports correctly the empty context if it exists", {
  withr::local_seed(0)
  rdts <- sample(0:1, size = 100, replace = TRUE)
  empty_vlmc <- vlmc(rdts, max_depth = 0)
  expect_equal(context_number(empty_vlmc), 1L)
})

test_that("post pruning is equivalent to direct pruning (alpha)", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  model <- vlmc(rdts, alpha = 0.1, keep_match = TRUE)
  cut_off <- cutoff(model)
  for (k in seq_along(cut_off)) {
    pruned_model <- prune(model, alpha = cut_off[k])
    direct_model <- vlmc(rdts, alpha = cut_off[k], keep_match = TRUE)
    expect_true(compare_vlmc(pruned_model, direct_model))
  }
})

test_that("post pruning is equivalent to direct pruning (cutoff)", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  model <- vlmc(rdts, alpha = 0.1)
  cut_off <- cutoff(model, scale = "native")
  for (k in seq_along(cut_off)) {
    pruned_model <- prune(model, cutoff = cut_off[k])
    direct_model <- vlmc(rdts, cutoff = cut_off[k])
    expect_true(compare_vlmc(pruned_model, direct_model))
  }
})

test_that("alpha pruning is equivalent to cutoff pruning", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  model <- vlmc(rdts, alpha = 0.1)
  c_cut_off <- cutoff(model, scale = "native")
  a_cut_off <- cutoff(model, scale = "quantile")
  expect_length(a_cut_off, length(c_cut_off))
  for (k in seq_along(a_cut_off)) {
    a_pruned_model <- prune(model, alpha = a_cut_off[k])
    c_pruned_model <- prune(model, cutoff = c_cut_off[k])
    expect_equal(a_pruned_model, c_pruned_model)
  }
})

test_that("max_depth is always reported", {
  data_set <- build_markov_chain(500, 2, seed = 6)
  model <- vlmc(data_set$x, cutoff = 0.5 * log(length(data_set$x)), max_depth = 2)
  expect_true(model$max_depth)
})

test_that("vlmc always returns a vlmc object", {
  data_set <- build_markov_chain(500, 2, seed = 6)
  model <- vlmc(data_set$x, cutoff = 0.5 * log(length(data_set$x)), max_depth = 2, prune = FALSE)
  expect_s3_class(model, "vlmc")
})

test_that("vlmc returns an object with all the needed internal fields", {
  data_set <- build_markov_chain(500, 2, seed = 6)
  model <- vlmc(data_set$x, cutoff = 0.1, keep_match = TRUE)
  expect_named(model, c(
    "children", "f_by", "max_depth", "vals", "depth",
    "nb_ctx", "alpha", "cutoff", "ix", "extended_ll",
    "keep_match", "data_size", "pruned"
  ), ignore.order = TRUE)
})

test_that("vlmc results do not depend on the time series representation", {
  data_set <- build_markov_chain(500, 2, seed = 6)
  model <- vlmc(data_set$x,
    cutoff = 0.1 * log(length(data_set$x)),
    max_depth = 4, prune = FALSE
  )
  dts_model <- vlmc(dts(data_set$x),
    cutoff = 0.1 * log(length(data_set$x)),
    max_depth = 4, prune = FALSE
  )
  expect_identical(model, dts_model)
})

test_that("states that do not appear in the time series are supported", {
  withr::local_seed(0)
  x <- character(500)
  x[1] <- "a"
  for (i in 2:500) {
    if (x[i - 1] == "a") {
      x[i] <- sample(c("a", "b"), 1, prob = c(0.2, 0.8))
    } else {
      x[i] <- sample(c("a", "b"), 1, prob = c(0.7, 0.3))
    }
  }
  x2 <- factor(x, levels = c("a", "b"))
  x3 <- factor(x, levels = c("a", "b", "c"))
  ## as "c" never appears, each internal node of the context tree is also a
  ## context (that does not match any part of the time series): the context
  ## trees differ from the ones obtained with only "a" and "b" but the
  ## likelihoods are identical
  nb_ctx <- list()
  for (backend in c("R", "C++")) {
    ct3 <- ctx_tree(x3, max_depth = 3, backend = backend)
    c3 <- contexts(ct3, frequency = "detailed")
    expect_true(all(c3[, 5] == 0))
    m2 <- vlmc(x2, cutoff = 2, backend = backend)
    m3 <- vlmc(x3, cutoff = 2, backend = backend)
    expect_gt(depth(m3), 0)
    expect_equal(depth(m3), depth(m2))
    for (initial in c("truncated", "specific", "extended")) {
      expect_equal(
        as.numeric(loglikelihood(m3, initial = initial)),
        as.numeric(loglikelihood(m2, initial = initial))
      )
    }
    x3_sim <- simulate(m3, 100, seed = 0)
    expect_false(any(x3_sim == "c"))
    nb_ctx[[backend]] <- c(context_number(ct3), context_number(m3))
  }
  expect_equal(nb_ctx[["R"]], nb_ctx[["C++"]])
  for (backend in c("R", "C++")) {
    expect_no_error(m_lgl <- vlmc(rep(TRUE, 20), backend = backend))
    expect_equal(context_number(m_lgl), 1)
  }
})

test_that("min_size and max_depth are validated", {
  x <- rep(c(0, 1, 1), 10)
  for (backend in c("R", "C++")) {
    for (ms in list(0, -5, NA, c(2, 3), "2")) {
      expect_error(ctx_tree(x, min_size = ms, max_depth = 3, backend = backend), "min_size")
      expect_error(vlmc(x, min_size = ms, max_depth = 3, backend = backend), "min_size")
    }
    for (md in list(-1, 2.5, NA, c(2, 3), "2", Inf)) {
      expect_error(ctx_tree(x, max_depth = md, backend = backend), "max_depth")
      expect_error(vlmc(x, max_depth = md, backend = backend), "max_depth")
    }
  }
  expect_error(tune_vlmc(x, min_size = 0, max_depth = 3), "min_size")
  expect_error(tune_vlmc(x, max_depth = -1), "max_depth")
})

test_that("time series with a single state are rejected", {
  for (x in list(rep("a", 50), rep(1, 50), factor(rep("a", 50)))) {
    for (backend in c("R", "C++")) {
      expect_error(ctx_tree(x, backend = backend), "at least two states")
      expect_error(vlmc(x, backend = backend), "at least two states")
      expect_error(tune_vlmc(x, backend = backend), "at least two states")
    }
    expect_error(covlmc(x, data.frame(y = seq_along(x))), "at least two states")
    expect_error(tune_covlmc(x, data.frame(y = seq_along(x))), "at least two states")
  }
  ## a single observed state in a larger state space is supported
  x <- factor(rep("a", 50), levels = c("a", "b"))
  for (backend in c("R", "C++")) {
    model <- vlmc(x, backend = backend)
    expect_equal(depth(model), 0)
    expect_equal(as.numeric(loglikelihood(model)), 0)
  }
})
