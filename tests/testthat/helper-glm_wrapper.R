build_binary_dataset <- function(seed = 0) {
  withr::local_seed(seed)
  beta <- c(-2, 2)
  x <- matrix(rnorm(500), ncol = 2)
  score <- x %*% beta
  prob <- 1 / (1 + exp(-score[, 1]))
  target <- as.integer(stats::runif(length(prob)) < prob)
  test_x <- matrix(rnorm(400), ncol = 2)
  test_score <- test_x %*% beta
  test_prob <- 1 / (1 + exp(-test_score[, 1]))
  test_target <- as.integer(stats::runif(length(test_prob)) < test_prob)
  list(
    target = target, mm = as.data.frame(x),
    test_target = test_target, test_mm = as.data.frame(test_x)
  )
}

build_multilevel_dataset <- function(nb_levels, seed = 0) {
  withr::local_seed(seed)
  nb_vars <- nb_levels
  nb_coeffs <- (nb_levels - 1) * nb_vars
  beta <- matrix(
    sample(c(-1, 1), nb_coeffs, replace = TRUE) *
      stats::runif(min = 1, max = 2, nb_coeffs),
    ncol = nb_levels - 1
  )
  x <- matrix(rnorm(500 * nb_vars), ncol = nb_vars)
  score <- x %*% beta
  exp_prob <- exp(-score)
  P_1 <- 1 / (1 + rowSums(exp_prob))
  probs <- cbind(P_1, sweep(exp_prob, 1, P_1, "*"))
  target <- apply(probs, 1, \(x) sample(1:nb_levels, 1, prob = x))
  vals <- paste0("l", 1:nb_levels)
  target <- factor(vals[target], levels = vals)
  x <- as.data.frame(x)
  list(
    target = target[1:250], mm = x[1:250, ],
    test_target = target[251:500], test_mm = x[251:500, ],
    probs = probs,
    vals = vals
  )
}

expect_probabilities <- function(probs, precision = .Machine$double.eps^0.5) {
  if (is.matrix(probs)) {
    ones <- rowSums(probs)
    names(ones) <- NULL
    expect_equal(ones, rep(1, nrow(probs)), tolerance = precision)
  }
  expect_true(all(probs > -precision))
  expect_true(all(probs < 1 + precision))
}
