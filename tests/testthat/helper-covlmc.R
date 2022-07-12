build_data_set <- function(n, seed = 0) {
  withr::local_seed(seed)
  blink <- stats::binomial()
  covariate <- data.frame(y = runif(n))
  coeffs <- matrix(c(-1, 2, 1, -2), ncol = 2)
  scores <- cbind(1, covariate$y) %*% coeffs
  probs <- apply(scores, 1, blink$linkinv)
  x <- rep(0, n)
  x[1] <- sample(c(0, 1), 1)
  for (k in 2:n) {
    prob <- probs[x[k - 1] + 1, k - 1]
    x[k] <- sample(c(0, 1), 1, prob = c(1 - prob, prob))
  }
  list(x = x, covariate = covariate)
}
