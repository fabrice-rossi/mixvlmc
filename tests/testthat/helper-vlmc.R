build_markov_chain <- function(n, nb_vals, seed = 0) {
  withr::local_seed(seed)
  TM <- matrix(NA, ncol = nb_vals, nrow = nb_vals)
  for (k in 1:nb_vals) {
    TM[k, ] <- runif(nb_vals)
    TM[k, ] <- TM[k, ] / sum(TM[k, ])
  }
  x <- rep(0, n)
  x[1] <- sample(0:(nb_vals - 1), 1)
  for (k in 2:n) {
    x[k] <- sample(0:(nb_vals - 1), 1, prob = TM[x[k - 1] + 1, ])
  }
  list(x = x, TM = TM)
}
