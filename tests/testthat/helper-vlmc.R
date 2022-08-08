build_markov_chain <- function(n, nb_vals, seed = 0) {
  nb_vals <- as.integer(nb_vals)
  withr::local_seed(seed)
  TM <- matrix(NA, ncol = nb_vals, nrow = nb_vals)
  for (k in 1:nb_vals) {
    TM[k, ] <- runif(nb_vals)
    TM[k, ] <- TM[k, ] / sum(TM[k, ])
  }
  x <- rep(0L, n)
  x[1] <- sample(0L:(nb_vals - 1L), 1)
  for (k in 2:n) {
    x[k] <- sample(0L:(nb_vals - 1L), 1, prob = TM[x[k - 1] + 1, ])
  }
  list(x = x, TM = TM)
}
