library(markovchain)
## basic order 1 markov chain
## transition matrix
TM <- matrix(c(0.7, 0.3, 0.4, 0.6),
  ncol = 2,
  byrow = TRUE
)
basic <- new("markovchain",
  states = c("on", "off"), transitionMatrix = TM,
  name = "basic"
)

set.seed(0)
x <- rmarkovchain(1000, basic)
x_mc <- markovchainFit(x)
x_homc <- fitHigherOrder(x, 2)

full_vlmc <- vlmc(x, alpha = 0.2)
alphas <- cutoff(full_vlmc, mode="quantile")

x_vlmcs <- vector(mode = "list", length(alphas))
prev_model <- full_vlmc
for (k in seq_along(alphas)) {
  prev_model <- prune(prev_model, alpha=alphas[k])
  x_vlmcs[[k]] <- prev_model
}

x_vlmcs_bic <- sapply(x_vlmcs, BIC)
x_vlmcs_nb_ctx <- sapply(x_vlmcs, context_number)
