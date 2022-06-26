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

full_vlmc <- vlmc(x, alpha = 0.15)
alphas <- cutoff(full_vlmc, mode="quantile")

x_vlmcs <- vector(mode = "list", length(alphas))
prev_model <- full_vlmc
for (k in seq_along(alphas)) {
  prev_model <- prune(prev_model, alpha=alphas[k])
  x_vlmcs[[k]] <- prev_model
}

x_vlmcs_bic <- sapply(x_vlmcs, BIC)
x_vlmcs_aic <- sapply(x_vlmcs, AIC)
x_vlmcs_nb_ctx <- sapply(x_vlmcs, context_number)
x_vlmc <- x_vlmcs[[which.min(x_vlmcs_bic)]]

TM2 <- matrix(c(0.6, 0.4, 0.25, 0.75),
             ncol = 2,
             byrow = TRUE
)
basic2 <- new("markovchain",
             states = c("on", "off"), transitionMatrix = TM2,
             name = "basic2"
)
x2 <- rmarkovchain(1000, basic2)
x2_vlmc <- vlmc(x2, alpha=alphas[which.min(x_vlmcs_bic)])
print(loglikelihood(x_vlmc, x))
print(loglikelihood(x_vlmc, x2))
print(loglikelihood(x2_vlmc, x))
print(loglikelihood(x2_vlmc, x2))
