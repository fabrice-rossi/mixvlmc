# "large" state space, large tree
x <- sample(0:9, 50000, replace = TRUE)
r_vlmc <- vlmc(x, alpha = 0.1, backend = "R")
loglikelihood_vlmc_loop_vs_tree <- bench::mark(
  loglikelihood(r_vlmc, newdata = x),
  slow_loglikelihood(r_vlmc, x),
  check = FALSE
)

# simple state space, larger tree
x <- sample(0:1, 10000, replace = TRUE)
r_vlmc <- vlmc(x, backend = "R", alpha = 0.5)
loglikelihood_vlmc_loop_vs_tree_larger <- bench::mark(
  loglikelihood(r_vlmc, newdata = x),
  slow_loglikelihood(r_vlmc, x),
  check = FALSE
)

# smaller tree
s_vlmc <- vlmc(x, backend = "R", alpha = 0.1)
loglikelihood_vlmc_loop_vs_tree_smaller <- bench::mark(
  loglikelihood(s_vlmc, newdata = x),
  slow_loglikelihood(s_vlmc, x),
  check = FALSE
)
