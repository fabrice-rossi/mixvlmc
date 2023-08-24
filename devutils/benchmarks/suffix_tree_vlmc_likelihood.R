x <- sample(0:9, 50000, replace = TRUE)

r_vlmc <- vlmc(x, alpha = 0.1, backend = "R")
cpp_vlmc <- vlmc(x, alpha = 0.1, backend = "C++")

loglik_vlmc_vs_R_small <- bench::mark(
  logLik(r_vlmc),
  logLik(cpp_vlmc),
  check = TRUE
)

loglikelihood_vlmc_vs_R_small <- bench::mark(
  loglikelihood(r_vlmc, newdata = x),
  loglikelihood(cpp_vlmc, newdata = x),
  check = TRUE
)

## larger tree
x <- sample(0:1, 10000, replace = TRUE)
r_vlmc <- vlmc(x, backend = "R", alpha = 0.5)
cpp_vlmc <- vlmc(x, backend = "C++", alpha = 0.5)
loglik_vlmc_vs_R_large <- bench::mark(
  logLik(r_vlmc),
  logLik(cpp_vlmc),
  check = TRUE
)

loglikelihood_vlmc_vs_R_large <- bench::mark(
  loglikelihood(r_vlmc, newdata = x),
  loglikelihood(cpp_vlmc, newdata = x),
  check = TRUE
)
