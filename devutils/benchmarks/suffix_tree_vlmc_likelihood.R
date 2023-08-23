x <- sample(0:9, 50000, replace = TRUE)

r_model <- vlmc(x, alpha = 0.1, backend = "R")
cpp_model <- vlmc(x, alpha = 0.1, backend = "C++")

st_vlmc_vs_R_ll <- bench::mark(
  loglikelihood(r_model, newdata = x),
  loglikelihood(cpp_model, newdata = x),
  check = TRUE
)

st_vlmc_vs_R_logLik <- bench::mark(
  logLik(r_model),
  logLik(cpp_model),
  check = TRUE
)
