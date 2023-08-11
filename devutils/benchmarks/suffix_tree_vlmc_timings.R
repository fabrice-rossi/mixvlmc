x <- sample(0:9, 10000, replace = TRUE)

st_vlmc_vs_R <- bench::mark(
  vlmc(x, backend = "R"),
  vlmc(x, backend = "C++"),
  check = FALSE
)

x <- sample(0:1, 10000, replace = TRUE)
r_vlmc <- vlmc(x, backend = "R", alpha = 0.5)
cpp_vlmc <- vlmc(x, backend = "C++", alpha = 0.5)
## the repetition is in favor of cpp_vlmc as we need to compute the R
## representation only once
loglik_vlmc_vs_R <- bench::mark(
  logLik(r_vlmc),
  logLik(cpp_vlmc),
  check = TRUE
)
## full construction
full_vlmc_vs_R <- bench::mark(
  vlmc(x, backend = "R", alpha = 0.5),
  vlmc(x, backend = "C++", alpha = 0.5),
  check = FALSE
)
## full construction + logLik
full_loglik_vlmc_vs_R <- bench::mark(
  logLik(vlmc(x, backend = "R", alpha = 0.5)),
  logLik(vlmc(x, backend = "C++", alpha = 0.5)),
  check = TRUE
)
