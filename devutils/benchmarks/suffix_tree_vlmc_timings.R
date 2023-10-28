x <- sample(0:9, 10000, replace = TRUE)

vlmc_vs_R_small <- bench::mark(
  vlmc(x, backend = "R"),
  vlmc(x, backend = "C++"),
  check = FALSE
)

r_vlmc <- vlmc(x, backend = "R")
cpp_vlmc <- vlmc(x, backend = "C++")

vlmc_cutoff_small <- bench::mark(
  cutoff(r_vlmc),
  cutoff(cpp_vlmc),
  check = FALSE
)

## larger tree
vlmc_vs_R_large <- bench::mark(
  vlmc(x, backend = "R", alpha = 0.5),
  vlmc(x, backend = "C++", alpha = 0.5),
  check = FALSE
)

r_vlmc <- vlmc(x, backend = "R", alpha = 0.5)
cpp_vlmc <- vlmc(x, backend = "C++", alpha = 0.5)

vlmc_cutoff_large <- bench::mark(
  cutoff(r_vlmc),
  cutoff(cpp_vlmc),
  check = FALSE
)
