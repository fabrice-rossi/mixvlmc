set.seed(0)
x <- sample(0:2, 10000, replace = TRUE)

r_vlmc <- vlmc(x, alpha = 0.1, backend = "R")
cpp_vlmc <- vlmc(x, alpha = 0.1, backend = "C++")

simulate_vlmc_vs_R_small <- bench::mark(
  simulate(r_vlmc, 5000, seed = 0),
  simulate(cpp_vlmc, 5000, seed = 0, sample = "R"),
  check = TRUE
)

fast_simulate_vlmc_vs_R_small <- bench::mark(
  simulate(r_vlmc, 5000, seed = 0),
  simulate(cpp_vlmc, 5000, seed = 0, sample = "fast"),
  check = FALSE
)

r_vlmc <- vlmc(x, alpha = 0.5, backend = "R")
cpp_vlmc <- vlmc(x, alpha = 0.5, backend = "C++")

simulate_vlmc_vs_R_large <- bench::mark(
  simulate(r_vlmc, 5000, seed = 0),
  simulate(cpp_vlmc, 5000, seed = 0, sample = "R"),
  check = TRUE
)

fast_simulate_vlmc_vs_R_large <- bench::mark(
  simulate(r_vlmc, 5000, seed = 0),
  simulate(cpp_vlmc, 5000, seed = 0, sample = "fast"),
  check = FALSE
)
