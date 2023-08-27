x <- sample(0:3, 2000, replace = TRUE)

tune_vlmc_vs_R <- bench::mark(
  tune_vlmc(x, backend = "R"),
  tune_vlmc(x, backend = "C++"),
  check = FALSE
)
