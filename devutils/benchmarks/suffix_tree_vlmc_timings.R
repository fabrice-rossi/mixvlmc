x <- sample(0:9, 10000, replace = TRUE)

st_vlmc_vs_R <- bench::mark(
  vlmc(x, backend = "R"),
  vlmc(x, backend = "C++"),
  check = FALSE
)
