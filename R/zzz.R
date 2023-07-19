Rcpp::loadModule("suffixtree", TRUE)
.onLoad <- function(libname, pkgname) {
  current_op <- options()
  ## default options
  mixvlmc_options <- list(
    mixvlmc.predictive = "glm",
    mixvlmc.maxit = 100
  )
  to_set <- !(names(mixvlmc_options) %in% names(current_op))
  if (any(to_set)) options(mixvlmc_options[to_set])
  invisible()
}
