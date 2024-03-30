Rcpp::loadModule("suffixtree", TRUE)
.onLoad <- function(libname, pkgname) {
  s3_register("ggplot2::autoplot", "tune_vlmc")
  s3_register("ggplot2::autoplot", "tune_covlmc")
  current_op <- options()
  ## default options
  mixvlmc_options <- list(
    mixvlmc.predictive = "glm",
    mixvlmc.maxit = 100,
    mixvlmc.backend = "R",
    mixvlmc.charset = ifelse(cli::is_utf8_output(), "utf8", "ascii")
  )
  to_set <- !(names(mixvlmc_options) %in% names(current_op))
  if (any(to_set)) options(mixvlmc_options[to_set])
  invisible()
}
