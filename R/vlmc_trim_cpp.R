#' @details
#' Trimming with the C++ backend is done directly in the memory managed by
#' `Rcpp` and cannot be detected at the R level using e.g.
#' [utils::object.size()].
#'
#' @inherit trim.vlmc
#' @export
#' @examples
#' ## VLMC trimming is generally useless unless match positions were kept
#' pc <- powerconsumption[powerconsumption$week %in% 5:6, ]
#' rdts <- cut(pc$active_power, breaks = 4)
#' model <- vlmc(rdts, backend = "C++", keep_match = TRUE)
#' model <- trim(model)
trim.vlmc_cpp <- function(ct, ...) {
  trim.ctx_tree_cpp(ct, ...)
}
