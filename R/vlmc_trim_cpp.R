#' @details
#' Trimming in the C++ backend is done directly in the `Rcpp` managed memory and
#' cannot be detected at R level using e.g. [utils::object.size()].
#'
#' @inherit trim.vlmc
#' @export
#' @examples
#' ## VLMC trimming is generally useless unless match positions were kept
#' pc <- powerconsumption[powerconsumption$week %in% 5:6, ]
#' dts <- cut(pc$active_power, breaks = 4)
#' model <- vlmc(dts, backend = "C++", keep_match = TRUE)
#' model <- trim(model)
trim.vlmc_cpp <- function(ct, ...) {
  if (extptr_is_null(ct$root$.pointer)) {
    stop("Missing C++ representation!\nThis object was probably restored from a saved object.\n")
  }
  if (ct$root$has_positions) {
    ct$root <- ct$root$trim()
  }
  ct
}
