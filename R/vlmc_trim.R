#' Trim a VLMC
#'
#' This function returns a trimmed VLMC from which the positions of the contexts
#' in the time series (see the `keep_match` parameter of [vlmc()]) have been
#' removed.
#'
#' @param ct a VLMC.
#' @param ... additional arguments (not used).
#' @return a trimmed VLMC.
#' @export
#' @examples
#' ## VLMC trimming is generally useless unless match positions were kept
#' pc <- powerconsumption[powerconsumption$week %in% 5:6, ]
#' rdts <- cut(pc$active_power, breaks = 4)
#' model <- vlmc(rdts, keep_match = TRUE)
#' print(object.size(model))
#' model <- trim(model)
#' ## memory use should be reduced
#' print(object.size(model))
#' nm_model <- vlmc(rdts)
#' print(object.size(nm_model))
#' nm_model <- trim(nm_model)
#' ## no effect when match positions are not kept
#' print(object.size(nm_model))
trim.vlmc <- function(ct, ...) {
  rlang::check_dots_empty()
  # no specific trimming is needed for now, we just need to remove
  # matches as in a context tree
  result <- rec_trim_ctx_tree(ct)
  result$keep_match <- FALSE
  result
}
