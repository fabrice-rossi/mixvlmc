#' This function returns a trimmed VLMC from which match positions have been
#' removed.
#'
#' @inherit trim
#' @param ct a VLMC.
#' @return a trimmed VLMC
#' @export
#' @examples
#' ## VLMC trimming is generally useless unless match positions were kept
#' pc <- powerconsumption[powerconsumption$week %in% 5:6, ]
#' dts <- cut(pc$active_power, breaks = 4)
#' model <- vlmc(dts, keep_match = TRUE)
#' print(object.size(model))
#' model <- trim(model)
#' ## memory use should be reduced
#' print(object.size(model))
#' nm_model <- vlmc(dts)
#' print(object.size(nm_model))
#' nm_model <- trim(nm_model)
#' ## no effect when match positions are not kept
#' print(object.size(nm_model))
trim.vlmc <- function(ct, ...) {
  # no specific trimming is needed for now, we just need to remove
  # matches as in a context tree
  rec_trim_ctx_tree(ct)
}
