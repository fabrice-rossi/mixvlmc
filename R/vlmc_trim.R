#' @rdname trim
#' @export
#' @examples
#' ## VLMC trimming is generally useless
#' pc <- powerconsumption[powerconsumption$week %in% 5:6, ]
#' dts <- cut(pc$active_power, breaks = 4)
#' model <- vlmc(dts)
#' print(object.size(model))
#' model <- trim(model)
#' print(object.size(model))
trim.vlmc <- function(ct, ...) {
  # no specific trimming is needed for now, we just need to remove
  # matches as in a context tree
  rec_trim_ctx_tree(ct)
}
