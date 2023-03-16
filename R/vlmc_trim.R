#' @export
trim.vlmc <- function(ct, ...) {
  # no specific trimming is needed for now, we just need to remove
  # matches as in a context tree
  rec_trim_ctx_tree(ct)
}
