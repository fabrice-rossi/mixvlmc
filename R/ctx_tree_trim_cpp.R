#' @export
trim.ctx_tree_cpp <- function(ct, ...) {
  restore_model(ct)
  if (ct$root$has_positions()) {
    ct$root <- ct$root$trim()
  }
  ## positions must not be computed when the tree is restored
  ct$keep_match <- FALSE
  ct
}
