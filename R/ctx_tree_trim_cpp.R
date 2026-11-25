#' @export
trim.ctx_tree_cpp <- function(ct, ...) {
  restore_model(ct)
  if (ct$root$has_positions()) {
    ct$root <- ct$root$trim()
  }
  ct
}
