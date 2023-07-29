new_ctx_tree_cpp <- function(vals, suffix_tree, ..., class = character()) {
  assertthat::assert_that(inherits(suffix_tree, "Rcpp_SuffixTree"))
  root <- list(
    root = suffix_tree, vals = vals, depth = suffix_tree$depth(),
    nb_ctx = suffix_tree$nb_contexts()
  )
  preres <- structure(root, ..., class = c(class, "ctx_tree_cpp", "ctx_tree"))
  preres
}

#' @export
print.ctx_tree_cpp <- function(x, ...) {
  cat(paste(
    "Context tree on",
    paste(x$vals, collapse = ", ")
  ), "[C++]\n")
  if (extptr_is_null(x$root$.pointer)) {
    cat("Missing C++ representation!\nThis object was probably restored from a saved object.\n")
  }
  if (!is.null(x$nb_ctx)) {
    cat(paste(" Number of contexts:", x$nb_ctx, "\n"))
  }
  if (!is.null(x$depth)) {
    cat(paste(" Maximum context length:", x$depth, "\n"))
  }
  invisible(x)
}
