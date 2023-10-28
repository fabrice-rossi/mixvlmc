new_ctx_tree_cpp <- function(vals, suffix_tree, ..., class = character()) {
  assertthat::assert_that(inherits(suffix_tree, "Rcpp_SuffixTree"))
  root <- list(
    root = suffix_tree, vals = vals, depth = suffix_tree$depth(),
    nb_ctx = suffix_tree$nb_contexts()
  )
  preres <- structure(root, ..., class = c(class, "ctx_tree_cpp", "ctx_tree"))
  preres
}

restore_ctx_tree_cpp <- function(tree) {
  if (extptr_is_null(tree$root$.pointer)) {
    cpp_tree <- build_suffix_tree(
      tree$restoration$rev_x,
      tree$restoration$max_x + 1
    )
    cpp_tree$compute_counts(tree$restoration$last_value, tree$keep_match)
    cpp_tree$prune(tree$restoration$min_size, tree$restoration$max_depth)
    tree$root@.xData$.pointer <- cpp_tree@.xData$.pointer
    tree$root@.xData$.cppclass <- cpp_tree@.xData$.cppclass
    tree$root@.xData$.module <- cpp_tree@.xData$.module
  }
}

#' @export
print.ctx_tree_cpp <- function(x, ...) {
  restore_ctx_tree_cpp(x)
  cat(paste(
    "Context tree on",
    paste(x$vals, collapse = ", ")
  ), "[C++]\n")
  if (!is.null(x$nb_ctx)) {
    cat(paste(" Number of contexts:", x$nb_ctx, "\n"))
  }
  if (!is.null(x$depth)) {
    cat(paste(" Maximum context length:", x$depth, "\n"))
  }
  invisible(x)
}
