#' @exportS3Method
summary.ctx_tree_cpp <- function(object, ...) {
  res <- NextMethod()
  class(res) <- c("summary.ctx_tree_cpp", class(res))
  res
}

#' @exportS3Method
print.summary.ctx_tree_cpp <- function(x, ...) {
  cat(paste(
    "Context tree on",
    paste(x$state_space, collapse = ", ")
  ), "[C++]\n")
  print_summary_ctx_tree(x)
  invisible(x)
}
