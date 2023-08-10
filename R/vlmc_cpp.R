#' @export
print.vlmc_cpp <- function(x, ...) {
  cat(paste(
    "VLMC context tree on",
    paste(x$vals, collapse = ", ")
  ), "[C++]\n")
  cat(paste(" cutoff: ", signif(x$cutoff, 4), " (quantile: ", x$alpha, ")\n", sep = ""))
  if (!is.null(x$nb_ctx)) {
    cat(paste(" Number of contexts:", x$nb_ctx, "\n"))
  }
  if (!is.null(x$depth)) {
    cat(paste(" Maximum context length:", x$depth, "\n"))
  }
  invisible(x)
}
