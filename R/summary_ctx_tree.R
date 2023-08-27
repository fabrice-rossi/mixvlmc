#' @exportS3Method
summary.ctx_tree <- function(object, ...) {
  ctx <- contexts(object, frequency = "detailed")
  all_length <- sapply(ctx$context, length)
  res <- list(
    state_space = states(object),
    nb = nrow(ctx),
    depth = max(all_length),
    avg_depth = mean(all_length)
  )
  structure(res, class = "summary.ctx_tree")
}

print_summary_ctx_tree <- function(x) {
  cat(paste(" Number of contexts:", x$nb, "\n"))
  cat(paste(" Maximum context length:", x$depth, "\n"))
  cat(paste(" Average context length:", signif(x$avg_depth, 4), "\n"))
}

#' @exportS3Method
print.summary.ctx_tree <- function(x, ...) {
  cat(paste(
    "Context tree on",
    paste(x$state_space, collapse = ", ")
  ), "\n")
  print_summary_ctx_tree(x)
  invisible(x)
}
