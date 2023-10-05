#' @exportS3Method
summary.ctx_tree <- function(object, ...) {
  ctx <- contexts(object)
  all_length <- sapply(ctx, length)
  res <- list(
    state_space = states(object),
    nb = length(ctx),
    depth = max(all_length),
    avg_depth = mean(all_length)
  )
  structure(res, class = "summary.ctx_tree")
}

#' @exportS3Method
print.summary.ctx_tree <- function(x, ...) {
  cat(paste(
    "Context tree on",
    paste(x$state_space, collapse = ", ")
  ), "\n")
  cat(paste(" Number of contexts:", x$nb, "\n"))
  cat(paste(" Maximum context length:", x$depth, "\n"))
  cat(paste(" Average context length:", x$avg_depth, "\n"))
  invisible(x)
}
