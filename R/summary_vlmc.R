#' @exportS3Method
summary.vlmc <- function(object, ...) {
  rlang::check_dots_empty()
  ## the list of ctx_node is the fastest representation of the contexts
  ## with the R backend
  summary_vlmc_from_lengths(object, vapply(contexts(object), length, 1L))
}

summary_vlmc_from_lengths <- function(object, all_length) {
  res <- list(
    state_space = states(object),
    nb = length(all_length),
    depth = max(all_length),
    avg_depth = mean(all_length),
    cutoff = object$cutoff,
    alpha = object$alpha
  )
  structure(res, class = "summary.vlmc")
}

print_summary_vlmc <- function(x) {
  cat(paste(" Cutoff: ", signif(x$cutoff, 4), " (quantile: ", signif(x$alpha, 4), ")\n", sep = ""))
  cat(paste(" Number of contexts:", x$nb, "\n"))
  cat(paste(" Maximum context length:", x$depth, "\n"))
  cat(paste(" Average context length:", signif(x$avg_depth, 4), "\n"))
}

#' @exportS3Method
print.summary.vlmc <- function(x, ...) {
  cat(paste(
    "VLMC context tree on",
    paste(x$state_space, collapse = ", ")
  ), "\n")
  print_summary_vlmc(x)
  invisible(x)
}
