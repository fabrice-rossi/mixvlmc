#' @exportS3Method
summary.covlmc <- function(object, ...) {
  ctx <- contexts(object, frequency = "detailed", hsize = TRUE)
  all_length <- sapply(ctx$context, length)
  delta_cov_depth <- all_length - ctx$hsize
  res <- list(
    state_space = states(object),
    nb = nrow(ctx),
    depth = max(all_length),
    avg_depth = mean(all_length),
    alpha = object$alpha,
    covariates = object$cov_names,
    no_cov = sum(ctx$hsize == 0),
    avg_hsize = mean(ctx$hsize[ctx$hsize > 0]),
    max_hsize = max(ctx$hsize),
    delta = mean(delta_cov_depth[ctx$hsize > 0])
  )
  structure(res, class = "summary.covlmc")
}

#' @exportS3Method
print.summary.covlmc <- function(x, ...) {
  cat(paste(
    "VLMC with covariate context tree on",
    paste(x$state_space, collapse = ", ")
  ), "\n")
  cat(paste(" Covariates:", stringr::str_c(x$covariates, collapse = ", "), "\n"))
  cat(paste(" Cutoff in quantile scale: ", signif(x$alpha, 4), "\n", sep = ""))
  cat(paste(" Number of contexts:", x$nb, "\n"))
  cat(paste(" Contexts without covariate:", x$no_cov, "\n"))
  cat(paste(" Maximum context length:", x$depth, "\n"))
  cat(paste(" Average context length:", signif(x$avg_depth, 4), "\n"))
  if (!is.nan(x$avg_hsize)) {
    cat(paste(" For nodes with covariates:", "\n"))
    cat(paste("  Maximum covariate history length:", x$max_hsize, "\n"))
    cat(paste("  Average covariate history length:", signif(x$avg_hsize, 4), "\n"))
    cat(paste("  Average difference between context length and history length:", signif(x$delta, 4), "\n"))
  }
  invisible(x)
}
