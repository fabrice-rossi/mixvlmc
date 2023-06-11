#' @exportS3Method
summary.tune_covlmc <- function(object, ...) {
  res <- object
  res$saved_models <- NULL
  structure(res, class = "summary.tune_covlmc")
}

#' @exportS3Method
print.summary.tune_covlmc <- function(x, ...) {
  cat("VLMC with covariate tune results\n\n")
  cat("Best VLMC with covariate selected by", x$criterion, "(")
  if (x$criterion == "BIC") {
    cat(min(x$results$BIC))
  } else {
    cat(min(x$results$AIC))
  }
  cat(")\n")
  print(x$best_model)
  cat("\nPruning results\n")
  print(x$results, row.names = FALSE)
  invisible(x)
}
