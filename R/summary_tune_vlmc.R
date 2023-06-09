#' @exportS3Method
summary.tune_vlmc <- function(object, ...) {
  res <- object
  res$saved_models <- NULL
  structure(res, class = "summary.tune_vlmc")
}

#' @exportS3Method
print.summary.tune_vlmc <- function(x, ...) {
  cat("VLMC tune results\n\n")
  cat("Best VLMC selected by", x$criterion, "(")
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
