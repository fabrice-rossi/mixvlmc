#' Summary of the model selection of a VLMC
#'
#' This function summarises the results of [tune_vlmc()]. The associated
#' `print()` method displays the selected model and the characteristics of
#' all the models considered during the selection.
#'
#' @param object the results of [tune_vlmc()] (a `tune_vlmc` object).
#' @param x a summary of the results of [tune_vlmc()], as returned by
#'   `summary()`.
#' @param ... additional arguments (not used).
#'
#' @returns `summary()` returns an object of class `summary.tune_vlmc`: the
#'   results of [tune_vlmc()] without the saved models (see the `save`
#'   parameter of [tune_vlmc()]). In particular, its `results` component is
#'   the data frame that describes the models considered during the selection.
#'
#'   `print()` displays the summary and returns `x` invisibly.
#' @seealso [tune_vlmc()]
#' @examples
#' rdts <- sample(as.factor(c("A", "B", "C")), 100, replace = TRUE)
#' tune_result <- tune_vlmc(rdts)
#' summary(tune_result)
#' @exportS3Method
summary.tune_vlmc <- function(object, ...) {
  rlang::check_dots_empty()
  res <- object
  res$saved_models <- NULL
  structure(res, class = "summary.tune_vlmc")
}

#' @rdname summary.tune_vlmc
#' @exportS3Method
print.summary.tune_vlmc <- function(x, ...) {
  cat("VLMC tune results\n\n")
  cat("Best VLMC selected by", x$criterion, "(")
  if (x$criterion == "BIC") {
    cat(min(x$results$BIC))
  } else {
    cat(min(x$results$AIC))
  }
  cat(") with likelihood function \"", x$initial, "\" (", sep = "")
  cat(tune_best_loglikelihood(x))
  cat(")\n")
  print(x$best_model)
  cat("\nPruning results\n")
  print(x$results, row.names = FALSE)
  invisible(x)
}
