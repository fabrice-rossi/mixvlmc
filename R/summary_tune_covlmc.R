#' Summary of the model selection of a VLMC with covariates
#'
#' This function summarises the results of [tune_covlmc()]. The associated
#' `print()` method displays the selected model and the characteristics of
#' all the models considered during the selection.
#'
#' @param object the results of [tune_covlmc()] (a `tune_covlmc` object).
#' @param x a summary of the results of [tune_covlmc()], as returned by
#'   `summary()`.
#' @param ... additional arguments (not used).
#'
#' @returns `summary()` returns an object of class `summary.tune_covlmc`: the
#'   results of [tune_covlmc()] without the saved models (see the `save`
#'   parameter of [tune_covlmc()]). In particular, its `results` component is
#'   the data frame that describes the models considered during the selection.
#'
#'   `print()` displays the summary and returns `x` invisibly.
#' @seealso [tune_covlmc()]
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 10, ]
#' rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
#' rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
#' rdts_tune <- tune_covlmc(rdts, rdts_cov, min_size = 5)
#' summary(rdts_tune)
#' @exportS3Method
summary.tune_covlmc <- function(object, ...) {
  rlang::check_dots_empty()
  res <- object
  res$saved_models <- NULL
  structure(res, class = "summary.tune_covlmc")
}

#' @rdname summary.tune_covlmc
#' @exportS3Method
print.summary.tune_covlmc <- function(x, ...) {
  cat("VLMC with covariates tune results\n\n")
  cat("Best VLMC with covariates selected by", x$criterion, "(")
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
