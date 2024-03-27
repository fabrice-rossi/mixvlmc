#' Convert an object to a Variable Length Markov Chain with covariates (coVLMC)
#'
#' This generic function converts an object into a covlmc.
#'
#' @param x an object to convert into a covlmc.
#' @param ... additional arguments for conversion functions.
#'
#' @returns a covlmc
#' @export
as_covlmc <- function(x, ...) {
  UseMethod("as_covlmc")
}

#' @export
#' @rdname as_covlmc
#' @seealso [tune_covlmc()]
#' @examples
#' ## conversion from the results of tune_covlmc
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
#' rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
#' rdts_best_model_tune <- tune_covlmc(rdts, rdts_cov)
#' rdts_best_model <- as_covlmc(rdts_best_model_tune)
#' draw(rdts_best_model)
as_covlmc.tune_covlmc <- function(x, ...) {
  x$best_model
}
