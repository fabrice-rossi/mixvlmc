#' Convert an object to a Variable Length Markov Chain with covariates (coVLMC)
#'
#' This generic function converts an object into a covlmc.
#'
#' @param x an object to convert into a covlmc.
#' @param ... additional arguments for conversion functions.
#'
#' @return a covlmc
#' @export
as_covlmc <- function(x, ...) {
  UseMethod("as_covlmc")
}

#' @inherit as_covlmc
#' @export
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
#' dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
#' dts_best_model_tune <- tune_covlmc(dts, dts_cov)
#' dts_best_model <- as_covlmc(dts_best_model_tune)
#' draw(dts_best_model)
as_covlmc.tune_covlmc <- function(x, ...) {
  x$best_model
}
