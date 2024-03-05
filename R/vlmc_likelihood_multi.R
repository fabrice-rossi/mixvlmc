#' @inherit loglikelihood
#' @param newdata either a discrete time series or a list of discrete time
#'   series
#' @param weights optional weights for the time series, see details.
#' @description This function evaluates the log-likelihood of a VLMC fitted on a
#'   collection of discrete time series.
#'
#' @section Limitation:
#'
#'   VLMC fitted via [multi_vlmc()] on a collection discrete time series do not
#'   support likelihood calculation without `newdata`.
#'
#' @section Weights:
#'
#'   When specified, the weights are used to compute a weighted sum of the log
#'   likelihood of the time series. The weights are interpreted as fractional
#'   instances and the number of observations is therefore computed accordingly.
#'
#' @seealso [multi_vlmc()]
#' @export
loglikelihood.multi_vlmc <- function(vlmc, newdata,
                                     initial = c("truncated", "specific", "extended"),
                                     ignore, weights = NULL, ...) {
  assertthat::assert_that(!missing(newdata))
  if (!is.list(newdata)) {
    NextMethod()
  } else {
    initial <- match.arg(initial)
    ll <- 0
    nb_obs <- 0L
    if (is.null(weights)) {
      weights <- rep(1, length(newdata))
    }
    for (k in seq_along(newdata)) {
      if (rlang::is_missing(ignore)) {
        the_ll <- loglikelihood(vlmc, newdata[[k]], initial, ...)
      } else {
        the_ll <- loglikelihood(vlmc, newdata[[k]], initial, ignore, ...)
      }
      ll <- ll + weights[k] * as.numeric(the_ll)
      nb_obs <- nb_obs + weights[k] * attr(the_ll, "nobs")
    }
    df <- attr(the_ll, "df")
    attr(ll, "df") <- df
    attr(ll, "nobs") <- nb_obs
    attr(ll, "initial") <- initial
    structure(ll, class = c("logLikMixVLMC", "logLik"))
  }
}
