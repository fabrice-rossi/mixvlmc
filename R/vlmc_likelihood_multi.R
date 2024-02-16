#' @inherit loglikelihood
#' @param newdata either a discrete time series or a list of discrete time
#'   series
#' @description This function evaluates the log-likelihood of a VLMC fitted on a
#'   collection of discrete time series.
#'
#' @section Limitation:
#'
#'   VLMC fitted via [multi_vlmc()] on a collection discrete time series do not
#'   support likelihood calculation with `newdata`.
#'
#' @seealso [multi_vlmc()]
#' @export
loglikelihood.multi_vlmc <- function(vlmc, newdata, initial = c("truncated", "specific", "extended"),
                                     ignore, ...) {
  assertthat::assert_that(!missing(newdata))
  if (!is.list(newdata)) {
    NextMethod()
  } else {
    initial <- match.arg(initial)
    ll <- 0
    nb_obs <- 0L
    for (k in seq_along(newdata)) {
      if (missing(ignore)) {
        the_ll <- loglikelihood(vlmc, newdata[[k]], initial, ...)
      } else {
        the_ll <- loglikelihood(vlmc, newdata[[k]], initial, ignore, ...)
      }
      ll <- ll + as.numeric(the_ll)
      nb_obs <- nb_obs + attr(the_ll, "nobs")
    }
    df <- attr(the_ll, "df")
    attr(ll, "df") <- df
    attr(ll, "nobs") <- nb_obs
    attr(ll, "initial") <- initial
    structure(ll, class = c("logLikMixVLMC", "logLik"))
  }
}
