# compute the log likelihood using multiple calls to
# loglikelihood
loglikelihood_multi <- function(model, xs,
                                initial = c("truncated", "specific", "extended"),
                                ignore,
                                weights = NULL) {
  initial <- match.arg(initial)
  ll <- 0
  nb_obs <- 0L
  if (is.null(weights)) {
    weights <- rep(1, length(xs))
  }
  for (k in seq_along(xs)) {
    if (rlang::is_missing(ignore)) {
      the_ll <- loglikelihood.vlmc(model, xs[[k]], initial)
    } else {
      the_ll <- loglikelihood.vlmc(model, xs[[k]], initial, ignore)
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
