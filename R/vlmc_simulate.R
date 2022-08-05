#' Simulate a discrete time series for a vlmc
#'
#' This function simulates a time series from the distribution estimated by the
#' given vlmc object.
#'
#' @param object a fitted covlmc object.
#' @param nsim length of the simulated time series (defaults to 1).
#' @param seed an optional random seed.
#' @param ... additional arguments.
#'
#' @export
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
#' model <- vlmc(dts, min_size = 5)
#' new_dts <- simulate(model, 500, seed = 0)
simulate.vlmc <- function(object, nsim = 1, seed = NULL, ...) {
  if (!is.null(seed)) {
    withr::local_seed(seed)
  }
  int_vals <- seq_along(object$vals)
  ctx <- c()
  pre_res <- rep(0, nsim)
  max_depth <- depth(object)
  for (i in 1:nsim) {
    subtree <- match_context(object, ctx)
    pre_res[i] <- sample(int_vals, 1, prob = subtree$tree$f_by)
    if (length(ctx) < max_depth) {
      ctx <- c(pre_res[i], ctx)
    } else {
      ctx <- c(pre_res[i], ctx[1:(max_depth - 1)])
    }
  }
  object$vals[pre_res]
}
