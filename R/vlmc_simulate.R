#' Simulate a discrete time series for a vlmc
#'
#' This function simulates a time series from the distribution estimated by the
#' given vlmc object.
#'
#' The time series can be initiated by a fixed sequence specified via the `init`
#' parameter.
#'
#' @param object a fitted vlmc object.
#' @param nsim length of the simulated time series (defaults to 1).
#' @param seed an optional random seed.
#' @param init an optional initial sequence for the time series
#' @param ... additional arguments.
#'
#' @export
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
#' model <- vlmc(dts, min_size = 5)
#' new_dts <- simulate(model, 500, seed = 0)
#' new_dts_2 <- simulate(model, 500, seed = 0, init = dts[1:5])
simulate.vlmc <- function(object, nsim = 1, seed = NULL, init = NULL, ...) {
  max_depth <- depth(object)
  if (!is.null(seed)) {
    withr::local_seed(seed)
  }
  if (!is.null(init)) {
    assertthat::assert_that((typeof(init) == typeof(object$vals)) && (class(init) == class(object$vals)),
      msg = "init is not compatible with the model state space"
    )
    assertthat::assert_that(length(init) <= nsim, msg = "too many initial values")
    init_dts <- to_dts(init, object$vals)
    ctx <- rev(init_dts$ix)[1:(min(max_depth, length(init)))] + 1
    istart <- 1 + length(init)
  } else {
    ctx <- c()
    istart <- 1
  }
  int_vals <- seq_along(object$vals)
  pre_res <- rep(0, nsim)
  if (!is.null(init)) {
    pre_res[1:length(init)] <- init_dts$ix + 1
  }
  if (istart <= nsim) {
    for (i in (istart):nsim) {
      subtree <- match_context(object, ctx)
      pre_res[i] <- sample(int_vals, 1, prob = subtree$tree$f_by)
      if (length(ctx) < max_depth) {
        ctx <- c(pre_res[i], ctx)
      } else {
        ctx <- c(pre_res[i], ctx[1:(max_depth - 1)])
      }
    }
  }
  object$vals[pre_res]
}
