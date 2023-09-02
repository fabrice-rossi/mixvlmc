#' Simulate a discrete time series for a vlmc
#'
#' This function simulates a time series from the distribution estimated by the
#' given vlmc object.
#'
#' The time series can be initiated by a fixed sequence specified via the `init`
#' parameter.
#'
#' @section Burn in (Warm up) period:
#'
#' When using a VLMC for simulation purposes, we are generally interested in the
#' stationary distribution of the corresponding Markov chain. To reduce the
#' dependence of the samples from the initial values and get closer to this
#' stationary distribution (if it exists), it is recommended to discard the first
#' samples which are produced in a so-called "burn in" (or "warm up") period. The
#' `burnin` parameter can be used to implement this approach. The VLMC is used
#' to produce a sample of size `burnin + nsim` but the first `burnin` values are
#' discarded. Notice that this burn in values can be partially given by the `init`
#' parameter if it is specified.
#'
#' If `burnin` is set to `"auto"`, the `burnin` period is set to
#' `64 * context_number(object)`, following the heuristic proposed in Mächler and
#' Bühlmann (2004).
#'
#' @references  Mächler, M. and Bühlmann, P. (2004)
#' "Variable Length Markov Chains: Methodology, Computing, and Software"
#' Journal of Computational and Graphical Statistics, 13 (2), 435-455,
#' \doi{10.1198/1061860043524}
#'
#' @param object a fitted vlmc object.
#' @param nsim length of the simulated time series (defaults to 1).
#' @param seed an optional random seed.
#' @param init an optional initial sequence for the time series
#' @param burnin number of initial observations to discard or `"auto"` (see the dedicated section)
#' @param ... additional arguments.
#'
#' @returns a simulated discrete time series of the same type as the one used to
#'   build the vlmc.
#' @export
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
#' model <- vlmc(dts, min_size = 5)
#' new_dts <- simulate(model, 500, seed = 0)
#' new_dts_2 <- simulate(model, 500, seed = 0, init = dts[1:5])
simulate.vlmc <- function(object, nsim = 1L, seed = NULL, init = NULL, burnin = 0L, ...) {
  max_depth <- depth(object)
  if (!is.null(seed)) {
    withr::local_seed(seed)
  }
  if (burnin == "auto") {
    burnin <- 64L * context_number(object)
  } else {
    burnin <- as.integer(burnin)
    if (burnin < 0L) {
      stop("burnin must be non negative or \"auto\"")
    }
  }
  if (!is.null(init)) {
    assertthat::assert_that((typeof(init) == typeof(object$vals)) && (class(init) == class(object$vals)),
      msg = "init is not compatible with the model state space"
    )
    assertthat::assert_that(length(init) <= nsim + burnin, msg = "too many initial values")
    init_dts <- to_dts(init, object$vals)
    ctx <- rev(init_dts$ix)[1:(min(max_depth, length(init)))] + 1L
  } else {
    ctx <- c()
    istart <- 1L
  }
  int_vals <- seq_along(object$vals)
  pre_res <- rep(0L, nsim)
  nb_sim <- nsim + burnin
  out_pos <- 1L
  if (!is.null(init)) {
    if (length(init) > burnin) {
      pre_res[1:(length(init) - burnin)] <- init_dts$ix[(burnin + 1):length(init)] + 1L
      out_pos <- length(init) - burnin + 1L
    }
    istart <- 1L + length(init)
  }
  if (istart <= nb_sim) {
    if (max_depth == 0) {
      ## burn in for the random generator
      if (burnin > istart) {
        sample(int_vals, burnin - istart, replace = TRUE, prob = object$f_by)
      }
      pre_res[out_pos:nsim] <- sample(int_vals, nsim - out_pos + 1L, replace = TRUE, prob = object$f_by)
    } else {
      for (i in (istart):nb_sim) {
        subtree <- match_context(object, ctx)
        a_sim <- sample(int_vals, 1L, prob = subtree$tree$f_by)
        if (length(ctx) < max_depth) {
          ctx <- c(a_sim, ctx)
        } else {
          ctx <- c(a_sim, ctx[1:(max_depth - 1L)])
        }
        if (i > burnin) {
          pre_res[out_pos] <- a_sim
          out_pos <- out_pos + 1L
        }
      }
    }
  }
  object$vals[pre_res]
}
