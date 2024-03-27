rec_match_context_co <- function(tree, d, ctx) {
  if (length(ctx) == 0) {
    list(tree = tree, depth = d, merged = FALSE, model = !is.null(tree[["model"]]))
  } else {
    if (is.null(tree$children)) {
      list(tree = tree, depth = d, merged = FALSE, model = !is.null(tree[["model"]]))
    } else {
      cand <- tree$children[[ctx[1]]]
      if (length(cand) > 0) {
        pre_res <- rec_match_context_co(cand, d + 1, ctx[-1])
        if (!pre_res$model) {
          if (!is.null(tree[["merged"]])) {
            if (ctx[1] %in% tree[["merged"]]) {
              pre_res <- list(tree = tree, depth = d, merged = TRUE, model = TRUE)
            }
          }
        }
        pre_res
      } else {
        list(tree = tree, depth = d, merged = FALSE, model = !is.null(tree[["model"]]))
      }
    }
  }
}

match_context_co <- function(tree, ctx) {
  rec_match_context_co(tree, 0, ctx)
}


#' Simulate a discrete time series for a covlmc
#'
#' This function simulates a time series from the distribution estimated by the
#' given covlmc object.
#'
#' A VLMC with covariates model needs covariates to compute its transition
#' probabilities. The covariates must be submitted as a data frame using the
#' `covariate` argument. In addition,  the time series can be initiated by a
#' fixed sequence specified via the `init` parameter.
#'
#' @param object a fitted covlmc object.
#' @param nsim length of the simulated time series (defaults to 1).
#' @param seed an optional random seed (see the dedicated section).
#' @param covariate values of the covariates.
#' @param init an optional initial sequence for the time series.
#' @param ... additional arguments.
#'
#' @section Extended contexts:
#'
#'   As explained in details in [loglikelihood.covlmc()] documentation and in
#'   the dedicated `vignette("likelihood", package = "mixvlmc")`, the first
#'   initial values of a time series do not in general have a proper context for
#'   a COVLMC with a non zero order. In order to simulate something meaningful
#'   for those values, we rely on the notion of extended context defined in the
#'   documents mentioned above. This follows the same logic as using
#'   [loglikelihood.covlmc()] with the parameter `initial="extended"`. All
#'   covlmc functions that need to manipulate initial values with no proper
#'   context use the same approach.
#' @inheritSection simulate.vlmc Random seed
#' @returns a simulated discrete time series of the same type as the one used to
#'   build the covlmc with a `seed` attribute (see the Random seed section). The
#'   results has also the `dts_simulated` class to hide the `seed` attribute when using
#'   `print` or similar function.
#' @seealso [stats::simulate()] for details and examples on the random number generator setting
#' @export
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
#' rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
#' m_cov <- covlmc(rdts, rdts_cov, min_size = 5)
#' # new week with day light from 6:00 to 18:00
#' new_cov <- data.frame(day_night = rep(c(rep(FALSE, 59), rep(TRUE, 121), rep(FALSE, 60)), times = 7))
#' new_rdts <- simulate(m_cov, nrow(new_cov), seed = 0, covariate = new_cov)
#' new_rdts_2 <- simulate(m_cov, nrow(new_cov), seed = 0, covariate = new_cov, init = rdts[1:10])
simulate.covlmc <- function(object, nsim = 1, seed = NULL, covariate, init = NULL, ...) {
  if (isTRUE(object$trimmed == "full")) {
    stop("simulate is not supported by fully trimmed covlmc")
  }
  assertthat::assert_that(nrow(covariate) >= nsim)
  covariate <- validate_covariate(object, covariate)
  max_depth <- depth(object)
  if (!is.null(seed)) {
    attr(seed, "kind") <- as.list(RNGkind())
    withr::local_seed(seed)
  } else {
    if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      stats::runif(1)
    }
    seed <- .Random.seed
  }
  int_vals <- seq_along(object$vals)
  if (!is.null(init)) {
    assertthat::assert_that((typeof(init) == typeof(object$vals)) && methods::is(init, class(object$vals)),
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
    if (max_depth == 0) {
      ## degenerate case
      mm <- covariate[1, , drop = FALSE]
      for (i in istart:nsim) {
        pre_res[i] <- 1 + glm_sample_one(object$model$model, mm, object$vals)
      }
    } else {
      for (i in istart:nsim) {
        subtree <- match_context_co(object, ctx)
        if (subtree$merged) {
          local_model <- subtree$tree$merged_model
        } else if (is.null(subtree$tree[["model"]])) {
          local_model <- subtree$tree$extended_model
        } else {
          local_model <- subtree$tree$model
        }
        mm <- prepare_covariate(covariate, i - subtree$depth - 1,
          d = local_model$hsize,
          from = subtree$depth - local_model$hsize
        )
        pre_res[i] <- 1 + glm_sample_one(local_model$model, mm, object$vals)
        if (length(ctx) < max_depth) {
          ctx <- c(pre_res[i], ctx)
        } else {
          ctx <- c(pre_res[i], ctx[1:(max_depth - 1)])
        }
      }
    }
  }
  pre_res <- object$vals[pre_res]
  structure(pre_res, "seed" = seed, "class" = c("dts_simulated", class(pre_res)))
}
