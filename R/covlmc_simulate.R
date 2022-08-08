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
#' @param seed an optional random seed.
#' @param covariate values of the covariates
#' @param init an optional initial sequence for the time series
#' @param ... additional arguments.
#'
#' @export
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
#' dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
#' m_cov <- covlmc(dts, dts_cov, min_size = 5)
#' # new week with day light from 6:00 to 18:00
#' new_cov <- data.frame(day_night = rep(c(rep(FALSE, 59), rep(TRUE, 121), rep(FALSE, 60)), times = 7))
#' new_dts <- simulate(m_cov, nrow(new_cov), seed = 0, covariate = new_cov)
#' new_dts_2 <- simulate(m_cov, nrow(new_cov), seed = 0, covariate = new_cov, init = dts[1:10])
simulate.covlmc <- function(object, nsim = 1, seed = NULL, covariate, init = NULL, ...) {
  assertthat::assert_that(nrow(covariate) >= nsim)
  assertthat::assert_that(assertthat::has_name(covariate, object$cov_names))
  max_depth <- depth(object)
  if (!is.null(seed)) {
    withr::local_seed(seed)
  }
  int_vals <- seq_along(object$vals)
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
    if (max_depth == 0) {
      ## degenerate case
      mm <- covariate[1, , drop = FALSE]
      for (i in istart:nsim) {
        pre_res[i] <- 1 + glm_sample_one(object$model$model, mm)
      }
    } else {
      for (i in istart:nsim) {
        subtree <- match_context_co(object, ctx)
        if (subtree$merged) {
          mm <- prepare_covariate(covariate, i - subtree$depth, d = subtree$tree$merged_model$hsize, from = 0)
          pre_res[i] <- 1 + glm_sample_one(subtree$tree$merged_model$model, mm)
        } else if (is.null(subtree$tree[["model"]])) {
          pre_res[i] <- sample(int_vals, 1, prob = subtree$tree$f_by)
        } else {
          mm <- prepare_covariate(covariate, i - subtree$depth, d = subtree$tree$model$hsize, from = 0)
          pre_res[i] <- 1 + glm_sample_one(subtree$tree$model$model, mm)
        }
        if (length(ctx) < max_depth) {
          ctx <- c(pre_res[i], ctx)
        } else {
          ctx <- c(pre_res[i], ctx[1:(max_depth - 1)])
        }
      }
    }
  }
  object$vals[pre_res]
}
