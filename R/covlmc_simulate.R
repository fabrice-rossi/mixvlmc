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


#' Simulate a discrete time series
#'
#' This function simulates a time series from the distribution estimated by the
#' given covlmc object.
#'
#' A VLMC with covariates model needs covariates to compute its transition
#' probabilities. The covariates must be submitted as a data frame using the
#' named `covariate` argument.
#'
#' @param object a fitted covlmc object.
#' @param nsim length of the simulated time series (defaults to 1).
#' @param seed an optional random seed.
#' @param ... additional arguments (see details).
#'
#' @export
simulate.covlmc <- function(object, nsim = 1, seed = NULL, ...) {
  params <- list(...)
  assertthat::assert_that(assertthat::has_name(params, "covariate"))
  covariate <- params$covariate
  assertthat::assert_that(nrow(covariate) >= nsim)
  assertthat::assert_that(assertthat::has_name(covariate, object$cov_names))
  if (!is.null(seed)) {
    withr::local_seed(seed)
  }
  int_vals <- seq_along(object$vals)
  ctx <- c()
  pre_res <- rep(0, nsim)
  max_depth <- depth(object)
  if (max_depth == 0) {
    ## degenerate case
    mm <- covariate[1, , drop = FALSE]
    for (i in 1:nsim) {
      pre_res[i] <- 1 + glm_sample_one(object$model$model, mm)
    }
  } else {
    for (i in 1:nsim) {
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
  object$vals[pre_res]
}
