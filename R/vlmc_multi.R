## VLMC for multiple series

#' Fit a Variable Length Markov Chain (VLMC) to a collection of time series
#'
#' This function fits a  Variable Length Markov Chain (VLMC) to a collection of
#' discrete time series.
#'
#' @param xs list of discrete times series
#' @param alpha number in (0,1] (default: 0.05) cut off value in quantile scale
#'   in the pruning phase.
#' @param cutoff non negative number: cut off value in native (likelihood ratio)
#'   scale in the pruning phase. Defaults to the value obtained from `alpha`.
#'   Takes precedence over `alpha` is specified.
#' @param min_size integer >= 1 (default: 2). Minimum number of observations for
#'   a context in the growing phase of the context tree.
#' @param max_depth integer >= 1 (default: 100). Longest context considered in
#'   growing phase of the context tree.
#' @param prune logical: specify whether the context tree should be pruned
#'   (default behaviour).
#' @param keep_match logical: specify whether to keep the context matches
#'   (default to FALSE)
#' @param weights optional weights for the time series, see details.
#'
#' @section Weights:
#'
#' If given, the `weights` parameter must be a vector of non negative values of the
#' same length as `xs`. Each time series is then weighted using the corresponding
#' weight. Weights are interpreted as fractional number of occurrences when
#' `min_size` is checked. A context is kept in the context tree if the sum of
#' the weights of the series in which it appears is larger than the `min_size`
#' threshold. Conditional probabilities associated to contexts are computed
#' from the weighted occurrences.
#'
#' @returns a fitted vlmc model (of class `multi_vlmc`)
#' @examples
#' pc <- powerconsumption[powerconsumption$week %in% 5:8, ]
#' powerlevels <- quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))
#' dts <- tapply(pc$active_power, pc$week, \(x) cut(x, breaks = c(0, powerlevels)))
#' model <- multi_vlmc(dts, max_depth = 3)
#' draw(model)
#' depth(model)
#' @export
#' @seealso [multi_ctx_tree()], [vlmc()]
multi_vlmc <- function(xs, alpha = 0.05, cutoff = NULL, min_size = 2L, max_depth = 100L,
                       prune = TRUE, keep_match = FALSE, weights = NULL) {
  ## keep_match=TRUE is currently not supported
  assertthat::assert_that(!keep_match)
  assertthat::assert_that(is.list(xs))
  ctx_tree <- multi_ctx_tree(xs,
    min_size = min_size, max_depth = max_depth,
    keep_position = keep_match,
    weights = weights
  )
  if (is.null(cutoff)) {
    if (is.null(alpha) || !is.numeric(alpha) || alpha <= 0 || alpha > 1) {
      stop("the alpha parameter must be in (0, 1]")
    }
    cutoff <- to_native(alpha, length(ctx_tree$vals))
  } else {
    ## cutoff takes precedence
    if (!is.numeric(cutoff) || cutoff < 0) {
      stop("the cutoff parameter must be a non negative number")
    }
    alpha <- to_quantile(cutoff, length(ctx_tree$vals))
  }
  if (prune) {
    result <- prune_ctx_tree(ctx_tree, alpha = alpha, cutoff = cutoff)
    class(result) <- c("multi_vlmc", class(result))
  } else {
    result <- new_ctx_tree(ctx_tree$vals, ctx_tree, class = c("multi_vlmc", "vlmc"))
  }
  result$alpha <- alpha
  result$cutoff <- cutoff
  result$keep_match <- keep_match
  result$data_size <- sum(lengths(xs, use.names = FALSE))
  result$pruned <- prune
  result
}

#' @rdname prune
#' @export
prune.multi_vlmc <- function(vlmc, alpha = 0.05, cutoff = NULL, ...) {
  if (is.null(cutoff)) {
    if (is.null(alpha) || !is.numeric(alpha) || alpha <= 0 || alpha > 1) {
      stop("the alpha parameter must be in (0, 1]")
    }
  }
  result <- prune_ctx_tree(vlmc,
    alpha = alpha, cutoff = cutoff,
    class = c("multi_vlmc", "vlmc")
  )
  if (is.null(cutoff)) {
    cutoff <- to_native(alpha, length(vlmc$vals))
  } else {
    ## cutoff takes precedence
    alpha <- to_quantile(cutoff, length(vlmc$vals))
  }
  result$alpha <- alpha
  result$cutoff <- cutoff
  result$data_size <- vlmc$data_size
  result$keep_match <- vlmc$keep_match
  ## preserve the construction information
  result$max_depth <- vlmc$max_depth
  result$pruned <- TRUE
  result
}
