#' Convert an object to a Variable Length Markov Chain (VLMC)
#'
#' This generic function converts an object into a VLMC.
#'
#' @param x an object to convert into a VLMC.
#' @param ... additional arguments for conversion functions.
#'
#' @returns a VLMC.
#' @export
as_vlmc <- function(x, ...) {
  UseMethod("as_vlmc")
}

#' @param alpha cut off parameter applied during the conversion, quantile scale
#'  (if specified).
#' @param cutoff cut off parameter applied during the conversion, native scale
#'  (if specified). Takes precedence over `alpha` if both are specified.
#' @details When `x` is a context tree, the function converts it into a VLMC.
#'  If `alpha` or `cutoff` is specified, it is used to reduce the complexity of
#'  the tree as in a direct call to [vlmc()] (see also [prune()]). Otherwise,
#'  the tree is converted without pruning (the cut off of the resulting VLMC
#'  is 0 in native scale, i.e. 1 in quantile scale).
#'
#'  When `x` is the result of [tune_vlmc()], the function returns the best
#'  model selected by [tune_vlmc()].
#' @export
#' @rdname as_vlmc
#' @seealso [ctx_tree()]
#' @examples
#' ## conversion from a context tree
#' rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
#' rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 3)
#' draw(rdts_ctree)
#' rdts_vlmc <- as_vlmc(rdts_ctree)
#' class(rdts_vlmc)
#' draw(rdts_vlmc)
#' ## conversion with pruning
#' rdts_pruned_vlmc <- as_vlmc(rdts_ctree, alpha = 0.25)
#' draw(rdts_pruned_vlmc)
as_vlmc.ctx_tree <- function(x, alpha, cutoff, ...) {
  rlang::check_dots_empty()
  if (missing(alpha) && missing(cutoff)) {
    result <- new_ctx_tree(x$vals, x, class = "vlmc")
    result$alpha <- 1
    result$cutoff <- 0
  } else {
    if (missing(cutoff)) {
      ## cutoff takes precedence
      if (!is.numeric(alpha) || alpha <= 0 || alpha > 1) {
        stop("the alpha parameter must be in (0, 1]")
      }
      cutoff <- to_native(alpha, length(x$vals))
    } else {
      if (!is.numeric(cutoff) || cutoff < 0) {
        stop("the cutoff parameter must be a non negative number")
      }
      alpha <- to_quantile(cutoff, length(x$vals))
    }
    result <- prune_ctx_tree(x, cutoff = cutoff)
    result$alpha <- alpha
    result$cutoff <- cutoff
    if (x$keep_match) {
      ## need to be recomputed in case of pruning
      ## handle the case where the root is context
      if (!is_full_node(result)) {
        result$match <- 0:(x$data_size - 1)
      }
    }
  }
  if (depth(result) > 0) {
    result$ix <- x$ix[1:min(depth(result), length(x$ix))]
    ivlmc <- match_ctx(result, result$ix)
    result$extended_ll <- rec_loglikelihood_vlmc(ivlmc, TRUE)
  } else {
    result$extended_ll <- 0
  }
  result
}

#' @export
#' @rdname as_vlmc
#' @seealso [tune_vlmc()]
#' @examples
#' ## conversion from the result of tune_vlmc()
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' rdts <- cut(pc$active_power,
#'   breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1)))
#' )
#' tune_result <- tune_vlmc(rdts)
#' tune_result
#' rdts_best_vlmc <- as_vlmc(tune_result)
#' draw(rdts_best_vlmc)
as_vlmc.tune_vlmc <- function(x, ...) {
  rlang::check_dots_empty()
  x$best_model
}
