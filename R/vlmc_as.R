#' Convert an object to a Variable Length Markov Chain (VLMC)
#'
#' This generic function converts an object into a vlmc.
#'
#' @param x an object to convert into a vlmc.
#' @param ... additional arguments for conversion functions.
#'
#' @returns a vlmc
#' @export
as_vlmc <- function(x, ...) {
  UseMethod("as_vlmc")
}

#' @param alpha cut off parameter applied during the conversion, quantile scale
#'  (if specified)
#' @param cutoff cut off parameter applied during the conversion, native scale
#'  (if specified)
#' @details This function converts a context tree into a VLMC. If `alpha` or
#'  `cutoff` is specified, it is used to reduce the complexity of the tree as in
#'  a direct call to [vlmc()] ([prune()]).
#' @export
#' @rdname as_vlmc
#' @seealso [ctx_tree()]
#' @examples
#' ## conversion from a context tree
#' dts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
#' dts_ctree <- ctx_tree(dts, min_size = 1, max_depth = 3)
#' draw(dts_ctree)
#' dts_vlmc <- as_vlmc(dts_ctree)
#' class(dts_vlmc)
#' draw(dts_vlmc)
as_vlmc.ctx_tree <- function(x, alpha, cutoff, ...) {
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
  }
  if (depth(result) > 0) {
    result$ix <- x$ix[1:min(depth(result), length(x$ix))]
    ivlmc <- match_ctx(result, result$ix)
    result$extended_ll <- rec_loglikelihood_vlmc(ivlmc, TRUE)
  }
  result
}

#' @export
#' @rdname as_vlmc
#' @seealso [tune_vlmc()]
#' @examples
#' ## conversion from the result of tune_vlmc
#' dts <- sample(as.factor(c("A", "B", "C")), 100, replace = TRUE)
#' tune_result <- tune_vlmc(dts)
#' tune_result
#' dts_best_vlmc <- as_vlmc(tune_result)
#' draw(dts_best_vlmc)
as_vlmc.tune_vlmc <- function(x, ...) {
  x$best_model
}
