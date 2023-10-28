#' @inherit as_vlmc.ctx_tree
#' @examples
#' ## conversion from a context tree
#' dts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
#' dts_ctree <- ctx_tree(dts, min_size = 1, max_depth = 3, backend = "C++")
#' draw(dts_ctree)
#' dts_vlmc <- as_vlmc(dts_ctree)
#' class(dts_vlmc)
#' draw(dts_vlmc)
#'
#' @export
as_vlmc.ctx_tree_cpp <- function(x, alpha, cutoff, ...) {
  if (missing(alpha) && missing(cutoff)) {
    result <- new_ctx_tree_cpp(x$vals, x$root, class = c("vlmc_cpp", "vlmc"))
    result$root$make_explicit()
    result$root$compute_reverse()
    result$max_depth <- x$max_depth
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
    x$root$make_explicit()
    x$root$compute_reverse()
    pre_result <- x$root$clone_prune_context(1L, -1L, cutoff)
    result <- new_ctx_tree_cpp(x$vals, pre_result, class = c("vlmc_cpp", "vlmc"))
    result$max_depth <- x$max_depth
    result$alpha <- alpha
    result$cutoff <- cutoff
  }
  if (depth(result) > 0) {
    result$ix <- x$ix[1:min(depth(result), length(x$ix))]
    result$extended_ll <- result$root$loglikelihood(result$ix, 0, TRUE, FALSE)
  } else {
    result$extended_ll <- 0
  }
  result$keep_match <- x$keep_match
  result$data_size <- x$data_size
  result
}
