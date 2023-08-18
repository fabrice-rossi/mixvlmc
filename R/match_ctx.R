#' Match a context tree to a new discrete time series
#'
#' This is an internal function.
#'
#' @param tree a context tree.
#' @param x a discrete time series in its low level representation.
#' @param keep_match whether to keep the match position.
#'
#' @return a context tree supplemented with the matching information.
#'
#' @noRd
match_ctx <- function(tree, x, keep_match = FALSE) {
  recurse_match_ctx <- function(tree, x, nb_vals, d, from, f_by) {
    result <- tree
    if (is.null(tree$children)) {
      result$data_f_by <- f_by
      if (keep_match) {
        result$match <- from
      }
      result
    } else {
      fmatch <- forward_match_all_ctx_counts(x, nb_vals, d, from)
      children <- vector(mode = "list", nb_vals)
      nb_children <- 0
      for (v in 1:nb_vals) {
        if (length(fmatch$positions[[v]]) > 0 && length(tree$children[[v]]) > 0) {
          children[[v]] <- recurse_match_ctx(tree$children[[v]], x, nb_vals, d + 1, fmatch$positions[[v]], fmatch$counts[v, ])
          nb_children <- nb_children + 1
        } else {
          children[[v]] <- list()
        }
      }
      if (nb_children > 0) {
        result$children <- children
      } else {
        result$children <- NULL
      }
      result$f_by <- tree$f_by
      result$data_f_by <- f_by
      if (keep_match) {
        result$match <- from
      }
      result
    }
  }
  recurse_match_ctx(tree, x, length(tree$vals), 0, NULL, tabulate(x + 1, nbins = length(tree$vals)))
}
