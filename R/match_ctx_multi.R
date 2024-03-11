#' Match a context tree to a new collection of discrete time series
#'
#' This is an internal function. It adds to a context tree a new member to each
#' node, data_f_by. The vector contains the counts of occurrences of symbols
#' after each of the context of the tree in the collection of time series x. In
#' addition it truncates the sub tree whose contexts are not matched in the time
#' series.
#'
#' @param tree a context tree.
#' @param xs a collection of discrete time series in their low level representation.
#' @param keep_match whether to keep the match position.
#' @param weights time series weights
#'
#' @return a context tree supplemented with the matching information.
#'
#' @noRd
match_multi_ctx <- function(tree, xs, keep_match = FALSE, weights = NULL) {
  recurse_match_multi_ctx <- function(tree, xs, nb_vals, d, from, f_by) {
    result <- tree
    if (is.null(tree$children)) {
      result$data_f_by <- f_by
      if (keep_match) {
        result$match <- from
      }
      result
    } else {
      if (d == 0) {
        fmatch <- forward_match_all_ctx_counts_multi(xs, nb_vals, d, NULL, weights)
      } else {
        fmatch <- forward_match_all_ctx_counts_multi(xs, nb_vals, d, from, weights)
      }
      children <- vector(mode = "list", nb_vals)
      nb_children <- 0
      for (v in 1:nb_vals) {
        if (sum(lengths(fmatch$positions[[v]])) > 0 && length(tree$children[[v]]) > 0) {
          if (any(fmatch$agg_counts[v, ] > 0)) {
            children[[v]] <- recurse_match_multi_ctx(
              tree$children[[v]], xs,
              nb_vals, d + 1,
              fmatch$positions[[v]],
              fmatch$agg_counts[v, ]
            )
            nb_children <- nb_children + 1
          } else {
            children[[v]] <- list()
            if (keep_match) {
              children[[v]]$match <- fmatch$positions[[v]]
              nb_children <- nb_children + 1
            }
          }
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
  init_from <- NULL
  if (!is.null(tree$match)) {
    init_from <- tree$match
  }
  recurse_match_multi_ctx(
    tree, xs, length(tree$vals), 0, init_from,
    weighted_tabulate(xs, nbins = length(tree$vals))
  )
}
