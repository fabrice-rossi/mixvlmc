#' Match a context tree to a new discrete time series
#'
#' This is an internal function. It adds a new member, data_f_by, to each node
#' of a context tree. This vector contains the counts of occurrences of symbols
#' after each of the contexts of the tree in the time series x. In addition
#' it removes the subtrees whose contexts are not matched in the time series.
#'
#' @param tree a context tree.
#' @param x a discrete time series in its low level representation.
#' @param keep_match whether to keep the match position.
#' @param start number of initial values of x which are not counted as
#'   occurrences of symbols after a context (they are still used as contexts for
#'   the following values). Not compatible with `keep_match = TRUE`.
#'
#' @return a context tree supplemented with the matching information.
#'
#' @noRd
match_ctx <- function(tree, x, keep_match = FALSE, start = 0L) {
  recurse_match_ctx <- function(tree, x, nb_vals, d, from, f_by) {
    result <- tree
    if (is.null(tree$children)) {
      result$data_f_by <- f_by
      if (keep_match) {
        result$match <- from
      }
      result
    } else {
      if (d == 0 && start == 0) {
        fmatch <- forward_match_all_ctx_counts(x, nb_vals, d, NULL)
      } else {
        fmatch <- forward_match_all_ctx_counts(x, nb_vals, d, from)
      }
      children <- vector(mode = "list", nb_vals)
      nb_children <- 0
      for (v in 1:nb_vals) {
        if (length(fmatch$positions[[v]]) > 0 && length(tree$children[[v]]) > 0) {
          if (any(fmatch$counts[v, ] > 0)) {
            children[[v]] <- recurse_match_ctx(tree$children[[v]], x, nb_vals, d + 1, fmatch$positions[[v]], fmatch$counts[v, ])
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
  if (start > 0) {
    assertthat::assert_that(!keep_match, start < length(x))
    ## positions (0-based) of the values counted as occurrences after the
    ## (empty) context
    init_from <- start:(length(x) - 1)
    x_counted <- x[(start + 1):length(x)]
  } else {
    if (!is.null(tree$match)) {
      init_from <- tree$match
    }
    x_counted <- x
  }
  recurse_match_ctx(tree, x, length(tree$vals), 0, init_from, tabulate(x_counted + 1, nbins = length(tree$vals)))
}
