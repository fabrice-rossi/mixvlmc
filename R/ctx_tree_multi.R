## Context tree for multiple series

## insert a new dts into an existing context tree
insert_dts <- function(tree, x, vals, max_depth, weight) {
  recurse_insert_dts <- function(tree, x, nb_vals, d, from, f_by) {
    if (d < max_depth) {
      fmatch <- forward_match_all_ctx_counts(x, nb_vals, d, from)
      nb_children <- 0L
      if (is.null(tree) || is.null(tree[["children"]])) {
        ## we are in a leaf of the current context tree
        children <- vector(mode = "list", nb_vals)
      } else {
        children <- tree$children
      }
      d_max <- FALSE
      for (v in 1:nb_vals) {
        if (sum(fmatch$counts[v, ]) > 0) {
          children[[v]] <- recurse_insert_dts(
            tree$children[[v]], x, nb_vals, d + 1L,
            fmatch$positions[[v]], fmatch$counts[v, ]
          )
          nb_children <- nb_children + 1
        } else {
          ## nothing to do we keep the current children[[v]]
          if (!is.null(children[[v]])) {
            nb_children <- nb_children + 1
          } else {
            ## make sure to avoid null content
            children[[v]] <- list()
          }
        }
        if (isTRUE(children[[v]]$max_depth)) {
          d_max <- TRUE
          children[[v]]$max_depth <- NULL
        }
      }
      if (is.null(weight)) {
        result <- list(
          children = children,
          f_by = f_by
        )
      } else {
        result <- list(
          children = children,
          f_by = f_by * weight
        )
      }
      if (d_max) {
        result$max_depth <- TRUE
      }
    } else {
      if (is.null(weight)) {
        result <- list(f_by = f_by, max_depth = TRUE)
      } else {
        result <- list(f_by = f_by, max_depth = TRUE)
      }
    }
    if (!is.null(tree[["f_by"]])) {
      if (is.null(weight)) {
        result$f_by <- f_by + tree[["f_by"]]
      } else {
        result$f_by <- f_by * weight + tree[["f_by"]]
      }
    }
    result
  }
  recurse_insert_dts(tree, x, length(vals), 0L, NULL, table(x))
}

## min_size based pruning
## depth cannot be larger than max_depth if this is used consistently
prune_multi_ctx_tree <- function(tree, min_size, max_depth) {
  rec_prune_mctx <- function(tree, d) {
    ## do we keep this node?
    if (is.null(tree[["f_by"]])) {
      ## empty node
      list()
    } else if (sum(tree[["f_by"]]) < min_size) {
      ## size based pruning
      list()
    } else {
      ## we keep this node for sure, do recursive processing if needed
      d_max <- FALSE
      if (!is.null(tree[["children"]])) {
        subtrees <- vector(mode = "list", length(tree$children))
        nb_children <- 0L
        for (v in seq_along(tree$children)) {
          subtrees[[v]] <- rec_prune_mctx(tree$children[[v]], d + 1L)
          if (length(subtrees[[v]]) > 0) {
            ## this sub tree was kept
            nb_children <- 1L + nb_children
            if (isTRUE(subtrees[[v]]$max_depth)) {
              ## propagate max_depth
              d_max <- TRUE
              subtrees[[v]]$max_depth <- NULL
            }
          }
        }
        if (nb_children > 0) {
          tree$children <- subtrees
        } else {
          ## remove all children
          tree$children <- NULL
        }
      }
      if (d_max || d == max_depth) {
        ## propagate max_depth
        tree$max_depth <- TRUE
      } else {
        tree$max_depth <- NULL
      }
      tree
    }
  }
  rec_prune_mctx(tree, 0L)
}

#' Build a context tree for a collection of discrete time series
#'
#' This function builds a context tree for a collection of time series.
#'
#' The tree represents all the sequences of symbols/states of length smaller
#' than `max_depth` that appear at least `min_size` times in collection of the
#' time series and stores the frequencies of the states that follow each
#' context.
#'
#' Owing to the iterative nature of construction, this function may use a large
#' quantity of memory as pruning infrequent contexts is only done after
#' computing all of them. It is therefore recommend to avoid large depths and
#' the default value of `max_depth` is smaller than in the single time series
#' function [ctx_tree()].
#'
#' @section Weights:
#'
#' If given, the `weights` parameter must be a vector of non negative values of the
#' same length as `xs`. Each time series is then weighted using the corresponding
#' weight. Weights are interpreted as fractional number of occurrences when
#' `min_size` is checked. A context is kept in the context tree if the sum of
#' the weights of the series in which it appears is larger than the `min_size`
#' threshold.
#'
#' @param xs a list of discrete times series
#' @param min_size positive numerical value (default: 2). Minimum number of observations for
#'   a context to be included in the tree (counted over the full collection of
#'   time series, see details for the case with `weights`)
#' @param max_depth integer >= 1 (default: 25). Maximum length of a context to
#'   be included in the tree.
#' @param keep_position  logical (default: FALSE). Should the context tree keep
#'   the position of the contexts.
#' @param weights optional weights for the time series, see details.
#'
#' @return a context tree (of class that inherits from `multi_ctx_tree`).
#' @export
#' @seealso [ctx_tree()]
#' @examples
#' dts <- c(0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0)
#' dts2 <- c(0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0)
#' mdts <- list(dts, dts2)
#' mctx <- multi_ctx_tree(mdts, max_depth = 4)
multi_ctx_tree <- function(xs, min_size = 2L, max_depth = 25L,
                           keep_position = FALSE, weights = NULL) {
  ## keep_position = TRUE is not supported currently
  assertthat::assert_that(!keep_position)
  assertthat::assert_that(is.list(xs))
  assertthat::assert_that(length(xs) >= 1)
  assertthat::assert_that(min_size > 0)
  if (!is.null(weights)) {
    assertthat::assert_that(is.numeric(weights))
    assertthat::assert_that(assertthat::are_equal(length(xs), length(weights)))
    assertthat::assert_that(all(weights >= 0))
  }
  nx_1 <- to_dts(xs[[1]])
  ix_1 <- nx_1$ix
  vals <- nx_1$vals
  if (length(vals) > max(10, 0.05 * length(xs[[1]]))) {
    warning(paste0("x[[1]] as numerous unique values (", length(vals), ")"))
  }
  ## we cannot use the original min_size for individual time series
  if (is.null(weights)) {
    weight <- NULL
  } else {
    weight <- weights[1]
  }
  pre_result <- grow_ctx_tree(ix_1, vals,
    min_size = 1L, max_depth = max_depth, keep_match = keep_position,
    compute_stats = FALSE, weight = weight
  )
  d_max <- pre_result$max_depth
  if (length(xs) > 1) {
    for (k in 2:length(xs)) {
      nx <- to_dts(xs[[k]], vals = vals)
      if (is.null(weights)) {
        weight <- NULL
      } else {
        weight <- weights[k]
      }
      pre_result <- insert_dts(pre_result, nx$ix, vals, max_depth = max_depth, weight = weight)
      d_max <- d_max | pre_result$max_depth
    }
  }
  ## let us post process the tree to remove rare contexts
  if (min_size > 1L) {
    pre_result <- prune_multi_ctx_tree(pre_result, min_size, max_depth)
  } else {
    pre_result$max_depth <- d_max
  }
  if (is.null(pre_result$max_depth)) {
    pre_result$max_depth <- FALSE
  }
  new_ctx_tree(vals, pre_result, compute_stats = TRUE, class = "multi_ctx_tree")
}
