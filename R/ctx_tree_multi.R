## Context tree for multiple series
grow_multi_ctx_tree <- function(xs, vals, min_size, max_depth, covsize = 0L,
                                keep_match = FALSE, all_children = FALSE,
                                compute_stats = FALSE, weights = NULL) {
  recurse_multi_ctx_tree <- function(xs, nb_vals, d, from, f_by) {
    if (d < max_depth) {
      fmatch <- forward_match_all_ctx_counts_multi(xs, nb_vals, d, from, weights)
      children <- vector(mode = "list", nb_vals)
      nb_children <- 0L
      d_max <- FALSE
      for (v in 1:nb_vals) {
        ## we look at the descendants hence the target depth is d + 1
        if (sum(fmatch$agg_counts[v, ]) >= min_size * (1L + covsize * (d + 1L))) {
          children[[v]] <- recurse_multi_ctx_tree(
            xs, nb_vals, d + 1L,
            fmatch$positions[[v]],
            fmatch$agg_counts[v, ]
          )
          nb_children <- nb_children + 1
          if (isTRUE(children[[v]]$max_depth)) {
            d_max <- TRUE
            children[[v]]$max_depth <- NULL
          }
        } else {
          children[[v]] <- list()
        }
      }
      result <- list()
      ## FIXME
      ## the rationale of this test is unclear...
      if (nb_children == nb_vals || (!all_children && nb_children > 0)) {
        result$children <- children
      }
      result$f_by <- f_by
      if (keep_match) {
        result$match <- from
      }
      if (d_max) {
        result$max_depth <- TRUE
      }
      result
    } else {
      result <- list(f_by = f_by, max_depth = TRUE)
      if (keep_match) {
        result$match <- from
      }
      result
    }
  }
  init_f_by <- weighted_table(xs, weights)
  pre_res <- recurse_multi_ctx_tree(xs, length(vals), 0L, NULL, init_f_by)
  if (is.null(pre_res$max_depth)) {
    pre_res$max_depth <- FALSE
  }
  new_ctx_tree(vals, pre_res, compute_stats = compute_stats)
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
#' @param max_depth integer >= 1 (default: 100). Maximum length of a context to
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
multi_ctx_tree <- function(xs, min_size = 2L, max_depth = 100L,
                           keep_position = FALSE, weights = NULL) {
  assertthat::assert_that(is.list(xs))
  assertthat::assert_that(length(xs) >= 1)
  assertthat::assert_that(min_size > 0)
  if (!is.null(weights)) {
    assertthat::assert_that(is.numeric(weights))
    assertthat::assert_that(assertthat::are_equal(length(xs), length(weights)))
    assertthat::assert_that(all(weights >= 0))
  }
  ixs <- to_multi_dts(xs)
  vals <- ixs$vals
  if (length(vals) > max(10, 0.05 * length(xs[[1]]))) {
    warning(paste0("xs[[1]] as numerous unique values (", length(vals), ")"))
  }
  pre_result <- grow_multi_ctx_tree(ixs$ixs, vals,
    min_size = min_size, max_depth = max_depth,
    keep_match = keep_position,
    compute_stats = FALSE, weights = weights
  )
  result <- new_ctx_tree(vals, pre_result, compute_stats = TRUE, class = "multi_ctx_tree")
  result$keep_match <- keep_position
  result$data_size <- sum(lengths(xs))
  if (depth(result) > 0) {
    d <- depth(result)
    result$ix <- lapply(ixs$ixs, \(x) x[1:min(d, length(x))])
  }
  result
}
