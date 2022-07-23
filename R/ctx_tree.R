##  context tree representation
nb_sub_tree <- function(ct) {
  if (is.null(ct) || is.null(ct$children) || length(ct$children) == 0) {
    0
  } else {
    sum(sapply(ct$children, length) > 0)
  }
}

count_local_context <- function(node) {
  if (is.null(node$children)) {
    if (is.null(node[["f_by"]])) {
      0
    } else {
      1
    }
  } else {
    if (nb_sub_tree(node) < length(node$children)) {
      1
    } else {
      0
    }
  }
}

rec_stats_ctx_tree <- function(ct, count_context = count_local_context) {
  if (is.null(ct$children)) {
    ## this is a leaf
    ## depth = 0
    c(0, count_context(ct))
  } else {
    subresults <- sapply(ct$children, rec_stats_ctx_tree, count_context)
    dp <- 1 + max(subresults[1, ])
    nb <- sum(subresults[2, ]) + count_context(ct)
    c(dp, nb)
  }
}

new_ctx_tree <- function(vals, root = NULL, compute_stats = TRUE,
                         count_context = count_local_context, ..., class = character()) {
  if (is.null(root)) {
    root <- list(vals = vals)
  } else {
    assertthat::assert_that(is.list(root))
    root$vals <- vals
  }
  preres <- structure(root, ..., class = c(class, "ctx_tree"))
  if (!is.null(root)) {
    if (compute_stats) {
      stats <- rec_stats_ctx_tree(root, count_context)
      preres$depth <- stats[1]
      preres$nb_ctx <- stats[2]
    }
  }
  preres
}

grow_ctx_tree <- function(x, vals, min_size, max_depth, covsize = 0, keep_match = FALSE, all_children = FALSE,
                          compute_stats = FALSE) {
  recurse_ctx_tree <- function(x, nb_vals, d, from, f_by) {
    if (d < max_depth) {
      fmatch <- forward_match_all_ctx_counts(x, nb_vals, d, from)
      children <- vector(mode = "list", nb_vals)
      nb_children <- 0
      for (v in 1:nb_vals) {
        if (length(fmatch$positions[[v]]) >= min_size * (1 + covsize * (d + 1))) {
          children[[v]] <- recurse_ctx_tree(x, nb_vals, d + 1, fmatch$positions[[v]], fmatch$counts[v, ])
          nb_children <- nb_children + 1
        } else {
          children[[v]] <- list()
        }
      }
      result <- list()
      if (nb_children == nb_vals | (!all_children & nb_children > 0)) {
        result$children <- children
      }
      result$f_by <- f_by
      if (keep_match) {
        result$match <- from
      }
      result
    } else {
      result <- list(f_by = f_by)
      if (keep_match) {
        result$match <- from
      }
      result
    }
  }
  pre_res <- recurse_ctx_tree(x, length(vals), 0, NULL, table(x))
  new_ctx_tree(vals, pre_res, compute_stats = compute_stats, class = "vlmc")
}

#' Build a context tree for a discrete time series
#'
#' This function builds a context tree for a time series. The tree represents
#' all the sequences of symbols/states of length smaller than \code{max_depth}
#' that appear at least \code{min_size} times in the time series and stores the
#' frequencies of the states that follow each context. Optionally, the positions
#' of the contexts in the time series can be stored in the tree.
#'
#' @param x a discrete time series; can be numeric, character or factor.
#' @param min_size integer >= 1 (default: 2). Minimum number of observations for
#'   a context to be included in the tree.
#' @param max_depth integer >= 1 (default: 100). Maximum length of a context to
#'   be included in the tree.
#' @param keep_position logical (default: TRUE). Should the context tree keep
#'   the position of the contexts.
#'
#' @return a context tree (of class \code{ctx_tree})
#' @export
#'
#' @examples
#' dts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
#' ## get all contexts of length 2
#' dts_ctree <- ctx_tree(dts, min_size = 1, max_depth = 2)
#' draw(dts_ctree)
ctx_tree <- function(x, min_size = 2, max_depth = 10, keep_position = FALSE) {
  nx <- to_dts(x)
  ix <- nx$ix
  vals <- nx$vals
  if (length(vals) > max(10, 0.05 * length(x))) {
    warning(paste0("x as numerous unique values (", length(vals), ")"))
  }
  grow_ctx_tree(ix, vals, min_size = min_size, max_depth = max_depth, keep_match = keep_position, compute_stats = TRUE)
}

#' Test if the object is a context tree
#'
#' This function returns \code{TRUE} for context trees and \code{FALSE} for other objects.
#'
#' @param x an R object.
#' @return \code{TRUE} for context trees.
#' @export
#' @examples
#' dts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
#' dts_ctree <- ctx_tree(dts, min_size = 1, max_depth = 2)
#' is_ctx_tree(dts_ctree)
#' is_ctx_tree(dts)
is_ctx_tree <- function(x) {
  inherits(x, "ctx_tree")
}

assertthat::on_failure(is_ctx_tree) <- function(call, env) {
  paste0(deparse(call$x), " is not a ctx_tree")
}

#' @export
print.ctx_tree <- function(x, ...) {
  cat(paste(
    "Context tree on",
    paste(x$vals, collapse = ", ")
  ), "\n")
  if (!is.null(x$depth)) {
    cat(paste(" Maximum context length:", x$depth, "\n"))
  }
  if (!is.null(x$nb_ctx)) {
    cat(paste(" Number of contexts:", x$nb_ctx, "\n"))
  }
  invisible(x)
}

#' State space of a context tree
#'
#' This function returns the state space of a context tree.
#'
#' @param ct a context tree.
#' @return the context space of the tree.
#'
#' @export
#' @examples
#' dts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
#' dts_ctree <- ctx_tree(dts, min_size = 1, max_depth = 2)
#' ## should be c("0", "1")
#' states(dts_ctree)
states <- function(ct) {
  assertthat::assert_that(is_ctx_tree(ct))
  ct$vals
}

rec_depth <- function(ct) {
  if (is.null(ct$children)) {
    0
  } else {
    1 + max(sapply(ct$children, rec_depth))
  }
}

#' Depth of a context tree
#'
#' This function return the depth of a context tree, i.e. the length of the
#' longest context represented in the tree.
#'
#' @param ct a context tree.
#' @return the depth of the tree.
#'
#' @export
#' @examples
#' dts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
#' dts_ctree <- ctx_tree(dts, min_size = 1, max_depth = 3)
#' # should be 3
#' depth(dts_ctree)
depth <- function(ct) {
  assertthat::assert_that(is_ctx_tree(ct))
  if (!is.null(ct$depth)) {
    ct$depth
  } else {
    rec_depth(ct)
  }
}

rec_context_number <- function(ct, count_context = count_local_context) {
  nb <- count_context(ct)
  if (!is.null(ct$children)) {
    nb <- nb + sum(sapply(ct$children, rec_context_number, count_context))
  }
  nb
}

#' Number of contexts of a context tree
#'
#' This function returns the number of distinct contexts in a context tree
#'
#' @param ct a context tree
#'
#' @return the number of contexts of the tree
#' @export
#' @examples
#' dts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
#' dts_ctree <- ctx_tree(dts, min_size = 1, max_depth = 3)
#' # should be 8
#' context_number(dts_ctree)
context_number <- function(ct) {
  UseMethod("context_number")
}

#' @export
context_number.ctx_tree <- function(ct) {
  if (!is.null(ct$nb_ctx)) {
    ct$nb_ctx
  } else {
    rec_context_number(ct)
  }
}
