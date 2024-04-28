##  context tree representation
nb_sub_tree <- function(ct) {
  if (is.null(ct) || is.null(ct$children) || length(ct$children) == 0) {
    0L
  } else {
    sum(sapply(ct$children, length) > 0)
  }
}

count_local_context <- function(node) {
  if (is.null(node$children)) {
    if (is.null(node[["f_by"]])) {
      0L
    } else {
      1L
    }
  } else {
    if (nb_sub_tree(node) < length(node$children)) {
      1L
    } else {
      0L
    }
  }
}

rec_stats_ctx_tree <- function(ct, count_context = count_local_context) {
  if (is.null(ct$children)) {
    ## this is a leaf
    ## depth = 0
    c(0L, count_context(ct))
  } else {
    subresults <- sapply(ct$children, rec_stats_ctx_tree, count_context)
    dp <- 1L + max(subresults[1, ])
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

grow_ctx_tree <- function(x, vals, min_size, max_depth, covsize = 0L, keep_match = FALSE, all_children = FALSE,
                          compute_stats = FALSE, weight = NULL) {
  recurse_ctx_tree <- function(x, nb_vals, d, from, f_by) {
    if (d < max_depth) {
      fmatch <- forward_match_all_ctx_counts(x, nb_vals, d, from)
      children <- vector(mode = "list", nb_vals)
      nb_children <- 0L
      d_max <- FALSE
      for (v in 1:nb_vals) {
        ## we look at the descendants hence the target depth is d + 1
        if (sum(fmatch$counts[v, ]) >= min_size * (1L + covsize * (d + 1L))) {
          children[[v]] <- recurse_ctx_tree(x, nb_vals, d + 1L, fmatch$positions[[v]], fmatch$counts[v, ])
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
      if (is.null(weight)) {
        result$f_by <- f_by
      } else {
        result$f_by <- f_by * weight
      }
      if (keep_match) {
        result$match <- from
      }
      if (d_max) {
        result$max_depth <- TRUE
      }
      result
    } else {
      if (is.null(weight)) {
        result <- list(f_by = f_by, max_depth = TRUE)
      } else {
        result <- list(f_by = f_by * weight, max_depth = TRUE)
      }
      if (keep_match) {
        result$match <- from
      }
      result
    }
  }
  pre_res <- recurse_ctx_tree(x, length(vals), 0L, NULL, table(x))
  if (is.null(pre_res$max_depth)) {
    pre_res$max_depth <- FALSE
  }
  new_ctx_tree(vals, pre_res, compute_stats = compute_stats)
}

#' Build a context tree for a discrete time series
#'
#' This function builds a context tree for a time series.
#'
#' The tree represents all the sequences of symbols/states of length smaller
#' than `max_depth` that appear at least `min_size` times in the time series and
#' stores the frequencies of the states that follow each context. Optionally,
#' the positions of the contexts in the time series can be stored in the tree.
#'
#' @param x an object that can be interpreted as a discrete time series, such
#'  as an integer vector or a `dts` object (see [dts()])
#' @param min_size integer >= 1 (default: 2). Minimum number of observations for
#'   a context to be included in the tree.
#' @param max_depth integer >= 1 (default: 100). Maximum length of a context to
#'   be included in the tree.
#' @param keep_position logical (default: TRUE). Should the context tree keep
#'   the position of the contexts.
#' @param backend "R" or "C++" (default: as specified by the "mixvlmc.backend"
#'   option). Specifies the implementation used to represent the context tree
#'   and to built it. See details.
#' @param ... additional parameters
#' @section Back ends:
#'
#'   Two back ends are available to compute context trees:
#'
#' - the "R" back end represents the tree in pure R data structures (nested lists)
#'   that be easily processed further in pure R (C++ helper functions are used
#'   to speed up the construction).
#' - the "C++" back end represents the tree with C++ classes. This back end is
#'   considered experimental. The tree is built with an optimised suffix tree
#'   algorithm which speeds up the construction by at least a factor 10 in
#'   standard settings. As the tree is kept outside of R direct reach, context
#'   trees built with the C++ back end must be restored after a
#'   `saveRDS()`/`readRDS()` sequence. This is done automatically by recomputing
#'   completely the context tree.
#'
#' @returns a context tree (of class that inherits from `ctx_tree`).
#' @export
#'
#' @examples
#' rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
#' ## get all contexts of length 2
#' rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 2)
#' draw(rdts_ctree)
ctx_tree <- function(x, min_size = 2L, max_depth = 100L, keep_position = TRUE,
                     backend = getOption("mixvlmc.backend", "R"), ...) {
  UseMethod("ctx_tree")
}

#' @export
#' @param x a numeric, character, factor or logical vector
#' @inherit ctx_tree
ctx_tree.default <- function(x, min_size = 2L, max_depth = 100L,
                             keep_position = TRUE,
                             backend = getOption("mixvlmc.backend", "R"),
                             ...) {
  x_dts <- dts(x)
  ctx_tree_internal(
    x_dts$ix, x_dts$vals, min_size, max_depth, keep_position,
    backend
  )
}

#' @export
#' @param x a discrete time series represented by a `dts` object as created by
#'   [dts()]
#' @inherit ctx_tree
#' @examples
#' x_dts <- dts(c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0))
#' ## get all contexts of length 2
#' ctree <- ctx_tree(x_dts, min_size = 1, max_depth = 2)
#' draw(ctree)
ctx_tree.dts <- function(x, min_size = 2L, max_depth = 100L,
                         keep_position = TRUE,
                         backend = getOption("mixvlmc.backend", "R"),
                         ...) {
  ctx_tree_internal(
    x$ix, x$vals, min_size, max_depth, keep_position,
    backend
  )
}

ctx_tree_internal <- function(ix,
                              vals,
                              min_size,
                              max_depth,
                              keep_position,
                              backend) {
  backend <- match.arg(backend, c("R", "C++"))
  if (length(vals) > max(10, 0.05 * length(ix))) {
    warning(paste0("x as numerous unique values (", length(vals), ")"))
  }
  if (backend == "R") {
    result <- grow_ctx_tree(ix, vals,
      min_size = min_size, max_depth = max_depth,
      keep_match = keep_position, compute_stats = TRUE
    )
    if (keep_position) {
      ## handle the case where the root is context
      if (!is_full_node(result)) {
        result$match <- 0:(length(ix) - 1)
      }
    }
  } else {
    cpp_tree <- build_suffix_tree(rev(ix)[-1], length(vals))
    cpp_tree$compute_counts(ix[length(ix)], keep_position)
    cpp_tree$prune(min_size, max_depth)
    result <- new_ctx_tree_cpp(vals, cpp_tree)
    ## with the C++ backend, max_depth is only used during pruning
    result$max_depth <- FALSE
    result$restoration <- cpp_tree$restoration_info()
  }
  result$keep_match <- keep_position
  result$data_size <- length(ix)
  if (depth(result) > 0) {
    result$ix <- ix[1:min(depth(result), length(ix))]
  }
  result
}

#' Test if the object is a context tree
#'
#' This function returns `TRUE` for context trees and `FALSE` for other objects.
#'
#' @param x an R object.
#' @returns `TRUE` for context trees.
#' @export
#' @examples
#' rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
#' rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 2)
#' is_ctx_tree(rdts_ctree)
#' is_ctx_tree(rdts)
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
  if (!is.null(x$nb_ctx)) {
    cat(paste(" Number of contexts:", x$nb_ctx, "\n"))
  }
  if (!is.null(x$depth)) {
    cat(paste(" Maximum context length:", x$depth, "\n"))
  }
  invisible(x)
}

#' State space of an object
#'
#' This function returns the state space of an object for which this is
#' meaningful such as a discrete time series or a context tree.
#'
#' @param x an object with a state space.
#' @returns the state space of the object (as vector of states).
#'
#' @export
#' @examples
#' rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
#' rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 2)
#' ## should be c(0, 1)
#' states(rdts_ctree)
states <- function(x) {
  UseMethod("states")
}

#' @export
#' @rdname states
states.ctx_tree <- function(x) {
  x$vals
}

rec_depth <- function(ct) {
  if (is.null(ct$children)) {
    0L
  } else {
    1L + max(sapply(ct$children, rec_depth))
  }
}

#' Depth of a context tree
#'
#' This function returns the depth of a context tree, i.e. the length of the
#' longest context represented in the tree.
#'
#' @param ct a context tree.
#' @returns the depth of the tree.
#'
#' @export
#' @examples
#' rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
#' rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 3)
#' ## should be 3
#' depth(rdts_ctree)
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
#' This function returns the number of distinct contexts in a context tree.
#'
#' @param ct a context tree.
#'
#' @returns the number of contexts of the tree.
#' @export
#' @examples
#' rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
#' rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 3)
#' # should be 8
#' context_number(rdts_ctree)
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

is_full_node <- function(node) {
  if (is.null(node)) {
    FALSE
  } else if (is.null(node$children)) {
    FALSE
  } else {
    sum(sapply(node$children, length) > 0) == length(node$children)
  }
}

count_full_nodes <- function(ct) {
  rec_context_number(ct, \(x) as.integer(is_full_node(x)))
}
