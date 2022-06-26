##  context tree representation
nb_sub_tree <- function(ct) {
  if (is.null(ct) || is.null(ct$children) || length(ct$children) == 0) {
    0
  } else {
    sum(sapply(ct$children, length) > 0)
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

#' Test if the object is a context tree
#'
#' This function returns \code{TRUE} for context trees and \code{FALSE} for other objects.
#'
#' @param x an R object.
#' @return \code{TRUE} for context trees.
#' @export
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
depth <- function(ct) {
  assertthat::assert_that(is_ctx_tree(ct))
  if (!is.null(ct$depth)) {
    ct$depth
  } else {
    rec_depth(ct)
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

rec_contexts <- function(path, ct, vals) {
  if (is.null(ct$children)) {
    ## this is a leaf
    ## if there is a f_by, then this is a context
    if (is.null(ct$f_by)) {
      NULL
    } else {
      list(path)
    }
  } else {
    all_ctx <- list()
    for (v in seq_along(ct$children)) {
      sub_ctx <- rec_contexts(c(path, vals[v]), ct$children[[v]], vals)
      if (!is.null(sub_ctx)) {
        all_ctx <- c(all_ctx, sub_ctx)
      }
    }
    if (nb_sub_tree(ct) < length(vals)) {
      all_ctx <- c(all_ctx, list(path))
    }
    all_ctx
  }
}

#' Contexts of a context tree
#'
#' This function extracts from a context tree the list of all its contexts.
#'
#' @param ct a context tree
#'
#' @return the list of the contexts represented in this tree.
#' @export
contexts <- function(ct) {
  UseMethod("contexts")
}

#' @export
contexts.ctx_tree <- function(ct) {
  preres <- rec_contexts(c(), ct, ct$vals)
  if (is.null(preres[[length(preres)]])) {
    ## root context
    preres[[length(preres)]] <- list()
  }
  preres
}

rec_match_context <- function(tree, d, ctx) {
  if (length(ctx) == 0) {
    list(tree = tree, depth = d)
  } else {
    if (is.null(tree$children)) {
      list(tree = tree, depth = d)
    } else {
      cand <- tree$children[[ctx[1]]]
      if (length(cand) > 0) {
        rec_match_context(cand, d + 1, ctx[-1])
      } else {
        list(tree = tree, depth = d)
      }
    }
  }
}

match_context <- function(tree, ctx) {
  rec_match_context(tree, 0, ctx)
}
