rec_contexts <- function(path, ct, vals) {
  if (is.null(ct$children)) {
    ## this is a leaf
    ## if there is a f_by, then this is a context
    if (is.null(ct[["f_by"]])) {
      NULL
    } else {
      list(path)
    }
  } else {
    all_ctx <- list()
    for (v in seq_along(ct$children)) {
      if (is.null(path)) {
        sub_path <- vals[v]
      } else {
        sub_path <- c(path, vals[v])
      }
      sub_ctx <- rec_contexts(sub_path, ct$children[[v]], vals)
      all_ctx <- c(all_ctx, sub_ctx)
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
#' @param ct a context tree.
#'
#' @return the list of the contexts represented in this tree.
#' @export
contexts <- function(ct) {
  UseMethod("contexts")
}

#' @export
contexts.ctx_tree <- function(ct) {
  preres <- rec_contexts(NULL, ct, ct$vals)
  if (is.null(preres[[length(preres)]])) {
    ## root context
    preres[[length(preres)]] <- ct$vals[0]
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
