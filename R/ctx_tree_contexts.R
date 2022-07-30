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
        sub_path <- c(vals[v], path)
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
#' The default behavior consists in returnin a list of all the contexts
#' contained in the tree (with `type="list"`). When
#' `type="data.frame"`, the method returns a data.frame whose first column
#' contains the contexts. Other columns contain context specific values which
#' depend on the actual class of the tree.
#'
#' Notice that contexts are given by default in their left to right reading
#' order. For instance, the context c(0, 1) is reported if the sequence 0, then
#' 1, is registered in the context tree. Set reverse to `TRUE` for the
#' reverse convention.
#'
#' @param ct a context tree.
#' @param type result type (see details).
#' @param reverse logical (defaults to FALSE). See details.
#' @param ... additional arguments for the contexts function.
#'
#' @return the list of the contexts represented in this tree or a data.frame
#'   with more content.
#' @examples
#' dts <- sample(as.factor(c("A", "B", "C")), 100, replace = TRUE)
#' dts_tree <- ctx_tree(dts, max_depth = 3, min_size = 5)
#' contexts(dts_tree)
#' contexts(dts_tree, "data.frame", TRUE)
#' @export
contexts <- function(ct, type = c("list", "data.frame"), reverse = FALSE, ...) {
  UseMethod("contexts")
}

#' @export
contexts.ctx_tree <- function(ct, type = c("list", "data.frame"), reverse = FALSE, ...) {
  type <- match.arg(type)
  preres <- rec_contexts(NULL, ct, ct$vals)
  if (reverse) {
    preres <- lapply(preres, rev)
  }
  if (is.null(preres[[length(preres)]])) {
    ## root context
    preres[[length(preres)]] <- ct$vals[0]
  }
  if (type == "list") {
    preres
  } else {
    data.frame(contexts = I(preres))
  }
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
