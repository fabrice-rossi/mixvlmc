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

rec_contexts_extractor <- function(path, ct, vals, extractor, control) {
  if (is.null(ct$children)) {
    ## this is a leaf
    ## if there is a f_by, then this is a context
    if (is.null(ct[["f_by"]])) {
      NULL
    } else {
      content <- extractor(ct, control)
      cbind(data.frame(context = I(list(path))), content)
    }
  } else {
    all_ctx <- NULL
    for (v in seq_along(ct$children)) {
      if (is.null(path)) {
        sub_path <- vals[v]
      } else {
        sub_path <- c(vals[v], path)
      }
      sub_ctx <- rec_contexts_extractor(sub_path, ct$children[[v]], vals, extractor, control)
      all_ctx <- rbind(all_ctx, sub_ctx)
    }
    if (nb_sub_tree(ct) < length(vals)) {
      all_ctx <- rbind(
        all_ctx,
        data.frame(
          context = I(list(path)),
          extractor(ct, control)
        )
      )
    }
    all_ctx
  }
}

ctx_context_extractor <- function(ctx, control) {
  res <- data.frame(freq = sum(ctx[["f_by"]]))
  if (!is.null(control[["vals"]])) {
    freq_by_val <- as.list(ctx[["f_by"]])
    names(freq_by_val) <- as.character(control[["vals"]])
    res <- cbind(res, data.frame(freq_by_val))
  }
  res
}

#' Contexts of a context tree
#'
#' This function extracts from a context tree a description of all of its
#' contexts.
#'
#' The default behavior consists in returning a list of all the contexts
#' contained in the tree (with `type="list"`). When
#' `type="data.frame"`, the method returns a data.frame whose first column,
#' named `context`, contains the contexts. Other columns contain context
#' specific values which depend on the actual class of the tree and on
#' additional parameters.
#'
#' @section State order in a context: Notice that contexts are given by default
#'   in their left to right reading order. For instance, the context `c(0, 1)` is
#'   reported if the sequence 0, then 1, is registered in the context tree. Set
#'   reverse to `TRUE` for the reverse convention.
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

#' @inherit contexts
#' @param frequency specify the counts to be included in the result data.frame.
#'   The default value of `NULL` does not include anything. `"total"`
#'   gives the number of occurrences of each context in the original sequence.
#'   `"detailed"` includes in addition the break down of these occurrences
#'   into all the possible states.
#' @details The default result for `type="list"` and `frequency=NULL`
#'   is the list of all contexts.
#'
#'   Other results are obtained only with `type="data.frame"`. In this case
#'   the resulting `data.frame` has a `context` column storing the
#'   contexts. If `frequency="total"`, an additional column named
#'   `freq` gives the number of occurrences of each context in the series
#'   used to build the tree. If `frequency="detailed"`, one additional
#'   column is added per state in the context space. Each column records the
#'   number of times a given context is followed by the corresponding value in
#'   the original series.
#' @examples
#' dts <- sample(as.factor(c("A", "B", "C")), 100, replace = TRUE)
#' dts_tree <- ctx_tree(dts, max_depth = 3, min_size = 5)
#' contexts(dts_tree)
#' contexts(dts_tree, "data.frame", frequency = "total")
#' contexts(dts_tree, "data.frame", frequency = "detailed")
#' @export
contexts.ctx_tree <- function(ct, type = c("list", "data.frame"), reverse = FALSE, frequency = NULL, ...) {
  type <- match.arg(type)
  if (missing(frequency)) {
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
      data.frame(context = I(preres))
    }
  } else {
    assertthat::assert_that(type == "data.frame")
    assertthat::assert_that(frequency %in% c("total", "detailed"))
    control <- NULL
    if (frequency == "detailed") {
      control <- list(vals = ct$vals)
    }
    preres <- rec_contexts_extractor(NULL, ct, ct$vals, ctx_context_extractor, control)
    if (reverse) {
      new_res <- data.frame(context = I(lapply(preres$context, rev)))
      preres <- cbind(new_res, preres[2:ncol(preres)])
    }
    preres
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
