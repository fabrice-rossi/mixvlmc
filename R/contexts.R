rec_contexts_extractor <- function(path, ct, vals, extractor, control, p_summary, summarize) {
  if (is.null(ct$children)) {
    ## this is a leaf
    ## if there is a f_by, then this is a context
    if (is.null(ct[["f_by"]])) {
      NULL
    } else {
      if (is.null(extractor)) {
        list(path)
      } else {
        content <- extractor(ct, vals, control, TRUE, p_summary)
        if (!is.null(content)) {
          cbind(data.frame(context = I(list(path))), content)
        } else {
          data.frame(context = I(list(path)))
        }
      }
    }
  } else {
    all_ctx <- NULL
    for (v in seq_along(ct$children)) {
      if (is.null(path)) {
        sub_path <- vals[v]
      } else {
        sub_path <- c(vals[v], path)
      }
      sub_ctx <- rec_contexts_extractor(
        sub_path, ct$children[[v]], vals,
        extractor, control, summarize(ct), summarize
      )
      if (is.null(extractor)) {
        all_ctx <- c(all_ctx, sub_ctx)
      } else {
        all_ctx <- rbind(all_ctx, sub_ctx)
      }
    }
    ## missing context children indicates that the current node is also a context
    if (nb_sub_tree(ct) < length(vals)) {
      if (is.null(extractor)) {
        all_ctx <- c(all_ctx, list(path))
      } else {
        content <- extractor(ct, vals, control, FALSE, p_summary)
        if (is.null(content)) {
          all_ctx <- rbind(
            all_ctx,
            data.frame(context = I(list(path)))
          )
        } else {
          all_ctx <- rbind(all_ctx, data.frame(context = I(list(path)), content))
        }
      }
    }
    all_ctx
  }
}

contexts_extractor <- function(ct, is_list, reverse, extractor, control, summarize_parent = function(ct) {
                                 NULL
                               }) {
  if (is_list) {
    preres <- rec_contexts_extractor(NULL, ct, ct$vals, NULL, NULL)
    if (reverse) {
      preres <- lapply(preres, rev)
    }
    if (is.null(preres[[length(preres)]])) {
      preres[[length(preres)]] <- ct$vals[0]
    }
    preres
  } else {
    preres <-
      rec_contexts_extractor(
        NULL, ct, ct$vals, extractor, control,
        summarize_parent(ct), summarize_parent
      )
    if (reverse) {
      new_res <- data.frame(context = I(lapply(preres$context, rev)))
      if (ncol(preres) > 1) {
        preres <- cbind(new_res, preres[2:ncol(preres)])
      } else {
        preres <- new_res
      }
    }
    if (is.null(preres[nrow(preres), 1][[1]])) {
      preres[nrow(preres), 1][[1]] <- list(ct$vals[0])
    }
    preres
  }
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
