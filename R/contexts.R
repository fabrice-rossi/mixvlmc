path_list_extractor <- function(path, ct, vals, control, is_leaf, p_summary) {
  if (is_leaf) {
    if (is.null(ct[["f_by"]])) {
      NULL
    } else {
      list(path)
    }
  } else {
    if (nb_sub_tree(ct) < length(vals)) {
      list(path)
    } else {
      NULL
    }
  }
}

no_summary <- function(ct) {
  NULL
}

path_df_extractor <- function(path, ct, vals, control, is_leaf, p_summary) {
  if (is_leaf) {
    if (is.null(ct[["f_by"]])) {
      NULL
    } else {
      data.frame(context = I(list(path)))
    }
  } else {
    if (nb_sub_tree(ct) < length(vals)) {
      data.frame(context = I(list(path)))
    } else {
      NULL
    }
  }
}

#' Recursive extraction of the contexts
#'
#' @param path the current context path (initial value: `NULL`)
#' @param ct the current node in the context tree (initial value: the root of the tree)
#' @param vals states
#' @param extractor extractor function, see details
#' @param control control parameter for the extractor function, see details
#' @param p_summary summary of the parent of the current node
#' @param summarize summarizing function to compute the summary submitted to the children
#'
#' @return a list or a data frame with the contexts and additional information
#' @details the contexts extractor proceeds as follows:
#'   1. if the ct has children:
#'      1. it applies the summarize function to itself (summarize(ct)).
#'      2. it calls itself recursively on each child and gather the results.
#'         A `NULL` result is discarded.
#'   2. it calls `extractor` on the current node `extractor(path, ct, vals, control, TRUE/FALSE, p_summary)`
#'      the fourth parameter is TRUE for a leaf node (no children) and FALSE for another node.
#'   3. the result is aggregated with sub results if available
#'
#'   `extractor` should only return a non NULL result if valid contexts can be extracted from the ct.
#' @noRd
rec_contexts_extractor <- function(path, ct, vals, extractor, control, summarize, p_summary) {
  if (is.null(ct$children)) {
    ## this is a leaf
    extractor(path, ct, vals, control, TRUE, p_summary)
  } else {
    all_ctx <- NULL
    l_summary <- summarize(ct)
    for (v in seq_along(ct$children)) {
      if (is.null(path)) {
        sub_path <- vals[v]
      } else {
        sub_path <- c(path, vals[v])
      }
      sub_ctx <- rec_contexts_extractor(
        sub_path, ct$children[[v]], vals,
        extractor, control, summarize, l_summary
      )
      all_ctx <- flex_append(all_ctx, sub_ctx)
    }
    local_ctx <- extractor(path, ct, vals, control, FALSE, p_summary)
    all_ctx <- flex_append(all_ctx, local_ctx)
    all_ctx
  }
}

contexts_extractor <- function(ct, reverse, extractor, control, summarize = no_summary) {
  preres <- rec_contexts_extractor(NULL, ct, ct$vals, extractor, control, summarize, summarize(ct))
  if (is.data.frame(preres)) {
    if (!reverse) {
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
  } else {
    if (!reverse) {
      preres <- lapply(preres, rev)
    }
    if (is.null(preres[[length(preres)]])) {
      preres[[length(preres)]] <- ct$vals[0]
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
#' contained in the tree (with `type="auto"` or `type="list"`). When
#' `type="data.frame"`, the method returns a data.frame whose first column,
#' named `context`, contains the contexts. Other columns contain context
#' specific values which depend on the actual class of the tree and on
#' additional parameters. An adapted return type is chosen when type="auto"`.
#'
#' @section State order in a context: Notice that contexts are given by default
#'   in the "reverse" order used by the VLMC papers: older values are on the
#'   right. For instance, the context `c(0, 1)` is reported if the sequence 1,
#'   then 0 appeared in the time series used to build the context tree. Set
#'   reverse to `FALSE` for the reverse convention.
#'
#' @param ct a context tree.
#' @param type result type (see details).
#' @param reverse logical (defaults to FALSE). See details.
#' @param ... additional arguments for the contexts function.
#'
#' @return The list of the contexts represented in this tree or a data.frame
#'   with more content.
#' @examples
#' dts <- sample(as.factor(c("A", "B", "C")), 100, replace = TRUE)
#' dts_tree <- ctx_tree(dts, max_depth = 3, min_size = 5)
#' contexts(dts_tree)
#' contexts(dts_tree, "data.frame", TRUE)
#' @seealso [contexts.ctx_tree()], [contexts.vlmc()], [contexts.covlmc()].
#' @export
contexts <- function(ct, type = c("auto", "list", "data.frame"), reverse = TRUE, ...) {
  UseMethod("contexts")
}
