path_list_extractor <- function(tree, path, ct, vals, control, is_leaf, p_summary) {
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

path_df_extractor <- function(tree, path, ct, vals, control, is_leaf, p_summary) {
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
#' @returns a list or a data frame with the contexts and additional information
#' @details the contexts extractor proceeds as follows:
#'   1. if the ct has children:
#'      1. it applies the summarize function to itself (summarize(ct)).
#'      2. it calls itself recursively on each child and gather the results.
#'         A `NULL` result is discarded.
#'   2. it calls `extractor` on the current node `extractor(tree, path, ct, vals, control, TRUE/FALSE, p_summary)`
#'      the fourth parameter is TRUE for a leaf node (no children) and FALSE for another node.
#'   3. the result is aggregated with sub results if available
#'
#'   `extractor` should only return a non NULL result if valid contexts can be extracted from the ct.
#' @noRd
rec_contexts_extractor <- function(tree, path, ct, vals, extractor, control, summarize, p_summary) {
  if (is.null(ct$children)) {
    ## this is a leaf
    extractor(tree, path, ct, vals, control, TRUE, p_summary)
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
        tree,
        sub_path, ct$children[[v]], vals,
        extractor, control, summarize, l_summary
      )
      all_ctx <- flex_append(all_ctx, sub_ctx)
    }
    local_ctx <- extractor(tree, path, ct, vals, control, FALSE, p_summary)
    all_ctx <- flex_append(all_ctx, local_ctx)
    all_ctx
  }
}

contexts_extractor <- function(ct, reverse, extractor, control, summarize = no_summary) {
  ## ct is the tree (i.e. the root of the tree)
  preres <- rec_contexts_extractor(ct, NULL, ct, ct$vals, extractor, control, summarize, summarize(ct))
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
    ## we have a list of ctx_node
    if (!reverse) {
      ## reverse each ctx node
      preres <- lapply(preres, \(x) rev(x))
    }
    if (is.null(preres[[length(preres)]])) {
      ## should never happen
      stop("internal error in contexts_extractor")
      ## preres[[length(preres)]] <- ct$vals[0]
    }
    preres
  }
}

#' Contexts of a context tree
#'
#' This function extracts from a context tree a description of all of its
#' contexts.
#'
#' The default behaviour consists in returning a list of all the contexts
#' contained in the tree using `ctx_node` objects (as returned by e.g.
#' [find_sequence()]) (with `type="list"`). The properties of the contexts can
#' then be explored using adapted functions such as [counts()] and
#' [positions()]. The result list is of class `contexts`. When `sequence=TRUE`,
#' the method returns a data.frame whose first column, named `context`, contains
#' the contexts as vectors (i.e. the value returned by `as_sequence()` applied
#' to a `ctx_node` object). Other columns contain context specific values which
#' depend on the actual class of the tree and on additional parameters. In all
#' implementations of `contexts()`, setting the additional parameters to any no
#' default value leads to a `data.frame` result.
#'
#' @section State order in a context: Notice that contexts are given by default
#'   in the temporal order and not in the "reverse" order used by many VLMC
#'   research papers: older values are on the left. For instance, the context
#'   `c(1, 0)` is reported if the sequence 0, then 1 appeared in the time series
#'   used to build the context tree. Set reverse to `TRUE` for the reverse
#'   convention which is somewhat easier to relate to the way the context trees
#'   are represented by [draw()] (i.e. recent values at the top the tree).
#'
#' @param ct a context tree.
#' @param sequence if `TRUE` the function returns its results as a `data.frame`,
#'   if `FALSE` (default) as a list of `ctx_node` objects. (see details)
#' @param reverse logical (defaults to `FALSE`). See details.
#' @param ... additional arguments for the contexts function.
#'
#' @returns A list of class `contexts` containing the contexts represented in
#'   this tree (as `ctx_node`) or a data.frame.
#' @examples
#' dts <- sample(as.factor(c("A", "B", "C")), 100, replace = TRUE)
#' dts_tree <- ctx_tree(dts, max_depth = 3, min_size = 5)
#' contexts(dts_tree)
#' contexts(dts_tree, TRUE, TRUE)
#' @seealso [find_sequence()] and [find_sequence.covlmc()] for direct access to
#'   a specific context, and [contexts.ctx_tree()], [contexts.vlmc()] and
#'   [contexts.covlmc()] for concrete implementations of `contexts()`.
#' @export
contexts <- function(ct, sequence = FALSE, reverse = FALSE, ...) {
  UseMethod("contexts")
}

new_context_list <- function(ctx_list, ..., class = character()) {
  structure(ctx_list, ..., class = c(class, "contexts", class(ctx_list)))
}

#' Print a context list
#'
#' This function prints a list of contexts i.e. a `contexts` object listing
#' `ctx_node` objects.
#'
#' @param x the `contexts` object to print
#' @param reverse specifies whether the contexts should be reported in
#'   temporal order (`FALSE`, default value) or in reverse temporal order (`TRUE`).
#'   If the parameter is not specified, the contexts are displayed in order
#'   specified by the call to `contexts()` used to build the context list.
#' @param ... additional arguments for the print function.
#' @returns the `x` object, invisibly
#' @seealso [contexts()]
#' @export
#' @examples
#' dts <- c("A", "B", "C", "A", "A", "B", "B", "C", "C", "A")
#' dts_tree <- ctx_tree(dts, max_depth = 3)
#' print(contexts(dts_tree))
print.contexts <- function(x, reverse = TRUE, ...) {
  cat("Contexts:\n")
  if (missing(reverse)) {
    for (i in seq_along(x)) {
      the_seq <- as_sequence(x[[i]])
      cat(" ", paste(the_seq, collapse = ", "), "\n", sep = "")
    }
  } else {
    for (i in seq_along(x)) {
      the_seq <- as_sequence(x[[i]], reverse = reverse)
      cat(" ", paste(the_seq, collapse = ", "), "\n", sep = "")
    }
  }
  invisible(x)
}
