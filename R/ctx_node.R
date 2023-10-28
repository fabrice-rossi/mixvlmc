#' Create `ctx_node`
#'
#' @param ctx the sequence in reverse order
#' @param tree the tree
#' @param node the node representing the context
#' @param rev whether the sequence should be reported in reverse order or in
#'   temporal order
#' @param ... additional parameters
#' @param class finer class
#' @noRd
new_ctx_node <- function(ctx, tree, node, rev, ..., class = character()) {
  structure(
    list(
      sequence = ctx, node = node, tree = tree,
      is_context = count_local_context(node) > 0,
      rev = rev, ...
    ),
    class = c(class, "ctx_node")
  )
}

#' @export
print.ctx_node <- function(x, ...) {
  if (x$is_context) {
    cat("Context")
  } else {
    cat("Sequence")
  }
  if (x$rev) {
    cat(" [R]: ")
    x_seq <- x$sequence
  } else {
    cat(" [T]: ")
    x_seq <- rev(x$sequence)
  }
  cat(paste(x_seq, collapse = ", "), "\n")
  cat(" followed by ", paste(paste(x$tree$vals, x$node$f_by, sep = " ("), collapse = "), "), ")\n", sep = "")
}

#' @export
length.ctx_node <- function(x) {
  length(x$sequence)
}

is_ctx_node <- function(node) {
  methods::is(node, "ctx_node")
}

assertthat::on_failure(is_ctx_node) <- function(call, env) {
  paste0(deparse(call$node), " is not a ctx_node object")
}

#' Report the ordering convention of the node
#'
#' This function returns `TRUE` if the node is using a reverse temporal ordering
#' and `FALSE` in the other case.
#'
#' @param node a `ctx_node` object as returned by [find_sequence()]
#' @returns `TRUE` if the node `node` use a reverse temporal ordering, `FALSE`
#'   when this is not the case
#' @examples
#' dts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
#' dts_ctree <- ctx_tree(dts, min_size = 1, max_depth = 3)
#' is_reversed(find_sequence(dts_ctree, c(0, 0)))
#' is_reversed(find_sequence(dts_ctree, c(1, 0), reverse = TRUE))
#' @export
#' @seealso [rev.ctx_node()]
is_reversed <- function(node) {
  assertthat::assert_that(is_ctx_node(node))
  node$rev
}

#' Reverse Sequence
#'
#' This function reverses the order in which the sequence represented by the
#' `ctx_node` parameter will be reported in other functions, mainly
#' [as_sequence()].
#'
#' @param x a `ctx_node` object as returned by [find_sequence()]
#' @returns a `ctx_node` using the opposite ordering convention as the parameter
#'   of the function
#' @export
#' @examples
#' dts <- c("A", "B", "C", "A", "A", "B", "B", "C", "C", "A")
#' dts_tree <- ctx_tree(dts, max_depth = 3)
#' res <- find_sequence(dts_tree, c("A", "B"))
#' print(res)
#' r_res <- rev(res)
#' print(r_res)
#' as_sequence(r_res)
#' @seealso [is_reversed()]
rev.ctx_node <- function(x) {
  x$rev <- !x$rev
  x
}

#' Extract the sequence encoded by a node
#'
#' This function returns the sequence represented by the `node` object.
#'
#' @param node a `ctx_node` object as returned by [find_sequence()]
#' @param reverse specifies whether the sequence should be reported in reverse
#'   temporal order (`TRUE`) or in the temporal order (`FALSE`). Defaults to the
#'   order associated to the `ctx_node` which is determined by the parameters of
#'   the call to [contexts()] or [find_sequence()].
#'
#' @returns the sequence represented by the `node` object, a vector
#' @export
#'
#' @examples
#' dts <- c("A", "B", "C", "A", "A", "B", "B", "C", "C", "A")
#' dts_tree <- ctx_tree(dts, max_depth = 3)
#' res <- find_sequence(dts_tree, "A")
#' as_sequence(res)
as_sequence <- function(node, reverse) {
  assertthat::assert_that(is_ctx_node(node))
  if (missing(reverse)) {
    reverse <- node$rev
  }
  if (reverse) {
    node$sequence
  } else {
    rev(node$sequence)
  }
}

#' Find the node of a sequence in a context tree
#'
#' This function checks whether the sequence `ctx` is represented in the context
#' tree `ct`. If this is the case, it returns a description of matching node, an
#' object of class `ctx_node`. If the sequence is not represented in the tree,
#' the function return `NULL`.
#'
#' The function looks for sequences in general. The [is_context()] function can
#' be used on the resulting object to test if the sequence is in addition a
#' proper context.
#'
#' @param ct a context tree.
#' @param ctx a sequence to search in the context tree
#' @param reverse specifies whether the sequence `ctx` is given the
#'   temporal order (`FALSE`, default value) or in the reverse temporal order
#'   (`TRUE`). See the dedicated section.
#' @param ... additional parameters for the find_sequence function
#' @returns an object of class `ctx_node` if the sequence `ctx` is represented
#'   in the context tree, `NULL` when this is not the case.
#' @section State order in a sequence: sequence are given by default
#'   in the temporal order and not in the "reverse" order used by many VLMC
#'   research papers: older values are on the left. For instance, the context
#'   `c(1, 0)` is reported if the sequence 0, then 1 appeared in the time series
#'   used to build the context tree. In the present function, `reverse` refers
#'   both to the order used for the `ctx` parameter and for the default order used by the resulting `ctx_node` object.
#' @examples
#' dts <- c("A", "B", "C", "A", "A", "B", "B", "C", "C", "A")
#' dts_tree <- ctx_tree(dts, max_depth = 3)
#' find_sequence(dts_tree, "A")
#' ## returns NULL as "A" "C" does not appear in dts
#' find_sequence(dts_tree, c("A", "C"))
#' @export
find_sequence <- function(ct, ctx, reverse = FALSE, ...) {
  UseMethod("find_sequence")
}

#' @export
#' @rdname find_sequence
find_sequence.ctx_tree <- function(ct, ctx, reverse = FALSE, ...) {
  if (length(ctx) == 0) {
    if (isTRUE(ct$keep_match) && is.null(ct$match)) {
      ct$match <- 1:ct$data_size
    }
    new_ctx_node(ctx, ct, ct, reverse)
  } else {
    assertthat::assert_that((typeof(ctx) == typeof(ct$vals)) && methods::is(ctx, class(ct$vals)),
      msg = "ctx is not compatible with the model state space"
    )
    if (!reverse) {
      ctx <- rev(ctx)
    }
    nx <- to_dts(ctx, ct$vals)
    current <- ct
    for (k in seq_along(ctx)) {
      if (is.null(current$children)) {
        return(NULL)
      }
      candidate <- current$children[[1 + nx$ix[k]]]
      if (is.null(candidate) || length(candidate) == 0) {
        return(NULL)
      }
      current <- candidate
    }
    new_ctx_node(ctx, ct, current, reverse)
  }
}

#' Report the nature of a node in a context tree
#'
#' This function returns `TRUE` if the node is a proper context, `FALSE`
#' in the other case.
#'
#' @param node a `ctx_node` object as returned by [find_sequence()]
#' @returns `TRUE` if the node `node` is a proper context,
#'   `FALSE` when this is not the case
#' @examples
#' dts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
#' dts_ctree <- ctx_tree(dts, min_size = 1, max_depth = 3)
#' draw(dts_ctree)
#' ## 0, 0 is a context but 1, 0 is not
#' is_context(find_sequence(dts_ctree, c(0, 0)))
#' is_context(find_sequence(dts_ctree, c(1, 0)))
#' @export
is_context <- function(node) {
  assertthat::assert_that(is_ctx_node(node))
  node$is_context
}

#' Report the positions of a sequence associated to a node
#'
#' This function returns the positions of the sequence represented by `node`
#' in the time series used to build the context tree in which the sequence is
#' represented. This is only possible is those positions were saved during the
#' construction of the context tree. In positions were not saved, a call to this
#' function produces an error.
#'
#' A position of a sequence `ctx` in the time series `x` is an index value `t`
#' such that the sequence ends with `x[t]`. Thus `x[t+1]` is after the context.
#' For instance if `x=c(0, 0, 1, 1)` and `ctx=c(0, 1)` (in standard state
#' order), then the position of `ctx` in `x` is 3.
#'
#' @param node a `ctx_node` object as returned by [find_sequence()]
#'
#' @returns positions of the sequence represented by `node` is the original
#'   time series as a integer vector
#' @export
#'
#' @examples
#' dts <- sample(as.factor(c("A", "B", "C")), 100, replace = TRUE)
#' dts_tree <- ctx_tree(dts, max_depth = 3, min_size = 5)
#' subseq <- find_sequence(dts_tree, factor(c("B", "A"), levels = c("A", "B", "C")))
#' if (!is.null(subseq)) {
#'   positions(subseq)
#' }
positions <- function(node) {
  UseMethod("positions")
}

#' @export
#' @rdname positions
positions.ctx_node <- function(node) {
  if (is.null(node$node[["match"]])) {
    stop("Cannot report positions if they were not saved")
  }
  node$node[["match"]] + length(node$sequence)
}

#' Report the distribution of values that follow occurrences of a sequence
#'
#' This function reports the number of occurrences of the sequence represented
#' by `node` in the original time series used to build the associated context
#' tree (not including a possible final occurrence not followed by any value at
#' the end of the original time series). In addition if `frequency=="detailed"`,
#' the function reports the frequencies of each of the possible value of the
#' time series when they appear just after the sequence.
#'
#' @param node a `ctx_node` object as returned by [find_sequence()]
#' @param frequency specifies the counts to be included in the result. `"total"`
#'   gives the number of occurrences of the sequence in the original sequence.
#'   `"detailed"` includes in addition the break down of these occurrences into
#'   all the possible states.
#' @param local specifies how the counts are computed. When `local` is `FALSE`
#'   (default value) the counts include both counts that are specific to the
#'   context (if any) and counts from the descendants of the context in the
#'   tree. When `local` is `TRUE` the counts include only the number of times
#'   the context appears without being the last part of a longer context.
#' @returns either an integer when `frequency="total"` which gives the total
#'   number of occurrences of the sequence represented by `node` or a
#'   `data.frame` with a `total` column with the same value and a column for
#'   each of the possible value of the original time series, reporting counts in
#'   each column (see the description above).
#' @export
#' @seealso [contexts()] and [contexts.ctx_tree()]
#' @examples
#' dts <- sample(as.factor(c("A", "B", "C")), 100, replace = TRUE)
#' dts_tree <- ctx_tree(dts, max_depth = 3, min_size = 5)
#' subseq <- find_sequence(dts_tree, factor(c("A", "A"), levels = c("A", "B", "C")))
#' if (!is.null(subseq)) {
#'   counts(subseq)
#' }
counts <- function(node,
                   frequency = c("detailed", "total"),
                   local = FALSE) {
  UseMethod("counts")
}

#' @export
#' @rdname counts
counts.ctx_node <- function(node,
                            frequency = c("detailed", "total"),
                            local = FALSE) {
  frequency <- match.arg(frequency)
  assertthat::assert_that(rlang::is_logical(local))
  freqs <- node$node[["f_by"]]
  if (local) {
    if (!is.null(node$node[["children"]])) {
      for (k in seq_along(node$node[["children"]])) {
        child <- node$node[["children"]][[k]]
        if (!is.null(child[["f_by"]])) {
          freqs <- freqs - child[["f_by"]]
        }
      }
    }
  }
  if (frequency == "total") {
    sum(freqs)
  } else {
    freq_by_val <- as.list(freqs)
    names(freq_by_val) <- as.character(node$tree$vals)
    freq_by_val <- c(list(total = sum(freqs)), freq_by_val)
    data.frame(freq_by_val, check.names = FALSE)
  }
}

probs <- function(node) {
  UseMethod("probs")
}

#' @export
probs.ctx_node <- function(node) {
  node$node[["f_by"]] / sum(node$node[["f_by"]])
}
