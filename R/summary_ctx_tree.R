#' Summary of a context tree
#'
#' This function computes summary statistics of a context tree, which are
#' displayed by the associated `print()` method.
#'
#' @param object a context tree (a `ctx_tree` object).
#' @param x a summary of a context tree, as returned by `summary()`.
#' @param ... additional arguments (not used).
#'
#' @returns `summary()` returns an object of class `summary.ctx_tree` (and
#'   `summary.ctx_tree_cpp` for a context tree built with the C++ backend),
#'   a list with the following components:
#'   - `state_space`: the state space of the context tree;
#'   - `nb`: the number of contexts;
#'   - `depth`: the length of the longest context;
#'   - `avg_depth`: the average length of the contexts.
#'
#'   `print()` displays the summary and returns `x` invisibly.
#' @seealso [ctx_tree()], [contexts()], [context_number()], [depth()]
#' @examples
#' rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0)
#' rdts_ctree <- ctx_tree(rdts, min_size = 2, max_depth = 4)
#' summary(rdts_ctree)
#' ## the components of the summary
#' unclass(summary(rdts_ctree))
#' @exportS3Method
summary.ctx_tree <- function(object, ...) {
  rlang::check_dots_empty()
  ctx <- contexts(object)
  all_length <- sapply(ctx, length)
  res <- list(
    state_space = states(object),
    nb = length(ctx),
    depth = max(all_length),
    avg_depth = mean(all_length)
  )
  structure(res, class = "summary.ctx_tree")
}

print_summary_ctx_tree <- function(x) {
  cat(paste(" Number of contexts:", x$nb, "\n"))
  cat(paste(" Maximum context length:", x$depth, "\n"))
  cat(paste(" Average context length:", signif(x$avg_depth, 4), "\n"))
}

#' @rdname summary.ctx_tree
#' @exportS3Method
print.summary.ctx_tree <- function(x, ...) {
  cat(paste(
    "Context tree on",
    paste(x$state_space, collapse = ", ")
  ), "\n")
  print_summary_ctx_tree(x)
  invisible(x)
}
