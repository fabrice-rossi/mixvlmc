#' Summary of a VLMC
#'
#' This function computes summary statistics of a VLMC, which are displayed by
#' the associated `print()` method.
#'
#' @param object a fitted VLMC (a `vlmc` object).
#' @param x a summary of a VLMC, as returned by `summary()`.
#' @param ... additional arguments (not used).
#'
#' @returns `summary()` returns an object of class `summary.vlmc` (and
#'   `summary.vlmc_cpp` for a VLMC built with the C++ backend), a list with
#'   the following components:
#'   - `state_space`: the state space of the VLMC;
#'   - `nb`: the number of contexts;
#'   - `depth`: the length of the longest context;
#'   - `avg_depth`: the average length of the contexts;
#'   - `cutoff`: the cut off of the VLMC in native scale (see [cutoff.vlmc()]);
#'   - `alpha`: the cut off of the VLMC in quantile scale.
#'
#'   `print()` displays the summary and returns `x` invisibly.
#' @seealso [vlmc()], [contexts.vlmc()], [context_number()], [depth()]
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' rdts <- cut(pc$active_power,
#'   breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1)))
#' )
#' model <- vlmc(rdts)
#' summary(model)
#' ## the components of the summary
#' unclass(summary(model))
#' @exportS3Method
summary.vlmc <- function(object, ...) {
  rlang::check_dots_empty()
  ## the list of ctx_node is the fastest representation of the contexts
  ## with the R backend
  summary_vlmc_from_lengths(object, vapply(contexts(object), length, 1L))
}

summary_vlmc_from_lengths <- function(object, all_length) {
  res <- list(
    state_space = states(object),
    nb = length(all_length),
    depth = max(all_length),
    avg_depth = mean(all_length),
    cutoff = object$cutoff,
    alpha = object$alpha
  )
  structure(res, class = "summary.vlmc")
}

print_summary_vlmc <- function(x) {
  cat(paste(" Cutoff: ", signif(x$cutoff, 4), " (quantile: ", signif(x$alpha, 4), ")\n", sep = ""))
  cat(paste(" Number of contexts:", x$nb, "\n"))
  cat(paste(" Maximum context length:", x$depth, "\n"))
  cat(paste(" Average context length:", signif(x$avg_depth, 4), "\n"))
}

#' @rdname summary.vlmc
#' @exportS3Method
print.summary.vlmc <- function(x, ...) {
  cat(paste(
    "VLMC context tree on",
    paste(x$state_space, collapse = ", ")
  ), "\n")
  print_summary_vlmc(x)
  invisible(x)
}
