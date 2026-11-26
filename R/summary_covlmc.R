#' Summary of a VLMC with covariates
#'
#' This function computes summary statistics of a VLMC with covariates, which
#' are displayed by the associated `print()` method.
#'
#' @param object a fitted VLMC with covariates (a `covlmc` object).
#' @param x a summary of a VLMC with covariates, as returned by `summary()`.
#' @param ... additional arguments (not used).
#'
#' @returns `summary()` returns an object of class `summary.covlmc`, a list
#'   with the following components:
#'   - `state_space`: the state space of the model;
#'   - `nb`: the number of contexts;
#'   - `depth`: the length of the longest context;
#'   - `avg_depth`: the average length of the contexts;
#'   - `alpha`: the cut off of the model in quantile scale;
#'   - `covariates`: the names of the covariates;
#'   - `no_cov`: the number of contexts whose model does not use the
#'   covariates;
#'   - `avg_hsize`: the average length of the memory of the covariates (see
#'   [covariate_memory()]), computed on the contexts whose model uses the
#'   covariates (`NaN` if there is none);
#'   - `max_hsize`: the longest memory of the covariates (see
#'   [covariate_depth()]);
#'   - `delta`: the average difference between the length of a context and
#'   the memory of its model, computed on the contexts whose model uses the
#'   covariates (`NaN` if there is none).
#'
#'   `print()` displays the summary and returns `x` invisibly.
#' @seealso [covlmc()], [contexts.covlmc()], [context_number.covlmc()],
#'   [depth()], [covariate_depth()]
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' rdts <- cut(pc$active_power,
#'   breaks = c(0, quantile(pc$active_power, probs = c(1 / 3, 2 / 3, 1)))
#' )
#' rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
#' m_cov <- covlmc(rdts, rdts_cov, min_size = 5)
#' summary(m_cov)
#' @exportS3Method
summary.covlmc <- function(object, ...) {
  rlang::check_dots_empty()
  ctx <- contexts(object, frequency = "detailed", hsize = TRUE)
  all_length <- sapply(ctx$context, length)
  delta_cov_depth <- all_length - ctx$hsize
  res <- list(
    state_space = states(object),
    nb = nrow(ctx),
    depth = max(all_length),
    avg_depth = mean(all_length),
    alpha = object$alpha,
    covariates = object$cov_names,
    no_cov = sum(ctx$hsize == 0),
    avg_hsize = mean(ctx$hsize[ctx$hsize > 0]),
    max_hsize = max(ctx$hsize),
    delta = mean(delta_cov_depth[ctx$hsize > 0])
  )
  structure(res, class = "summary.covlmc")
}

#' @rdname summary.covlmc
#' @exportS3Method
print.summary.covlmc <- function(x, ...) {
  cat(paste(
    "VLMC with covariates context tree on",
    paste(x$state_space, collapse = ", ")
  ), "\n")
  cat(paste(" Covariates:", stringr::str_c(x$covariates, collapse = ", "), "\n"))
  cat(paste(" Cutoff in quantile scale: ", signif(x$alpha, 4), "\n", sep = ""))
  cat(paste(" Number of contexts:", x$nb, "\n"))
  cat(paste(" Contexts without covariate:", x$no_cov, "\n"))
  cat(paste(" Maximum context length:", x$depth, "\n"))
  cat(paste(" Average context length:", signif(x$avg_depth, 4), "\n"))
  if (!is.nan(x$avg_hsize)) {
    cat(paste(" For nodes with covariates:", "\n"))
    cat(paste("  Maximum covariate history length:", x$max_hsize, "\n"))
    cat(paste("  Average covariate history length:", signif(x$avg_hsize, 4), "\n"))
    cat(paste("  Average difference between context length and history length:", signif(x$delta, 4), "\n"))
  }
  invisible(x)
}
