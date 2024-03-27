vlmc_parent_summary <- function(ctx) {
  ctx$f_by / sum(ctx$f_by)
}

vlmc_context_extractor <-
  function(tree, path, ct, vals, control, is_leaf, p_summary) {
    res <- frequency_context_extractor(tree, path, ct, vals, control, is_leaf, p_summary)
    if (is.null(res)) {
      NULL
    } else {
      if (isTRUE(control$p_value)) {
        c_probs <- ct$f_by / sum(ct$f_by)
        local_kl <- kl_div(c_probs, p_summary) * sum(ct$f_by)
        res <- cbind(res, data.frame(cutoff = local_kl))
      }
      if (isTRUE(control$metric)) {
        if (isFALSE(control[["frequency"]] == "detailed") || isFALSE(control[["local"]])) {
          l_cont <- control
          l_cont$frequency <- "detailed"
          l_cont$local <- TRUE
          lres <- frequency_context_extractor(tree, path, ct, vals, l_cont, is_leaf, p_summary)
        } else {
          lres <- res
        }
        probs <- matrix(ct$f_by / sum(ct$f_by), nrow = 1)
        fake_data <- generate_fake_data(lres$freq, lres[, 3:(2 + length(vals)), drop = FALSE], probs, vals)
        local_m <- main_metrics(fake_data$response, fake_data$predictor)
        local_m$roc <- NULL
        local_m$conf_mat <- NULL
        res <- cbind(res, as.data.frame(local_m))
      }
      res
    }
  }

#' Contexts of a VLMC
#'
#' This function extracts all the contexts from a fitted VLMC, possibly with
#' some associated data.
#'
#' @inherit contexts.ctx_tree
#' @param cutoff specifies whether to include the cut off value associated to
#'   each context (see [cutoff()] and [prune()]). The default result with
#'   `cutoff=NULL` does not include those values. Setting `cutoff` to `quantile`
#'   adds the cut off values in quantile scale, while `cutoff="native"` adds
#'   them in the native scale. The returned values are directly based on the log
#'   likelihood ratio computed in the context tree and are not modified to
#'   ensure pruning (as when [cutoff()] is called by  `raw=TRUE`).
#' @param local specifies how the counts reported by `frequency` are computed.
#'   When `local` is `FALSE` (default value) the counts include both counts that
#'   are specific to the context (if any) and counts from the descendants of the
#'   context in the tree. When `local` is `TRUE` the counts include only the
#'   number of times the context appears without being the last part of a longer
#'   context.
#' @param metrics if TRUE, adds predictive metrics for each context (see
#'   [metrics()] for the definition of predictive metrics).
#' @details The default behaviour of the function is to return a list of all the
#'   contexts using `ctx_node` objects (as returned by [find_sequence()]). The
#'   properties of the contexts can then be explored using adapted functions
#'   such as [counts()], [cutoff.ctx_node()], [metrics.ctx_node()] and
#'   [positions()].
#'
#'   When `sequence=TRUE` the method returns a data.frame whose first column,
#'   named `context`, contains the contexts as vectors (i.e. the value returned
#'   by `as_sequence()` applied to a `ctx_node` object). Other columns contain
#'   context specific values specified by the additional parameters. Setting any
#'   of those parameters to a value that ask for reporting information will
#'   toggle the result type of the function to `data.frame`.
#'
#'   The `frequency` parameter is described in details in the documentation of
#'   [contexts.ctx_tree()]. When `cutoff` is non `NULL`, the resulting
#'   `data.frame` contains a `cutoff` column with the cut off values, either in
#'   quantile or in native scale. See [cutoff.vlmc()] and [prune.vlmc()] for the
#'   definitions of cut off values and of the two scales.
#' @section Cut off values: The cut off values reported by `contexts.vlmc` can
#'   be different from the ones reported by [cutoff.vlmc()] for three reasons:
#'
#'   1. [cutoff.vlmc()] reports only useful cut off values, i.e., cut off values
#'   that should induce a simplification of the VLMC when used in [prune()].
#'   This exclude cut off values associated to simple contexts that are smaller
#'   than the ones of their descendants in the context tree. Those values are
#'   reported by `context.vlmc`.
#'
#'   2. `context.vlmc` reports only cut off values of actual contexts, while
#'   [cutoff.vlmc()] reports cut off values for all nodes of the context tree.
#'
#'   3. values are not modified to induce pruning, contrarily to the default
#'   behaviour of [cutoff.vlmc()]
#'
#' @examples
#' rdts <- sample(as.factor(c("A", "B", "C")), 100, replace = TRUE)
#' model <- vlmc(rdts, alpha = 0.5)
#' ## direct representation with ctx_node objects
#' model_ctxs <- contexts(model)
#' model_ctxs
#' sapply(model_ctxs, cutoff, scale = "quantile")
#' sapply(model_ctxs, cutoff, scale = "native")
#' sapply(model_ctxs, function(x) metrics(x)$accuracy)
#' ## data.frame format
#' contexts(model, frequency = "total")
#' contexts(model, cutoff = "quantile")
#' contexts(model, cutoff = "native", metrics = TRUE)
#' @export
contexts.vlmc <- function(ct, sequence = FALSE, reverse = FALSE, frequency = NULL,
                          positions = FALSE, local = FALSE, cutoff = NULL,
                          metrics = FALSE, ...) {
  assertthat::assert_that(rlang::is_logical(sequence))
  assertthat::assert_that(rlang::is_logical(reverse))
  assertthat::assert_that(rlang::is_logical(local))
  assertthat::assert_that(rlang::is_logical(metrics))
  if (!is.null(frequency)) {
    assertthat::assert_that(frequency %in% c("total", "detailed"))
  }
  if (!is.null(cutoff)) {
    assertthat::assert_that(cutoff %in% c("quantile", "native"))
  }
  wants_df <- !is.null(frequency) || positions || !is.null(cutoff) || metrics
  if (missing(sequence)) {
    sequence <- wants_df
  } else {
    if (!sequence && wants_df) {
      stop("sequence = 'FALSE' is incompatible with the other requested values")
    }
  }
  if (!sequence) {
    NextMethod()
  } else {
    control <- list(
      frequency = frequency, local = local, p_value = !is.null(cutoff),
      metrics = metrics, positions = positions
    )
    preres <- contexts_extractor(
      ct, reverse, vlmc_context_extractor, control,
      vlmc_parent_summary
    )
    if (!is.null(cutoff)) {
      if ((cutoff == "quantile")) {
        preres$cutoff <- to_quantile(preres$cutoff, length(ct$vals))
      }
    }
    if (positions) {
      preres$positions <- I(preres$positions)
    }
    preres
  }
}
