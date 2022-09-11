vlmc_parent_summary <- function(ctx) {
  ctx$f_by / sum(ctx$f_by)
}

vlmc_context_extractor <-
  function(path, ct, vals, control, is_leaf, p_summary) {
    res <- frequency_context_extractor(path, ct, vals, control, is_leaf, p_summary)
    if (is.null(res)) {
      NULL
    } else {
      if (isTRUE(control$p_value)) {
        c_probs <- ct$f_by / sum(ct$f_by)
        local_kl <- kl_div(c_probs, p_summary) * sum(ct$f_by)
        res <- cbind(res, data.frame(cutoff = local_kl))
      }
      if (isTRUE(control$metric)) {
        if (isFALSE(control[["frequency"]] == "detailed") || isFALSE(control[["counts"]] == "local")) {
          l_cont <- control
          l_cont$frequency <- "detailed"
          l_cont$counts <- "local"
          lres <- frequency_context_extractor(path, ct, vals, l_cont, is_leaf, p_summary)
        } else {
          lres <- res
        }
        fake_data <- generate_fake_data(lres$freq, lres[, -(1:2), drop = FALSE], vals)
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
#'   them in the native scale.
#' @param counts specifies how the counts reported by `frequency` are computed.
#'   The default value `"desc"` includes both counts that are specific to the
#'   context (if any) and counts from the descendants of the context in the
#'   tree. When `counts = "local"` the counts include only the number of times
#'   the context appears without being the last part of a longer context.
#' @param metrics if TRUE, adds predictive metrics for each context (see [metrics()]
#'   for the definition of predictive metrics).
#' @details The default result for `type="auto"` (or `type="list"`),
#'   `frequency=NULL`, `cutoff=NULL` and `metrics=FALSE` is the list of all contexts.
#'
#'   Other results are obtained only with `type="auto"` or `type="data.frame"`.
#'   See [contexts.ctx_tree()] for details about the `frequency` parameter. When
#'   `cutoff` is non `NULL`, the resulting `data.frame` contains a `cutoff`
#'   column with the cut off values, either in quantile or in native scale. See
#'   [cutoff()] and [prune()] for the definitions of cut off values and of the
#'   two scales.
#' @section Cut off values: The cut off values reported by `contexts.vlmc` can
#'   be different from the ones reported by [cutoff()] for two reasons:
#'
#'   1. [cutoff()] reports only useful cut off values, i.e., cut off values that
#'   should induce a simplification of the VLMC when used in [prune()]. This
#'   exclude cut off values associated to simple contexts that are smaller than
#'   the ones of their descendants in the context tree. Those values are
#'   reported by `context.vlmc`.
#'
#'   2. `context.vlmc` reports only cut off values of actual contexts, while
#'   [cutoff()] reports cut off values for all nodes of the context tree.
#'
#' @examples
#' dts <- sample(as.factor(c("A", "B", "C")), 100, replace = TRUE)
#' model <- vlmc(dts, alpha = 0.5)
#' contexts(model)
#' contexts(model, frequency = "total")
#' contexts(model, cutoff = "quantile")
#' @export
contexts.vlmc <- function(ct, type = c("auto", "list", "data.frame"), reverse = TRUE, frequency = NULL,
                          counts = c("desc", "local"), cutoff = NULL, metrics = FALSE, ...) {
  type <- match.arg(type)
  counts <- match.arg(counts)
  if (is.null(cutoff) && counts == "desc" && !metrics) {
    NextMethod()
  } else {
    assertthat::assert_that(type %in% c("auto", "data.frame"))
    if (!is.null(frequency)) {
      assertthat::assert_that(frequency %in% c("total", "detailed"))
    }
    if (!is.null(cutoff)) {
      assertthat::assert_that(cutoff %in% c("quantile", "native"))
    }
    control <- list(frequency = frequency, counts = counts, p_value = !is.null(cutoff), metrics = metrics)
    preres <- contexts_extractor(ct, reverse, vlmc_context_extractor, control, vlmc_parent_summary)
    if (!is.null(cutoff)) {
      if ((cutoff == "quantile")) {
        preres$cutoff <- before(stats::pchisq(2 * preres$cutoff, df = length(ct$vals) - 1, lower.tail = FALSE))
      } else {
        preres$cutoff <- before(preres$cutoff)
      }
    }
    preres
  }
}
