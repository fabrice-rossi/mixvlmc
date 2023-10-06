#' Cut off value for pruning a node in the context tree of a VLMC
#'
#' This function returns the cut off value associated to a specific node in the
#' context tree interpreted as a VLMC. The node is represented by a `ctx_node`
#' object as returned by [find_sequence()] or [contexts()]. For details, see
#' [cutoff.vlmc()].
#'
#' @param model a `ctx_node` object as returned by [find_sequence()]
#' @param scale specify whether the results should be "native" log likelihood
#'   ratio values or expressed in a "quantile" scale of a chi-squared
#'   distribution (defaults to "quantile").
#' @param raw specify whether the returned values should be limit values
#'   computed in the model or modified values that guarantee pruning (see
#'   details in [cutoff.vlmc()])
#' @param ... additional arguments for the `cutoff` function.
#' @returns a cut off value
#' @export
#' @seealso [cutoff()]
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
#' model <- vlmc(dts)
#' model_ctxs <- contexts(model)
#' cutoff(model_ctxs[[1]])
#' cutoff(model_ctxs[[2]], scale = "native", raw = TRUE)
cutoff.ctx_node <- function(model, scale = c("quantile", "native"), raw = FALSE,
                            ...) {
  scale <- match.arg(scale)
  cparent <- parent(model)
  if (is.null(cparent)) {
    if (scale == "quantile") {
      1
    } else {
      0
    }
  } else {
    p_probs <- probs(cparent)
    c_probs <- probs(model)
    local_kl <- kl_div(c_probs, p_probs) * counts(model, frequency = "total")
    guaranteed_pruning(local_kl, length(model$tree$vals), scale, raw)
  }
}

#' Predictive quality metrics for a node of a context tree
#'
#' This function computes and returns predictive quality metrics for a node
#' (`ctx_node`) extracted from a context tree.
#'
#' Compared to [metrics.vlmc()], this function focuses on a single context and
#' assesses the quality of its predictions, disregarding observations that have
#' other contexts. Apart from this limited scope, the function operates as
#' [metrics.vlmc()].
#'
#' @param model T `ctx_node` object as returned by [find_sequence()].
#' @inherit metrics
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
#' model <- vlmc(dts)
#' model_ctxs <- contexts(model)
#' metrics(model_ctxs[[4]])
#' @export
metrics.ctx_node <- function(model, ...) {
  c_probs <- matrix(probs(model), nrow = 1)
  c_freq <- counts(model, counts = "local")
  fake_data <- generate_fake_data(
    c_freq$total,
    c_freq[, 2:ncol(c_freq), drop = FALSE],
    c_probs,
    model$tree$vals
  )
  res <- main_metrics(fake_data$response, fake_data$predictor)
  rownames(res$conf_mat) <- model$tree$vals
  colnames(res$conf_mat) <- model$tree$vals
  res$fake_data <- fake_data
  res$model <- model
  structure(res, class = "metrics.vlmc")
}
