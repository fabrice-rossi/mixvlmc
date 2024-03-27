trim_one_model <- function(model, keep_model, vals) {
  model$data <- NULL
  model$metrics$roc <- NULL
  ml <- glm_levels(model$model, vals)
  if (length(ml) < length(vals)) {
    model_levels <- ml
  } else {
    model_levels <- NULL
  }
  model_probs <- NULL
  if (keep_model) {
    model$model <- glm_trim(model$model)
  } else {
    if (glm_is_constant(model$model)) {
      model_probs <- as.numeric(glm_to_probs(model$model, vals))
    }
    model$model <- NULL
  }
  if (is.null(model_probs)) {
    list(model = model, model_levels = model_levels)
  } else {
    list(model = model, model_levels = model_levels, model_probs = model_probs)
  }
}

rec_trim_covlmc <- function(ct, keep_model, vals) {
  ct$match <- NULL
  ct$cache <- NULL
  if (!is.null(ct$model)) {
    tr_model <- trim_one_model(ct$model, keep_model, vals)
    ct$model <- tr_model$model
    ct$model_levels <- tr_model[["model_levels"]]
    ct$model_probs <- tr_model[["model_probs"]]
  }
  if (!is.null(ct$merged_model)) {
    tr_model <- trim_one_model(ct$merged_model, keep_model, vals)
    ct$merged_model <- tr_model$model
    ct$merged_model$model_levels <- tr_model$model_levels
    ct$merged_model$model_probs <- tr_model$model_probs
  }
  if (!is.null(ct$extended_model)) {
    tr_model <- trim_one_model(ct$extended_model, keep_model, vals)
    ct$extended_model <- tr_model$model
    ct$extended_model$model_levels <- tr_model$model_levels
    ct$extended_model$model_probs <- tr_model$model_probs
  }
  if (!is.null(ct$children)) {
    for (k in seq_along(ct$children)) {
      if (length(ct$children[[k]]) > 0) {
        ct$children[[k]] <- rec_trim_covlmc(ct$children[[k]], keep_model, vals)
      }
    }
  }
  ct
}

#' Trim a COVLMC
#'
#' This function returns a trimmed COVLMC from which cached data have been removed.
#'
#' Called with `keep_model` set to `FALSE` (default case), the trimming is maximal and reduces
#' further usability of the model. In particular [loglikelihood.covlmc()] cannot be used
#' for new data, [contexts.covlmc()] do not support model extraction, and
#' [simulate.covlmc()], [metrics.covlmc()]  and [prune.covlmc()] cannot be used at all.
#'
#' Called with `keep_model` set to `TRUE`, the trimming process is less complete. In
#' particular internal models are simplified using [butcher::butcher()] and some
#' additional minor reductions. This saves less memory but enables the use of
#' [loglikelihood.covlmc()] for new data as
#' well as the use of [simulate.covlmc()].
#'
#' @param ct a context tree.
#' @param keep_model specifies whether to keep the internal models (or not)
#' @param ... additional arguments for the trim function.
#'
#' @returns a trimmed context tree.
#' @export
#' @seealso [tune_covlmc()]
#'
#' @examples
#' pc <- powerconsumption[powerconsumption$week %in% 5:7, ]
#' rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
#' rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
#' m_cov <- covlmc(rdts, rdts_cov, min_size = 10, keep_data = TRUE)
#' print(object.size(m_cov), units = "Mb")
#' t_m_cov_model <- trim(m_cov, keep_model = TRUE)
#' print(object.size(t_m_cov_model), units = "Mb")
#' t_m_cov <- trim(m_cov)
#' print(object.size(t_m_cov), units = "Mb")
trim.covlmc <- function(ct, keep_model = FALSE, ...) {
  pre_res <- rec_trim_covlmc(ct, keep_model, ct$vals)
  pre_res$x <- NULL
  pre_res$covariate <- NULL
  pre_res$trimmed <- ifelse(keep_model, "non_model", "full")
  pre_res
}
