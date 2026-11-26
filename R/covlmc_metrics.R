covlmc_predictive_extractor <- function(tree, path, ct, vals, control, is_leaf, p_summary) {
  if (is.null(ct[["model"]])) {
    if (!is.null(ct[["merged_model"]])) {
      data.frame(
        target = ct$merged_model$data$target,
        predictions = glm_predict(ct$merged_model$model, lev = vals)
      )
    } else {
      NULL
    }
  } else {
    data.frame(
      target = ct$model$data$target,
      predictions = glm_predict(ct$model$model, lev = vals)
    )
  }
}


#' @inherit metrics
#' @title Predictive quality metrics for VLMC with covariates
#' @param model a fitted VLMC with covariates (a `covlmc` object).
#' @section Trimmed models:
#'
#'   The predictions of a trimmed model (see [trim.covlmc()]) on the time
#'   series used to fit it are not available any more. The accuracy and the
#'   confusion matrix are obtained from the confusion matrices of the contexts
#'   computed when the model was fitted (and from the one of the first
#'   observations computed when the model was trimmed), but the AUC cannot be
#'   computed: it is `NA`, with a warning.
#' @returns An object of class `metrics.covlmc` with the following components:
#'
#'  - `accuracy`: the accuracy of the predictions
#'  - `conf_mat`: the confusion matrix of the predictions, with predicted values
#'    in rows and true values in columns
#'  - `auc`: the AUC of the predictive model
#'
#'  The object has a print method that recalls basic information about the
#'  model together with the values of the components above.
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' breaks <- c(
#'   0,
#'   median(powerconsumption$active_power, na.rm = TRUE),
#'   max(powerconsumption$active_power, na.rm = TRUE)
#' )
#' labels <- c(0, 1)
#' rdts <- cut(pc$active_power, breaks = breaks, labels = labels)
#' rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
#' m_cov <- covlmc(rdts, rdts_cov, min_size = 5)
#' metrics(m_cov)
#'
#' @inheritSection predict.covlmc Extended contexts
#'
#' @exportS3Method
metrics.covlmc <- function(model, ...) {
  rlang::check_dots_empty()
  if (!is.null(model$trimmed)) {
    return(metrics_trimmed_covlmc(model))
  }
  all_preds <- contexts_extractor(model, TRUE, covlmc_predictive_extractor, list(), no_summary)
  if (length(all_preds$target) < model$data_size) {
    ## we have proper extended contexts, we need to get them
    nb <- model$data_size - length(all_preds$target)
    all_preds <- rbind(all_preds, covlmc_extended_predictions(model, nb))
  }
  res <- main_metrics(all_preds$target, all_preds[, -1])
  rownames(res$conf_mat) <- model$vals
  colnames(res$conf_mat) <- model$vals
  res$model <- model
  structure(res, class = "metrics.covlmc")
}

## predictions of the first nb observations of the time series used to fit
## the model, which are predicted with extended contexts (we rely on predict
## for that)
covlmc_extended_predictions <- function(model, nb) {
  unmatched_data <- model$vals[model$iix[1:nb] + 1]
  unmatched_cov <- model$icov[1:nb, , drop = FALSE]
  extended_ctx_probs <- predict(model,
    unmatched_data,
    unmatched_cov,
    final_pred = FALSE,
    type = "probs"
  )
  if (ncol(extended_ctx_probs) == 2) {
    extended_ctx_probs <- extended_ctx_probs[, 2, drop = FALSE]
    colnames(extended_ctx_probs) <- c("predictions")
    unmatched_data <- model$iix[1:nb]
  } else {
    ## as the targets of the models of the contexts
    unmatched_data <- factor(as.character(unmatched_data),
      levels = as.character(model$vals)
    )
  }
  data.frame(
    target = unmatched_data,
    predictions = extended_ctx_probs
  )
}

## sum of the confusion matrices of the models of the contexts, computed when
## the models are fitted (they are kept by trimming)
rec_covlmc_conf_mat <- function(ct) {
  res <- 0
  for (f in c("model", "merged_model")) {
    if (!is.null(ct[[f]])) {
      res <- res + unclass(ct[[f]]$metrics$conf_mat)
    }
  }
  if (!is.null(ct$children)) {
    for (child in ct$children) {
      if (length(child) > 0) {
        res <- res + rec_covlmc_conf_mat(child)
      }
    }
  }
  res
}

## confusion matrix of the predictions of the first observations (with
## extended contexts), computed before trimming a model (NULL if all the
## observations are predicted by the models of the contexts)
covlmc_extended_conf_mat <- function(model) {
  nb <- model$data_size - sum(rec_covlmc_conf_mat(model))
  if (nb > 0) {
    preds <- covlmc_extended_predictions(model, nb)
    unclass(main_metrics(preds$target, preds[, -1], roc = FALSE)$conf_mat)
  } else {
    NULL
  }
}

## metrics of a trimmed model: the predictions are not available any more,
## but the confusion matrices are
metrics_trimmed_covlmc <- function(model) {
  conf_mat <- rec_covlmc_conf_mat(model)
  if (!is.null(model[["extended_conf_mat"]])) {
    conf_mat <- conf_mat + model[["extended_conf_mat"]]
  }
  dimnames(conf_mat) <- list(
    "predicted value" = model$vals,
    "true value" = model$vals
  )
  conf_mat <- as.table(conf_mat)
  warning("the AUC cannot be computed for a trimmed covlmc")
  res <- list(
    accuracy = sum(diag(conf_mat)) / sum(conf_mat),
    conf_mat = conf_mat,
    auc = NA_real_,
    model = model
  )
  structure(res, class = "metrics.covlmc")
}

#' @describeIn metrics.covlmc Prints the predictive metrics of the VLMC with
#'   covariates.
#' @param x a `metrics.covlmc` object, result of a call to [metrics.covlmc()].
#' @exportS3Method
print.metrics.covlmc <- function(x, ...) {
  print(x$model)
  cat(paste(" Confusion matrix:", "\n"))
  pcm <- pp_mat(x$conf_mat, colnames = colnames(x$conf_mat))
  rn <- rownames(x$conf_mat)
  l_rn <- max(cli::utf8_nchar(rn, "width"))
  rn <- utf8_pad(c("", rn), l_rn, "right")
  for (k in seq_along(rn)) {
    cat(paste("  ", rn[k], " ", pcm[k], sep = ""), "\n")
  }
  cat(paste(" Accuracy:", signif(x$accuracy, 4), "\n"))
  cat(paste(" AUC:", signif(x$auc, 4), "\n"))
  invisible(x)
}
