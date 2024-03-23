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
#' @returns An object of class `metrics.covlmc` with the following components:
#'
#'  - `accuracy`: the accuracy of the predictions
#'  - `conf_mat`: the confusion matrix of the predictions, with predicted values
#'    in rows and true values in columns
#'  - `auc`: the AUC of the predictive model
#'
#'  The object has a print method that recalls basic information about the model
#'  together with the values of the components above.
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' breaks <- c(
#'   0,
#'   median(powerconsumption$active_power, na.rm = TRUE),
#'   max(powerconsumption$active_power, na.rm = TRUE)
#' )
#' labels <- c(0, 1)
#' dts <- cut(pc$active_power, breaks = breaks, labels = labels)
#' dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
#' m_cov <- covlmc(dts, dts_cov, min_size = 5)
#' metrics(m_cov)
#'
#' @inheritSection predict.covlmc Extended contexts
#'
#' @exportS3Method
metrics.covlmc <- function(model, ...) {
  if (!is.null(model$trimmed)) {
    stop("metrics is not supported by trimmed covlmc")
  }
  all_preds <- contexts_extractor(model, TRUE, covlmc_predictive_extractor, list(), no_summary)
  if (length(all_preds$target) < model$data_size) {
    ## we have proper extended contexts, we need to get them
    ## we rely on predict for that
    nb <- model$data_size - length(all_preds$target)
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
    }
    local_preds <- data.frame(
      target = unmatched_data,
      predictions = extended_ctx_probs
    )
    all_preds <- rbind(all_preds, local_preds)
  }
  res <- main_metrics(all_preds$target, all_preds[, -1])
  rownames(res$conf_mat) <- model$vals
  colnames(res$conf_mat) <- model$vals
  res$model <- model
  structure(res, class = "metrics.covlmc")
}

#' @describeIn metrics.covlmc Prints the predictive metrics of the VLMC model with covariates.
#' @param x A metrics.covlmc object, results of a call to [metrics.covlmc()]
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
