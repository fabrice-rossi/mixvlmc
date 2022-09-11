covlmc_predictive_extractor <- function(path, ct, vals, control, is_leaf, p_summary) {
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
#' @return An object of class `metrics.covlmc` with the following components:
#'
#'  - `accuracy`: the accuracy of the predictions
#'  - `conf_mat`: the confusion matrix of the predictions, with predicted values
#'    in rows and true values in columns
#'  - `auc`: the AUC of the predictive model
#'
#'  The object has a print method that recalls basic information about the model
#'  together with the values of the components above.
#' @exportS3Method
metrics.covlmc <- function(model, ...) {
  all_preds <- contexts_extractor(model, TRUE, covlmc_predictive_extractor, list(), no_summary)
  res <- main_metrics(all_preds$target, all_preds[, -1])
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
  l_rn <- max(stringr::str_length(rn))
  rn <- stringr::str_pad(c("", rn), l_rn, side = "right")
  for (k in seq_along(rn)) {
    cat(paste("  ", rn[k], " ", pcm[k], sep = ""), "\n")
  }
  cat(paste(" Accurary:", signif(x$accuracy, 4), "\n"))
  cat(paste(" AUC:", signif(x$auc, 4), "\n"))
  invisible(x)
}
