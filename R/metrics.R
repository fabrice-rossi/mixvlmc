#' Predictive quality metrics for context based models
#'
#' This function computes and returns predictive quality metrics for VLMC and
#' VLMC with covariates.
#'
#' A context based model computes transition probabilities for its contexts.
#' Using a maximum transition probability decision rule, this can be used to
#' "predict" the new state that is the more likely to follow the current one,
#' given the context. The quality of these predictions is evaluated using
#' standard metrics including:
#'
#' - accuracy
#' - the full confusion matrix
#' - the area under the roc curve (AUC), considering the context based model as
#'   a (conditional) probability estimator. We use Hand and Till (2001) multiclass
#'   AUC in case of a state space with more than 2 states
#'
#' @param model The context based model on which to compute predictive metrics.
#' @param ... Additional parameters for predictive metrics computation.
#'
#' @returns The returned value is guaranteed to have at least three components
#'
#'  - `accuracy`: the accuracy of the predictions
#'  - `conf_mat`: the confusion matrix of the predictions, with predicted values
#'    in rows and true values in columns
#'  - `auc`: the AUC of the predictive model
#'
#' @seealso [metrics.vlmc()], [contexts.covlmc()]
#' @references
#' David J. Hand and Robert J. Till (2001). "A Simple Generalisation of the Area
#' Under the ROC Curve for Multiple Class Classification
#' Problems." _Machine Learning_ 45(2), p. 171--186. DOI:
#'  \doi{10.1023/A:1010920819831}.
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' breaks <- c(
#'   0,
#'   median(powerconsumption$active_power, na.rm = TRUE),
#'   max(powerconsumption$active_power, na.rm = TRUE)
#' )
#' labels <- c(0, 1)
#' dts <- cut(pc$active_power, breaks = breaks, labels = labels)
#' model <- vlmc(dts)
#' metrics(model)
#'
#' @export
metrics <- function(model, ...) {
  UseMethod("metrics")
}

metrics_from_cm <- function(cm) {
  list(accuracy = sum(diag(cm)) / sum(cm))
}

metrics_fix_names <- function(cm) {
  cm_dm <- dimnames(cm)
  names(cm_dm) <- c("predicted value", "true value")
  dimnames(cm) <- cm_dm
  cm
}

#' Predictive metrics calculation
#'
#' This function computes classical quality metrics for a predictive model in a
#' classification setting.
#'
#' The `target` parameter gives the true label, while `probs` provides predicted
#' probabilities. In the case of binary classification, `target` should take 0
#' and 1 values, and `probs` is an estimation of the probablity of 1. In the
#' general case `probs` must be a matrix with as many columns as there are
#' classes.
#'
#' In all cases, the predicted class is the one with the maximal posterior
#' probability.
#'
#' @param target A vector of true classes.
#' @param probs A matrix/vector providing estimates of the probabilities of each class.
#'
#' @returns A list with several quality metrics.
#' @noRd
main_metrics <- function(target, probs) {
  if (is.matrix(probs) || is.data.frame(probs)) {
    if (ncol(probs) > 1) {
      decision <- apply(probs, 1, which.max)
      decision <- factor(levels(target)[decision], levels = levels(target))
    } else {
      decision <- factor(as.integer(probs[, 1] >= 0.5), levels = c(0, 1))
    }
  } else {
    decision <- factor(as.integer(probs >= 0.5), levels = c(0, 1))
  }
  if (!is.factor(target)) {
    f_target <- factor(target, levels = c(0, 1))
  } else {
    f_target <- target
  }
  cm <- metrics_fix_names(table(decision, f_target))
  t_dist <- colSums(cm)
  if (is.factor(target)) {
    nb_levels <- length(levels(target))
  } else {
    nb_levels <- 2
  }
  if (is.null(ncol(probs))) {
    degenerate <- nb_levels > 2
  } else {
    degenerate <- (nb_levels > ncol(probs))
  }
  degenerate <- degenerate || any(t_dist == 0)
  if (degenerate) {
    ## degenerate case
    auc <- NA
    roc <- NULL
  } else {
    if (ncol(cm) > 2) {
      probs <- as.matrix(probs)
      colnames(probs) <- levels(target)
      roc <- pROC::multiclass.roc(target, probs)
    } else {
      roc <- pROC::roc(target, probs, levels = c(0, 1), direction = "<")
    }
    auc <- as.numeric(pROC::auc(roc))
  }
  c(metrics_from_cm(cm), list(conf_mat = cm, roc = roc, auc = auc))
}
