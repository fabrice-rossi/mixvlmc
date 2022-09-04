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
#' @return The returned value is guaranteed to have at least three components
#'
#'  - `accuracy`: the accuracy of the predictions
#'  - `conf_mat`: the confusion matrix of the predictions, with predicted values
#'    in rows and true values in columns
#'  - `auc`: the AUC of the predictive model
#'
#' @seealso [metrics.vlmc()], [contexts.covlmc()]
#' @references
#' David J. Hand and Robert J. Till (2001). A Simple Generalisation of the Area
#' Under the ROC Curve for Multiple Class Classification
#' Problems. _Machine Learning_ 45(2), p. 171--186. DOI:
#'  [10.1023/A:1010920819831](http://dx.doi.org/10.1023/A:1010920819831).
#' @export
metrics <- function(model, ...) {
  UseMethod("metrics")
}

metrics_from_cm <- function(cm) {
  list(accuracy = sum(diag(cm)) / sum(cm))
}
