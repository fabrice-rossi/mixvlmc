#' Predictive quality metrics for context based models
#'
#' This function computes and returns predictive quality metrics for context
#' based models such as VLMC and VLMC with covariates.
#'
#' A context based model computes transition probabilities for its contexts.
#' Using a maximum transition probability decision rule, this can be used to
#' predict the new state that is the more likely to follow the current one,
#' given the context (see [predict.vlmc()]). The quality of these predictions is
#' evaluated using standard metrics including:
#'
#' - accuracy
#' - the full confusion matrix
#' - the area under the roc curve (AUC), considering the context based model as
#' a (conditional) probability estimator. We use Hand and Till (2001) multiclass
#' AUC in case of a state space with more than 2 states
#'
#' @param model The context based model on which to compute predictive metrics.
#' @param ... Additional parameters for predictive metrics computation.
#'
#' @returns The returned value is guaranteed to have at least three components
#'
#'  - `accuracy`: the accuracy of the predictions
#'  - `conf_mat`: the confusion matrix of the predictions, with predicted values
#'   in rows and true values in columns
#'  - `auc`: the AUC of the predictive model
#'
#' @seealso [metrics.vlmc()], [metrics.ctx_node()], [contexts.vlmc()], [predict.vlmc()].
#' @references David J. Hand and Robert J. Till (2001). "A Simple Generalisation
#' of the Area Under the ROC Curve for Multiple Class Classification Problems."
#' _Machine Learning_ 45(2), p. 171--186. DOI: \doi{10.1023/A:1010920819831}.
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' breaks <- c(
#'   0,
#'   median(powerconsumption$active_power, na.rm = TRUE),
#'   max(powerconsumption$active_power, na.rm = TRUE)
#' )
#' labels <- c(0, 1)
#' rdts <- cut(pc$active_power, breaks = breaks, labels = labels)
#' model <- vlmc(rdts)
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
#' When `roc` is `FALSE`, the AUC is computed directly from the ranks of the
#' predicted probabilities (Mann-Whitney statistic), without building the ROC
#' curves. This is much faster and gives the same value as [pROC::auc()].
#'
#' @param target A vector of true classes.
#' @param probs A matrix/vector providing estimates of the probabilities of each class.
#' @param roc If `TRUE`, the ROC curve(s) are computed with `pROC` and returned
#'   in the `roc` component of the result.
#'
#' @returns A list with several quality metrics.
#' @noRd
main_metrics <- function(target, probs, roc = TRUE) {
  with_roc <- roc
  if (is.matrix(probs) || is.data.frame(probs)) {
    if (ncol(probs) > 1) {
      decision <- apply(probs, 1, which.max)
      decision <- factor(levels(target)[decision], levels = levels(target))
    } else {
      decision <- factor(as.integer(probs[, 1] > 0.5), levels = c(0, 1))
    }
  } else {
    decision <- factor(as.integer(probs > 0.5), levels = c(0, 1))
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
    auc <- as.numeric(NA)
    roc <- NULL
  } else if (!with_roc) {
    roc <- NULL
    if (ncol(cm) > 2) {
      auc <- auc_hand_till(target, as.matrix(probs))
    } else {
      if (!is.null(dim(probs))) {
        probs <- probs[, 1]
      }
      if (is.factor(target)) {
        positive <- target == levels(target)[2]
      } else {
        positive <- target == 1
      }
      auc <- auc_from_ranks(probs[positive], probs[!positive])
    }
  } else {
    if (ncol(cm) > 2) {
      probs <- as.matrix(probs)
      colnames(probs) <- levels(target)
      roc <- pROC::multiclass.roc(target, probs)
    } else {
      if (is.factor(target)) {
        roc <- pROC::roc(target, probs, levels = levels(target), direction = "<")
      } else {
        roc <- pROC::roc(target, probs, levels = c(0, 1), direction = "<")
      }
    }
    auc <- as.numeric(pROC::auc(roc))
  }
  if (with_roc) {
    c(metrics_from_cm(cm), list(conf_mat = cm, roc = roc, auc = auc))
  } else {
    c(metrics_from_cm(cm), list(conf_mat = cm, auc = auc))
  }
}

## probability that a score of the positive class is larger than a score of the
## negative class, counting ties for 1/2 (AUC, Mann-Whitney statistic)
auc_from_ranks <- function(pos, neg) {
  n_pos <- length(pos)
  n_neg <- length(neg)
  ranks <- rank(c(pos, neg))
  (sum(ranks[seq_len(n_pos)]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

## Hand and Till (2001) multiclass AUC, as computed by pROC::multiclass.roc()
## with a matrix of probabilities
auc_hand_till <- function(target, probs) {
  lev <- levels(target)
  pairs <- utils::combn(seq_along(lev), 2)
  pair_auc <- apply(pairs, 2, function(ij) {
    i <- ij[1]
    j <- ij[2]
    in_i <- target == lev[i]
    in_j <- target == lev[j]
    a_ij <- auc_from_ranks(probs[in_i, i], probs[in_j, i])
    a_ji <- auc_from_ranks(probs[in_j, j], probs[in_i, j])
    (a_ij + a_ji) / 2
  })
  mean(pair_auc)
}
