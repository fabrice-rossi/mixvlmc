#' @inherit metrics
#' @title Predictive quality metrics for VLMC
#' @return An object of class `metrics.vlmc` with the following components:
#'
#'  - `accuracy`: the accuracy of the predictions
#'  - `conf_mat`: the confusion matrix of the predictions, with predicted values
#'    in rows and true values in columns
#'  - `auc`: the AUC of the predictive model
#'
#'  The object has a print method that recalls basic information about the model
#'  together with the values of the components above.
#' @exportS3Method
metrics.vlmc <- function(model, ...) {
  all_ctx <- contexts(model, frequency = "detailed", counts = "local")
  counts <- as.matrix(all_ctx[, -(1:2)])
  freq <- all_ctx$freq
  probs <- sweep(counts, 1, freq, "/")
  err <- freq - apply(counts, 1, max)
  total <- sum(freq)
  ## accuracy
  res <- list(accuracy = (total - sum(err)) / total)
  ## confusion matrix
  decision <- apply(counts, 1, which.max)
  if (is.factor(model$vals)) {
    the_levels <- levels(model$vals)
  } else {
    the_levels <- model$vals
  }
  cm <- matrix(0, ncol = ncol(counts), nrow = ncol(counts))
  for (k in 1:ncol(counts)) {
    cm[k, ] <- colSums(counts[decision == k, , drop = FALSE])
  }
  colnames(cm) <- the_levels
  rownames(cm) <- stringr::str_c("predicted", the_levels, sep = " ")
  res$conf_mat <- cm
  ## AUC
  # we generate fake responses that match the original data
  response <- rep(the_levels[1], sum(freq))
  pos <- 1
  for (k in 1:nrow(counts)) {
    for (l in 1:ncol(counts)) {
      if (counts[k, l] > 0) {
        new_pos <- pos + counts[k, l]
        response[pos:(new_pos - 1)] <- the_levels[l]
        pos <- new_pos
      }
    }
  }
  assertthat::assert_that(all(colSums(counts) == table(response)))
  if (ncol(counts) > 2) {
    ## multiclass case
    predictor <- matrix(0, ncol = ncol(probs), nrow = length(response))
    pos <- 1
    for (k in seq_along(freq)) {
      if (freq[k] > 0) {
        new_pos <- pos + freq[k]
        predictor[pos:(new_pos - 1), ] <- probs[k, ]
        pos <- new_pos
      }
    }
    colnames(predictor) <- the_levels
    res$roc <- pROC::multiclass.roc(response, predictor)
  } else {
    one_probs <- probs[freq > 0, 2]
    predictor <- rep(one_probs, times = freq[freq > 0])
    res$roc <- pROC::roc(response, predictor, levels = the_levels, direction = "<")
  }
  res$auc <- as.numeric(pROC::auc(res$roc))
  res$model <- model
  structure(res, class = "metrics.vlmc")
}

#' @describeIn metrics.vlmc Prints the predictive metrics of the VLMC model.
#' @param x A metrics.vlmc object, results of a call to [metrics.vlmc()]
#' @exportS3Method
print.metrics.vlmc <- function(x, ...) {
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
