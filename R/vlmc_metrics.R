generate_fake_data <- function(freq, counts, vals) {
  counts <- as.matrix(counts)
  probs <- sweep(counts, 1, freq, "/")
  if (is.factor(vals)) {
    the_levels <- levels(vals)
  } else {
    the_levels <- vals
  }
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
  response <- factor(response, levels = the_levels)
  assertthat::assert_that(all(colSums(counts) == table(response)))
  if (ncol(counts) > 2) {
    ## multiclass case
    predictor <- matrix(0, ncol = ncol(probs), nrow = length(response))
    pos <- 1
    for (k in seq_along(freq)) {
      if (freq[k] > 0) {
        new_pos <- pos + freq[k]
        predictor[pos:(new_pos - 1), ] <- matrix(probs[k, ], ncol = ncol(predictor), nrow = freq[k], byrow = TRUE)
        pos <- new_pos
      }
    }
    colnames(predictor) <- the_levels
  } else {
    one_probs <- probs[freq > 0, 2]
    predictor <- rep(one_probs, times = freq[freq > 0])
  }
  list(response = response, predictor = predictor)
}


#' @inherit metrics
#' @title Predictive quality metrics for VLMC
#' @returns An object of class `metrics.vlmc` with the following components:
#'
#'  - `accuracy`: the accuracy of the predictions
#'  - `conf_mat`: the confusion matrix of the predictions, with predicted values
#'   in rows and true values in columns
#'  - `auc`: the AUC of the predictive model
#'
#'   The object has a print method that recalls basic information about the
#'   model together with the values of the components above.
#'
#' @section Extended contexts:
#'
#'   As explained in details in [loglikelihood.vlmc()] documentation and in the
#'   dedicated `vignette("likelihood", package = "mixvlmc")`, the first initial
#'   values of a time series do not in general have a proper context for a VLMC
#'   with a non zero order. In order to predict something meaningful for those
#'   values, we rely on the notion of extended context defined in the documents
#'   mentioned above. Metrics are computed using this extended context approach.
#'   This follows the same logic as using [loglikelihood.vlmc()] with the
#'   parameter `initial="extended"`. [simulate.vlmc()] and [predict.vlmc()] use
#'   the same approach.
#'
#' @exportS3Method
metrics.vlmc <- function(model, ...) {
  all_ctx <- contexts(model, frequency = "detailed", counts = "local")
  counts <- as.matrix(all_ctx[, -(1:2)])
  fake_data <- generate_fake_data(all_ctx$freq, counts, model$vals)
  res <- main_metrics(fake_data$response, fake_data$predictor)
  rownames(res$conf_mat) <- model$vals
  colnames(res$conf_mat) <- model$vals
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
