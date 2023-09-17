## builds a fake data set from the counts associated to contexts in order
## to compute prediction metrics without making actual prediction (for the
## learning data).
generate_fake_data <- function(freq, counts, probs, vals) {
  counts <- as.matrix(counts)
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
        predictor[pos:(new_pos - 1), ] <- matrix(as.numeric(probs[k, ]), ncol = ncol(predictor), nrow = freq[k], byrow = TRUE)
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
#' @inheritSection predict.vlmc Extended contexts
#'
#' @exportS3Method
metrics.vlmc <- function(model, ...) {
  all_ctx <- contexts(model, frequency = "detailed", counts = "local")
  all_ctx_global <- contexts(model, frequency = "detailed")
  probs <- sweep(as.matrix(all_ctx_global[, -(1:2)]), 1, all_ctx_global$freq, "/")
  counts <- as.matrix(all_ctx[, -(1:2)])
  accounted_for <- sum(all_ctx$freq)
  freqs <- all_ctx$freq
  fake_data <- generate_fake_data(freqs, counts, probs, model$vals)
  if (accounted_for < model$data_size) {
    ## we have proper extended contexts, we need to get them
    ## we rely on predict for that
    nb <- model$data_size - accounted_for
    unmatched_data <- model$vals[model$ix[1:nb] + 1]
    extended_ctx_probs <- predict(model, unmatched_data,
      final_pred = FALSE,
      type = "probs"
    )
    if (is.factor(model$vals)) {
      the_levels <- levels(model$vals)
    } else {
      the_levels <- model$vals
    }
    fake_data$response <- c(
      fake_data$response,
      factor(the_levels[model$ix[1:nb] + 1], levels = the_levels)
    )
    if (length(model$vals) == 2) {
      fake_data$predictor <- c(fake_data$predictor, extended_ctx_probs[, 2])
    } else {
      fake_data$predictor <- rbind(fake_data$predictor, extended_ctx_probs)
    }
  }
  assertthat::assert_that(length(fake_data$response) == model$data_size)
  res <- main_metrics(fake_data$response, fake_data$predictor)
  rownames(res$conf_mat) <- model$vals
  colnames(res$conf_mat) <- model$vals
  res$model <- model
  res$fake_data <- fake_data
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
