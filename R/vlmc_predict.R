#' Predictor of the following state of a discrete time series for a vlmc
#'
#' This function predict each state of a time series from the distribution estimated by the
#' given vlmc object.
#'
#'
#' @param object a fitted vlmc object.
#' @param ts a time series adapted to the vlmc object.
#' @param type charachter indicating the type of prediction required.
#' The default "class" returns the predictor,
#' "probs" returns a matrix of prediction probabilities.
#' @param ... additional arguments.
#'
#' @export
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
#' model <- vlmc(dts, min_size = 5)
#' predict(model, dts[1:5])
predict.vlmc <- function(object, ts, type = "class", ...) {
  max_depth <- depth(object)
  if (!is.null(ts)) {
    assertthat::assert_that((typeof(ts) == typeof(object$vals)) && (class(ts) == class(object$vals)),
                            msg = "ts is not compatible with the model state space"
    )
    dts <- to_dts(ts, object$vals)
    ctx <- rev(dts$ix) + 1
  } else {
    stop("ts is empty.")
  }
  pred_vals <- object$vals ### ATTENTION
  MAT <- NULL
  if ("class" %in% type) {
    if (max_depth == 0) {
      pred <- which.max(object$f_by)
      thing <- matrix(rep(pred, (length(ctx) + 1)), ncol = 1)
    } else {
      thing <- matrix(nrow = (length(ctx) + 1), ncol = 1)
      thing[1, ] <- which.max(object$f_by)
      for (ii in 1:length(ctx)) {
        if (ii <= max_depth) {
          ac_ctx <- ctx[(length(ctx) - ii + 1):length(ctx)]
          thing[1 + ii, ] <- which.max(match_context(object, ac_ctx)$tree$f_by)
        } else {
          ac_ctx <- ctx[(length(ctx) - ii + 1):(length(ctx) - ii + max_depth)]
          thing[1 + ii, ] <- which.max(match_context(object, ac_ctx)$tree$f_by)
        }
      }
    }
    MAT <- cbind(MAT, matrix(pred_vals[thing[, 1]], ncol = 1))
    colnames(MAT) <- "class"
  }
  if ("probs" %in% type) {
    if (max_depth == 0) {
      prob <- object$f_by / sum(object$f_by)
      probM <- matrix(rep(prob, (length(ctx) + 1)), ncol = length(pred_vals), byrow = T)
    } else {
      probM <- matrix(nrow = (length(ctx) + 1), ncol = length(pred_vals))
      probM[1, ] <- object$f_by / sum(object$f_by)
      for (ii in 1:length(ctx)) {
        if (ii <= max_depth) {
          ac_ctx <- ctx[(length(ctx) - ii + 1):length(ctx)]
          ac_prob <- match_context(object, ac_ctx)$tree$f_by
          probM[1 + ii, ] <- ac_prob / sum(ac_prob)
        } else {
          ac_ctx <- ctx[(length(ctx) - ii + 1):(length(ctx) - ii + max_depth)]
          ac_prob <- match_context(object, ac_ctx)$tree$f_by
          probM[1 + ii, ] <- ac_prob / sum(ac_prob)
        }
      }
    }
    colnames(probM) <- paste(pred_vals)
    MAT <- cbind(MAT, probM)
  }
  return(MAT)
}
