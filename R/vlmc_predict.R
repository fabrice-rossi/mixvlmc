#' Next state prediction in a discrete time series for a VLMC
#'
#' This function computes one step ahead predictions for a discrete time series
#' based on a VLMC.
#'
#' Given a time series `X`, at time step `t`, a context is computed using
#' observations from `X[1]` to `X[t-1]`. The prediction is then the most
#' probable state for `X[t]` given this contexts. Ties are broken according to
#' the natural order in the state space, favouring "small" values. The time
#' series of predictions is returned by the function when `type="raw"` (default
#' case).
#'
#' When `type="probs"`, each `X[t]` is associated to the conditional probabilities
#' of the next state given the context. Those probabilities are returned as a matrix
#' of probabilities with column names given by the state names.
#'
#' @param object a fitted vlmc object.
#' @param newdata a time series adapted to the vlmc object.
#' @param type character indicating the type of prediction required. The default
#'  `"raw"` returns actual predictions in the form of a new time series. The
#'  alternative `"probs"` returns a matrix of prediction probabilities (see details).
#' @param ... additional arguments.
#'
#' @returns A vector of predictions if `type="raw"` or a matrix of state probabilities
#'   if `type="probs"`.
#' @export
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
#' model <- vlmc(dts, min_size = 5)
#' predict(model, dts[1:5])
#' predict(model, dts[1:5], "probs")
predict.vlmc <- function(object, newdata, type = c("raw", "probs"), ...) {
  type <- match.arg(type)
  max_depth <- depth(object)
  if (!is.null(newdata)) {
    assertthat::assert_that((typeof(newdata) == typeof(object$vals)) && (class(newdata) == class(object$vals)),
      msg = "newdata is not compatible with the model state space"
    )
    dts <- to_dts(newdata, object$vals)
    ctx <- rev(dts$ix) + 1
  } else {
    stop("newdata is empty.")
  }
  pred_vals <- object$vals ### ATTENTION
  MAT <- NULL
  if (type == "raw") {
    if (max_depth == 0) {
      pred <- which.max(object$f_by) ## implements the tie break rule by default
      thing <- rep(pred, (length(newdata) + 1))
    } else {
      thing <- rep(0L, (length(newdata) + 1))
      thing[1] <- which.max(object$f_by)
      for (ii in 1:length(newdata)) {
        if (ii <= max_depth) {
          ac_ctx <- ctx[(length(ctx) - ii + 1):length(ctx)]
        } else {
          ac_ctx <- ctx[(length(ctx) - ii + 1):(length(ctx) - ii + max_depth)]
        }
        thing[1 + ii] <- which.max(match_context(object, ac_ctx)$tree$f_by)
      }
    }
    MAT <- pred_vals[thing]
  } else {
    if (max_depth == 0) {
      prob <- object$f_by / sum(object$f_by)
      probM <- matrix(rep(prob, each = (length(newdata) + 1)), ncol = length(pred_vals))
    } else {
      probM <- matrix(NA, nrow = (length(newdata) + 1), ncol = length(pred_vals))
      probM[1, ] <- object$f_by / sum(object$f_by)
      for (ii in 1:length(newdata)) {
        if (ii <= max_depth) {
          ac_ctx <- ctx[(length(ctx) - ii + 1):length(ctx)]
        } else {
          ac_ctx <- ctx[(length(ctx) - ii + 1):(length(ctx) - ii + max_depth)]
        }
        ac_prob <- match_context(object, ac_ctx)$tree$f_by
        probM[1 + ii, ] <- ac_prob / sum(ac_prob)
      }
    }
    colnames(probM) <- as.character(pred_vals)
    MAT <- probM
  }
  return(MAT)
}
