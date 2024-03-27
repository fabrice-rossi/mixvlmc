#' Next state prediction in a discrete time series for a VLMC
#'
#' This function computes one step ahead predictions for a discrete time series
#' based on a VLMC.
#'
#' Given a time series `X`, at time step `t`, a context is computed using
#' observations from `X[1]` to `X[t-1]` (see the dedicated section). The
#' prediction is then the most probable state for `X[t]` given this contexts.
#' Ties are broken according to the natural order in the state space, favouring
#' "small" values. The time series of predictions is returned by the function
#' when `type="raw"` (default case).
#'
#' When `type="probs"`, each `X[t]` is associated to the conditional
#' probabilities of the next state given the context. Those probabilities are
#' returned as a matrix of probabilities with column names given by the state
#' names.
#'
#' @section Extended contexts:
#'
#'   As explained in details in [loglikelihood.vlmc()] documentation and in the
#'   dedicated `vignette("likelihood", package = "mixvlmc")`, the first initial
#'   values of a time series do not in general have a proper context for a VLMC
#'   with a non zero order. In order to predict something meaningful for those
#'   values, we rely on the notion of extended context defined in the documents
#'   mentioned above. This follows the same logic as using
#'   [loglikelihood.vlmc()] with the parameter `initial="extended"`. All vlmc
#'   functions that need to manipulate initial values with no proper context use
#'   the same approach.
#'
#' @param object a fitted vlmc object.
#' @param newdata a time series adapted to the vlmc object.
#' @param type character indicating the type of prediction required. The default
#'   `"raw"` returns actual predictions in the form of a new time series. The
#'   alternative `"probs"` returns a matrix of prediction probabilities (see
#'   details).
#' @param final_pred if `TRUE` (default value), the predictions include a final
#'   prediction step, made by computing the context of the full time series.
#'   When `FALSE` this final prediction is not included.
#' @param ... additional arguments.
#'
#' @returns A vector of predictions if `type="raw"` or a matrix of state
#'   probabilities if `type="probs"`.
#' @export
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' rdts <- cut(pc$active_power,
#'   breaks = c(0, quantile(pc$active_power,
#'     probs = c(0.25, 0.5, 0.75, 1)
#'   ))
#' )
#' model <- vlmc(rdts, min_size = 5)
#' predict(model, rdts[1:5])
#' predict(model, rdts[1:5], "probs")
predict.vlmc <- function(object, newdata, type = c("raw", "probs"),
                         final_pred = TRUE, ...) {
  type <- match.arg(type)
  max_depth <- depth(object)
  assertthat::assert_that(rlang::is_logical(final_pred))
  if (!missing(newdata) && !is.null(newdata)) {
    assertthat::assert_that((typeof(newdata) == typeof(object$vals)) && methods::is(newdata, class(object$vals)),
      msg = "newdata is not compatible with the model state space"
    )
    nd_dts <- to_dts(newdata, object$vals)
    ctx <- rev(nd_dts$ix) + 1
  } else {
    stop("newdata must be provided.")
  }
  if (length(newdata) == 0 && final_pred == FALSE) {
    ## degenerate cases
    if (type == "raw") {
      return(object$vals[0])
    } else {
      res <- matrix(0, nrow = 0, ncol = length(object$vals))
      colnames(res) <- as.character(object$vals)
      return(res)
    }
  }
  pred_vals <- object$vals
  MAT <- NULL
  with_final <- as.integer(final_pred)
  if (type == "raw") {
    if (max_depth == 0) {
      pred <- which.max(object$f_by) ## implements the tie break rule by default
      thing <- rep(pred, length(newdata) + with_final)
    } else {
      thing <- rep(0L, length(newdata) + with_final)
      thing[1] <- which.max(object$f_by)
      nb_preds <- length(newdata) - 1 + with_final
      if (nb_preds > 0) {
        for (ii in 1:nb_preds) {
          if (ii <= max_depth) {
            ac_ctx <- ctx[(length(ctx) - ii + 1):length(ctx)]
          } else {
            ac_ctx <- ctx[(length(ctx) - ii + 1):(length(ctx) - ii + max_depth)]
          }
          thing[1 + ii] <- which.max(match_context(object, ac_ctx)$tree$f_by)
        }
      }
    }
    MAT <- pred_vals[thing]
  } else {
    if (max_depth == 0) {
      prob <- object$f_by / sum(object$f_by)
      probM <- matrix(rep(prob, each = length(newdata) + with_final), ncol = length(pred_vals))
    } else {
      probM <- matrix(NA, nrow = length(newdata) + with_final, ncol = length(pred_vals))
      probM[1, ] <- object$f_by / sum(object$f_by)
      nb_preds <- length(newdata) - 1 + with_final
      if (nb_preds > 0) {
        for (ii in 1:nb_preds) {
          if (ii <= max_depth) {
            ac_ctx <- ctx[(length(ctx) - ii + 1):length(ctx)]
          } else {
            ac_ctx <- ctx[(length(ctx) - ii + 1):(length(ctx) - ii + max_depth)]
          }
          ac_prob <- match_context(object, ac_ctx)$tree$f_by
          probM[1 + ii, ] <- ac_prob / sum(ac_prob)
        }
      }
    }
    colnames(probM) <- as.character(pred_vals)
    MAT <- probM
  }
  return(MAT)
}
