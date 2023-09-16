#' Next state prediction in a discrete time series for a VLMC with covariates
#'
#' This function computes one step ahead predictions for a discrete time series
#' based on a VLMC with covariates.
#'
#' Given a time series `X`, at time step `t`, a context is computed using
#' observations from `X[1]` to `X[t-1]` (see the dedicated section). The
#' prediction is then the most probable state for `X[t]` given this logistic
#' model of the context and the corresponding values of the covariates. The time
#' series of predictions is returned by the function when `type="raw"` (default
#' case).
#'
#' When `type="probs"`, the function returns of the probabilities of each state
#' for `X[t]` as estimated by the logistic models. Those probabilities are
#' returned as a matrix of probabilities with column names given by the state
#' names.
#'
#' @section Extended contexts:
#'
#'   As explained in details in [loglikelihood.covlmc()] documentation and in
#'   the dedicated `vignette("likelihood", package = "mixvlmc")`, the first
#'   initial values of a time series do not in general have a proper context for
#'   a COVLMC with a non zero order. In order to predict something meaningful
#'   for those values, we rely on the notion of extended context defined in the
#'   documents mentioned above. This follows the same logic as using
#'   [loglikelihood.covlmc()] with the parameter `initial="extended"`. All
#'   covlmc functions that need to manipulate initial values with no proper
#'   context use the same approach.
#'
#' @param object a fitted covlmc object.
#' @param newdata a time series adapted to the covlmc object.
#' @param newcov a data frame with the new values for the covariates.
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
#' pc <- powerconsumption[powerconsumption$week == 10, ]
#' dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.2, 0.7, 1))))
#' dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
#' m_cov <- covlmc(dts, dts_cov, min_size = 5, alpha = 0.5)
#' dts_probs <- predict(m_cov, dts[1:144], dts_cov[1:144, , drop = FALSE], type = "probs")
#' dts_preds <- predict(m_cov, dts[1:144], dts_cov[1:144, , drop = FALSE],
#'   type = "raw", final_pred = FALSE
#' )
predict.covlmc <- function(object, newdata, newcov, type = c("raw", "probs"),
                           final_pred = TRUE, ...) {
  type <- match.arg(type)
  if (isTRUE(object$trimmed == "full")) {
    stop("predict is not supported by fully trimmed covlmc")
  }
  if (missing(newdata) || is.null(newdata)) {
    stop("newdata must be provided.")
  }
  assertthat::assert_that(
    (typeof(newdata) == typeof(object$vals)) &&
      (class(newdata) == class(object$vals)),
    msg = "newdata is not compatible with the model state space"
  )
  nx <- to_dts(newdata, object$vals)
  x <- nx$ix + 1
  if (missing(newcov) || is.null(newcov)) {
    stop("newcov must be provided.")
  }
  assertthat::assert_that(rlang::is_logical(final_pred))
  assertthat::assert_that(is.data.frame(newcov))
  assertthat::assert_that(nrow(newcov) == length(newdata))
  covariate <- validate_covariate(object, newcov)
  data_size <- length(newdata)
  max_depth <- depth(object)
  with_final <- as.integer(final_pred)
  if (max_depth == 0) {
    ## the logistic model is a constant one
    mm <- covariate[1, , drop = FALSE]
    prob <- glm_predict(object$model$model, mm, lev = object$vals)
    if (length(object$vals) == 2 && ncol(prob) == 1) {
      prob <- c(1 - prob, prob)
    }
    if (type == "raw") {
      pre <- which.max(prob)
      res <- object$vals[rep(pre, data_size + with_final)]
    } else {
      res <- matrix(prob, byrow = TRUE, nrow = data_size + with_final, ncol = length(prob))
      colnames(res) <- as.character(object$vals)
    }
    res
  } else {
    if (type == "raw") {
      res <- rep(0L, data_size + with_final)
    } else {
      res <- matrix(NA, ncol = length(object$vals), nrow = data_size + with_final)
      colnames(res) <- as.character(object$vals)
    }
    ctx <- c()
    for (i in 1:(data_size + with_final)) {
      subtree <- match_context_co(object, ctx)
      if (subtree$merged) {
        local_model <- subtree$tree$merged_model
      } else if (is.null(subtree$tree[["model"]])) {
        local_model <- subtree$tree$extended_model
      } else {
        local_model <- subtree$tree$model
      }
      mm <- prepare_covariate(covariate, i - subtree$depth - 1,
        d = local_model$hsize,
        from = subtree$depth - local_model$hsize
      )
      prob <- glm_predict(local_model$model, mm, lev = object$vals)
      if (length(object$vals) == 2) {
        prob <- c(1 - prob, prob)
      }
      if (type == "raw") {
        res[i] <- which.max(prob)
      } else {
        res[i, ] <- prob
      }
      if (i <= data_size) {
        j <- max(i - max_depth + 1, 1)
        ctx <- x[i:j]
      }
    }
    if (type == "raw") {
      res <- object$vals[res]
    }
    res
  }
}
