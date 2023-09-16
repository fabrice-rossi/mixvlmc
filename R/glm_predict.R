#' Logistic model prediction
#'
#' This generic function provides a unified interface for prediction
#' by logistic models, regardless of the back-end (`glm`, `vglm` and
#' `multinom`). In particular, it takes into account degenerate models.
#'
#' @param model a logistic model
#' @param newdata optional new data
#' @param lev original levels of the target variable
#'
#' @return a matrix of predicted probabilities is the target variable has at
#' least three levels or a vector of predicted probabilities of the positive
#' class for a target variable with only two levels.
#' @noRd
glm_predict <- function(model, newdata = NULL, lev) {
  UseMethod("glm_predict")
}

glm_add_missing_prediction <- function(probs, lev) {
  if ((length(lev) > 2) && (ncol(probs) < length(lev))) {
    ## degenerate case
    preprobs <- matrix(0, nrow = nrow(probs), ncol = length(lev))
    preprobs[, match(colnames(probs), lev)] <- probs
    colnames(preprobs) <- lev
    probs <- preprobs
  }
  probs
}

#' @exportS3Method
glm_predict.default <- function(model, newdata = NULL, lev) {
  if (!is.null(newdata)) {
    newdata <- glm_drop_level_correction(model, newdata, model$xlevels)
  }
  probs <- predict(model, newdata = newdata, type = "response")
  if (!is.null(newdata) && nrow(newdata) == 1 && !is.matrix(probs)) {
    probs <- matrix(probs, nrow = 1)
  }
  glm_add_missing_prediction(probs, lev)
}

#' @exportS3Method
glm_predict.vglm <- function(model, newdata = NULL, lev) {
  if (!is.null(newdata)) {
    if (ncol(newdata) == 0) {
      one_prob <- VGAM::predictvglm(model, type = "response")[1, ]
      probs <- matrix(one_prob, nrow = nrow(newdata), ncol = length(one_prob), byrow = TRUE)
    } else {
      probs <- VGAM::predictvglm(model, newdata, type = "response")
    }
  } else {
    probs <- VGAM::predictvglm(model, type = "response")
  }
  if (length(lev) != ncol(probs)) {
    ## degenerate case
    colnames(probs) <- model@extra$colnames.y
    probs <- glm_add_missing_prediction(probs, lev)
  }
  probs
}

#' @exportS3Method
glm_predict.multinom <- function(model, newdata = NULL, lev) {
  if (is.null(newdata)) {
    probs <- predict(model, type = "probs")
  } else {
    probs <- predict(model, newdata = newdata, type = "probs")
    if (nrow(newdata) == 1 && !is.matrix(probs)) {
      probs <- matrix(probs, nrow = 1)
    }
  }
  if (length(lev) > 2) {
    if (!is.matrix(probs) || ncol(probs) == 1) {
      probs <- cbind(1 - probs, probs)
      colnames(probs) <- model$lev
      probs <- glm_add_missing_prediction(probs, lev)
    } else if (ncol(probs) < length(lev)) {
      colnames(probs) <- model$lev
      probs <- glm_add_missing_prediction(probs, lev)
    }
  }
  probs
}
