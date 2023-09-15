#' Sampling from Logistic Model
#'
#' This generic function provides a unified interface for sampling by logistic
#' models, regardless of the back-end (`glm`, `vglm` and `multinom`).
#'
#' @param model a logistic model
#' @param newdata new data
#'
#' @return a single value obtained by sampling according to the distribution
#'   predicted by the logistic model based on the provided observation
#' @noRd
glm_sample_one <- function(model, newdata) {
  UseMethod("glm_sample_one")
}

#' @exportS3Method
glm_sample_one.glm <- function(model, newdata) {
  newdata <- glm_drop_level_correction(model, newdata, model$xlevels)
  probs <- stats::predict(model, newdata = newdata, type = "response")
  if (stats::runif(1) <= probs) {
    1
  } else {
    0
  }
}

#' @exportS3Method
glm_sample_one.vglm <- function(model, newdata) {
  if (ncol(newdata) == 0) {
    probs <- VGAM::predictvglm(model, type = "response")[1, ]
  } else {
    probs <- VGAM::predictvglm(model, newdata, type = "response")[1, ]
  }
  sample(0:(length(probs) - 1), 1, prob = probs)
}

#' @exportS3Method
glm_sample_one.multinom <- function(model, newdata) {
  probs <- stats::predict(model, newdata, type = "probs")
  if (length(probs) >= 2) {
    sample(0:(length(probs) - 1), 1, prob = probs)
  } else {
    if (stats::runif(1) <= probs) {
      1
    } else {
      0
    }
  }
}
