#' Sampling from Logistic Model
#'
#' This generic function provides a unified interface for sampling by logistic
#' models, regardless of the back-end (`glm`, `vglm` and `multinom`). Degenerate
#' models are supported.
#'
#' For binary model, sampling is done directly on binary values and the function
#' return 0 or 1. For categorical models with more than 2 values, sampling is
#' done on value indexes, starting at 0.
#'
#' @param model a logistic model
#' @param newdata new data
#' @param lev original levels of the target variable
#'
#' @return a single (index) value obtained by sampling according to the distribution
#'   predicted by the logistic model based on the provided observation
#' @noRd
glm_sample_one <- function(model, newdata, lev) {
  UseMethod("glm_sample_one")
}

#' @exportS3Method
glm_sample_one.glm <- function(model, newdata, lev) {
  newdata <- glm_drop_level_correction(model, newdata, model$xlevels)
  probs <- stats::predict(model, newdata = newdata, type = "response")
  if (stats::runif(1) <= probs) {
    1
  } else {
    0
  }
}

#' @exportS3Method
glm_sample_one.vglm <- function(model, newdata, lev) {
  if (ncol(newdata) == 0) {
    probs <- VGAM::predictvglm(model, type = "response")[1, ]
  } else {
    probs <- VGAM::predictvglm(model, newdata, type = "response")[1, ]
  }
  if (length(probs) < length(lev)) {
    ## degenerate case
    model_lev <- model@extra$colnames.y
    idx <- match(model_lev, lev) - 1
  } else {
    idx <- 0:(length(probs) - 1)
  }
  sample(idx, 1, prob = probs)
}

#' @exportS3Method
glm_sample_one.multinom <- function(model, newdata, lev) {
  probs <- stats::predict(model, newdata, type = "probs")
  if (length(lev) == 2) {
    if (stats::runif(1) <= probs) {
      1L
    } else {
      0L
    }
  } else {
    if (length(probs) == 1) {
      ## fully degenerate case
      model_lev <- model$lev
      idx <- match(model_lev, lev) - 1
      if (stats::runif(1) <= probs) {
        idx[2]
      } else {
        idx[1]
      }
    } else {
      if (length(probs) < length(lev)) {
        ## degenerate case
        model_lev <- model$lev
        idx <- match(model_lev, lev) - 1
      } else {
        idx <- 0:(length(probs) - 1)
      }
      sample(idx, 1, prob = probs)
    }
  }
}
