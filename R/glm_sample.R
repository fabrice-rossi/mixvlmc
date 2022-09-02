
glm_sample_one <- function(model, newdata) {
  UseMethod("glm_sample_one")
}

#' @exportS3Method
glm_sample_one.glm <- function(model, newdata) {
  newdata <- glm_drop_level_correction(model, newdata)
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
