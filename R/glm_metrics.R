glm_likelihood <- function(model, mm, target) {
  UseMethod("glm_likelihood")
}

#' @exportS3Method
glm_likelihood.glm <- function(model, mm, target) {
  probs <- stats::predict(model, mm, type = "response")
  sum_log_prob(probs, target) + sum_log_prob(1 - probs, 1 - target)
}

#' @exportS3Method
glm_likelihood.vglm <- function(model, mm, target) {
  if (ncol(mm) == 0) {
    one_prob <- VGAM::predictvglm(model, type = "response")[1, ]
    probs <- matrix(one_prob, nrow = nrow(mm), ncol = length(one_prob), byrow = TRUE)
  } else {
    probs <- VGAM::predictvglm(model, mm, type = "response")
  }
  sum(log(probs) * stats::model.matrix(~ target - 1))
}

#' @exportS3Method
glm_likelihood.multinom <- function(model, mm, target) {
  probs <- stats::predict(model, mm, type = "probs")
  tm <- stats::model.matrix(~ target - 1)
  if (ncol(tm) == 1) {
    sum_log_prob(probs, tm[, 1]) + sum_log_prob(1 - probs, 1 - tm[, 1])
  } else {
    sum(log(probs) * tm)
  }
}
