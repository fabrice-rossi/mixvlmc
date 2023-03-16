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
    newdata <- glm_drop_level_correction(model, newdata)
  }
  glm_add_missing_prediction(predict(model, newdata = newdata, type = "response"), lev)
}

#' @exportS3Method
glm_predict.multinom <- function(model, newdata = NULL, lev) {
  if (is.null(newdata)) {
    probs <- predict(model, type = "probs")
  } else {
    probs <- predict(model, newdata = newdata, type = "probs")
  }
  glm_add_missing_prediction(probs, lev)
}
