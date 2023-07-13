glm_trim <- function(model) {
  UseMethod("glm_trim")
}

#' @exportS3Method
glm_trim.default <- function(model) {
  model
}

#' @exportS3Method
glm_trim.glm <- function(model) {
  model <- butcher::butcher(model)
  attr(model$formula, ".Environment") <- NULL
  model
}

#' @exportS3Method
glm_trim.multinom <- function(model) {
  butcher::butcher(model)
}
