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
  attr(model$formula, ".Environment") <- baseenv()
  model
}

#' @exportS3Method
glm_trim.multinom <- function(model) {
  butcher::butcher(model)
}

#' @exportS3Method
glm_trim.vglm <- function(model) {
  attr(model@terms$terms, ".Environment") <- baseenv()
  attr(model@misc$formula, ".Environment") <- baseenv()
  model
}
