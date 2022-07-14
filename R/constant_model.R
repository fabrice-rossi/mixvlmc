constant_model <- function(target, mm, nb_vals, fake_obs = 1) {
  if (ncol(mm) > 0) {
    nb_coeffs <- (ncol(stats::model.matrix(target ~ ., data = mm))) * (nb_vals - 1)
  } else {
    nb_coeffs <- nb_vals - 1
  }
  coeffs <- rep(NA, nb_coeffs)
  if (nb_vals == 2) {
    ## logistic regression case
    nb_0 <- sum(target == 0)
    nb_1 <- length(target) - nb_0
    f_nb_0 <- nb_0
    f_nb_1 <- nb_1
    if (nb_0 == 0) {
      f_nb_0 <- nb_0 + fake_obs
    } else {
      f_nb_1 <- nb_1 + fake_obs
    }
    prob_1 <- f_nb_1 / (f_nb_1 + f_nb_0)
    coeffs[1] <- stats::binomial()$linkfun(prob_1)
    ll <- log(prob_1) * nb_1 + log(1 - prob_1) * nb_0
    attr(ll, "df") <- nb_coeffs
    attr(ll, "nobs") <- nb_1 + nb_0
    class(ll) <- "logLik"
    structure(list(coefficients = coeffs, ll = ll, rank = 1, target = ifelse(nb_0 == 0, 1, 0)), class = "constant_model")
  } else {
    target_dist <- table(target)
    f_target_dist <- target_dist
    f_target_dist[target_dist == 0] <- fake_obs
    coeffs[1:(nb_vals - 1)] <- VGAM::multilogitlink(matrix(f_target_dist, nrow = 1))
    probs <- f_target_dist / sum(f_target_dist)
    ll <- sum(target_dist * log(probs))
    attr(ll, "df") <- nb_coeffs
    attr(ll, "nobs") <- sum(target_dist)
    class(ll) <- "logLik"
    structure(list(coefficients = coeffs, ll = ll, rank = nb_vals - 1, target = which(target_dist > 0) - 1), class = "constant_model")
  }
}

#' @exportS3Method
coef.constant_model <- function(object, ...) {
  object$coefficients
}

#' @exportS3Method
logLik.constant_model <- function(object, ...) {
  object$ll
}

#' @exportS3Method
predict.constant_model <- function(object, ...) {
  args <- list(...)
  assertthat::assert_that(length(args) == 1)
  if (object$rank == 1) {
    rep(stats::binomial()$linkinv(object$coefficients[1]), nrow(args[[1]]))
  } else {
    base_prob <- VGAM::multilogitlink(matrix(object$coefficients[1:object$rank], ncol = object$rank), inverse = TRUE)[1, ]
    matrix(base_prob, ncol = object$rank + 1, nrow = nrow(args[[1]]), byrow = TRUE)
  }
}

#' @exportS3Method
glm_likelihood.constant_model <- function(model, mm, target) {
  probs <- stats::predict(model, mm)
  if (model$rank == 1) {
    sum(log(probs) * target + log(1 - probs) * (1 - target))
  } else {
    sum(log(probs) * stats::model.matrix(~ target - 1))
  }
}

#' @exportS3Method
glm_coef.constant_model <- function(model) {
  if (model$rank > 1) {
    matrix(model$coefficients, nrow = model$rank)
  } else {
    model$coefficients
  }
}

#' @exportS3Method
glm_sample_one.constant_model <- function(model, newdata) {
  model$target
}
