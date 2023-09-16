constant_model <- function(target, mm, nb_vals, pseudo_obs = 1) {
  if (ncol(mm) > 0) {
    nb_coeffs <- (ncol(stats::model.matrix(target ~ ., data = mm))) * (nb_vals - 1)
    cov_desc <- list(names = names(mm), levels = lapply(mm, levels), types = lapply(mm, typeof))
  } else {
    nb_coeffs <- nb_vals - 1
    cov_desc <- NULL
  }
  coeffs <- rep(NA, nb_coeffs)
  if (nb_vals == 2) {
    ## logistic regression case
    nb_0 <- sum(target == 0)
    nb_1 <- length(target) - nb_0
    f_nb_0 <- nb_0 + pseudo_obs
    f_nb_1 <- nb_1 + pseudo_obs
    prob_1 <- f_nb_1 / (f_nb_1 + f_nb_0)
    coeffs[1] <- stats::binomial()$linkfun(prob_1)
    ll <- log(prob_1) * nb_1 + log(1 - prob_1) * nb_0
    attr(ll, "df") <- nb_coeffs
    attr(ll, "nobs") <- nb_1 + nb_0
    class(ll) <- "logLik"
    structure(list(coefficients = coeffs, ll = ll, rank = 1, target = ifelse(nb_0 == 0, 1, 0), cov_desc = cov_desc), class = "constant_model")
  } else {
    target_dist <- table(target)
    f_target_dist <- target_dist + pseudo_obs
    coeffs[1:(nb_vals - 1)] <- VGAM::multilogitlink(matrix(f_target_dist, nrow = 1))
    probs <- f_target_dist / sum(f_target_dist)
    ll <- sum(target_dist * log(probs))
    attr(ll, "df") <- nb_coeffs
    attr(ll, "nobs") <- sum(target_dist)
    class(ll) <- "logLik"
    structure(list(coefficients = coeffs, ll = ll, rank = nb_vals - 1, target = which(target_dist > 0) - 1, cov_desc = cov_desc), class = "constant_model")
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
predict.constant_model <- function(object, newdata = NULL, ...) {
  args <- list(...)
  if (is.null(newdata)) {
    nobs <- attr(object$ll, "nobs")
  } else {
    assertthat::assert_that(is.data.frame(newdata))
    if (is.null(object[["cov_desc"]])) {
      assertthat::assert_that(ncol(newdata) == 0)
    } else {
      assertthat::assert_that(assertthat::has_name(newdata, object$cov_desc$names))
      assertthat::assert_that(assertthat::are_equal(lapply(newdata[object$cov_desc$names], levels), object$cov_desc$levels))
      assertthat::assert_that(assertthat::are_equal(lapply(newdata[object$cov_desc$names], typeof), object$cov_desc$types))
    }
    nobs <- nrow(newdata)
  }
  if (object$rank == 1) {
    rep(stats::binomial()$linkinv(object$coefficients[1]), nobs)
  } else {
    base_prob <- VGAM::multilogitlink(matrix(object$coefficients[1:object$rank], ncol = object$rank), inverse = TRUE)[1, ]
    matrix(base_prob, ncol = object$rank + 1, nrow = nobs, byrow = TRUE)
  }
}

#' @exportS3Method
glm_likelihood.constant_model <- function(model, mm, target) {
  probs <- stats::predict(model, newdata = mm)
  if (model$rank == 1) {
    sum(log(probs) * target + log(1 - probs) * (1 - target))
  } else {
    sum(log(probs) * stats::model.matrix(~ target - 1))
  }
}

#' @exportS3Method
glm_coef.constant_model <- function(model, data) {
  if (model$rank > 1) {
    matrix(model$coefficients, nrow = model$rank)
  } else {
    model$coefficients
  }
}

#' @exportS3Method
glm_sample_one.constant_model <- function(model, newdata, lev) {
  model$target
}

#' @exportS3Method
glm_variable_names.constant_model <- function(model, data) {
  "(I)"
}

#' @exportS3Method
glm_levels.constant_model <- function(model, vals) {
  vals
}

#' @exportS3Method
glm_metrics.constant_model <- function(model, mm, target) {
  probs <- predict(model, mm, target)
  main_metrics(target, probs)
}
