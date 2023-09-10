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
  smm <- stats::model.matrix(~ target - 1)
  if (ncol(smm) != ncol(probs)) {
    ## degenerate model
    model_levels <- glm_levels(model)
    mapper <- match(levels(target), model_levels)
    for (i in (1:ncol(smm))[is.na(mapper)]) {
      if (any(smm[, i] == 1)) {
        return(-Inf)
      }
    }
    sum(log(probs) * smm[, which(!is.na(mapper)), drop = FALSE])
  } else {
    sum(log(probs) * smm)
  }
}

#' @exportS3Method
glm_likelihood.multinom <- function(model, mm, target) {
  probs <- stats::predict(model, mm, type = "probs")
  tm <- stats::model.matrix(~ target - 1)
  if (ncol(tm) == 1) {
    sum_log_prob(probs, tm[, 1]) + sum_log_prob(1 - probs, 1 - tm[, 1])
  } else {
    if (!is.matrix(probs) || ncol(probs) != ncol(tm)) {
      ## degenerate model
      model_levels <- glm_levels(model)
      mapper <- match(levels(target), model_levels)
      for (i in (1:ncol(tm))[is.na(mapper)]) {
        if (any(tm[, i] == 1)) {
          return(-Inf)
        }
      }
      tm_tokeep <- tm[, which(!is.na(mapper)), drop = FALSE]
      if (!is.matrix(probs)) {
        ## this happens if we have only one prediction to make or if the model
        ## has degenerate to a single regression and we should have only two
        ## columns in tm_tokeep
        if (nrow(mm) > 1) {
          ## degenerate case
          assertthat::assert_that(ncol(tm_tokeep) == 2)
          sum_log_prob(probs, tm_tokeep[, 2]) + sum_log_prob(1 - probs, 1 - tm_tokeep[, 2])
        } else {
          ## single prediction
          if (length(probs) == ncol(tm_tokeep)) {
            sum(log(probs) * tm_tokeep[1, ])
          } else {
            sum_log_prob(probs, tm_tokeep[, 2]) + sum_log_prob(1 - probs, 1 - tm_tokeep[, 2])
          }
        }
      } else {
        sum(log(probs) * tm)
      }
    } else {
      sum(log(probs) * tm)
    }
  }
}

glm_metrics <- function(model, mm, target) {
  UseMethod("glm_metrics")
}

#' @exportS3Method
glm_metrics.glm <- function(model, mm, target) {
  probs <- stats::predict(model, mm, type = "response")
  main_metrics(target, probs)
}

#' @exportS3Method
glm_metrics.vglm <- function(model, mm, target) {
  if (ncol(mm) == 0) {
    one_prob <- VGAM::predictvglm(model, type = "response")[1, ]
    probs <- matrix(one_prob, nrow = nrow(mm), ncol = length(one_prob), byrow = TRUE)
  } else {
    probs <- VGAM::predictvglm(model, mm, type = "response")
  }
  colnames(probs) <- glm_levels(model)
  main_metrics(target, probs)
}

#' @exportS3Method
glm_metrics.multinom <- function(model, mm, target) {
  probs <- stats::predict(model, mm, type = "probs")
  main_metrics(target, probs)
}
