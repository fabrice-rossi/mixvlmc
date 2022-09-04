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

glm_metrics <- function(model, mm, target) {
  UseMethod("glm_metrics")
}

#' @exportS3Method
glm_metrics.glm <- function(model, mm, target) {
  probs <- stats::predict(model, mm, type = "response")
  cm <- as.matrix(table(probs >= 0.5, target))
  acc <- sum(diag(cm)) / length(target)
  roc <- pROC::roc(target, probs, levels = c(0, 1), direction = "<")
  list(accuracy = acc, conf_mat = cm, roc = roc, auc = as.numeric(pROC::auc(roc)))
}

#' @exportS3Method
glm_metrics.vglm <- function(model, mm, target) {
  if (ncol(mm) == 0) {
    one_prob <- VGAM::predictvglm(model, type = "response")[1, ]
    probs <- matrix(one_prob, nrow = nrow(mm), ncol = length(one_prob), byrow = TRUE)
  } else {
    probs <- VGAM::predictvglm(model, mm, type = "response")
  }
  decisions <- apply(probs, 1, which.max)
  nb_levels <- length(levels(target))
  cm <- matrix(0L, ncol = nb_levels, nrow = nb_levels)
  for (k in 1:nrow(cm)) {
    predicted_k <- target[decisions == k]
    if (length(predicted_k) > 0) {
      cm[k, ] <- table(predicted_k)
    }
  }
  acc <- sum(diag(cm)) / length(target)
  if (ncol(probs) != nb_levels) {
    ## degenerate case
    colnames(probs) <- glm_levels(model)
    auc <- NA
    roc <- NULL
  } else {
    colnames(probs) <- levels(target)
    roc <- pROC::multiclass.roc(target, probs)
    auc <- as.numeric(pROC::auc(roc))
  }
  list(accuracy = acc, conf_mat = cm, roc = roc, auc = auc)
}

#' @exportS3Method
glm_metrics.multinom <- function(model, mm, target) {
  probs <- stats::predict(model, mm, type = "probs")
  if (is.null(dim(probs))) {
    if (is.factor(target)) {
      lev <- levels(target)
      nb_levels <- length(lev)
    } else {
      lev <- c(0, 1)
      nb_levels <- 2
    }
    if (nb_levels == 2) {
      cm <- as.matrix(table(lev[1 + as.numeric(probs >= 0.5)], target))
      roc <- pROC::roc(target, probs, levels = c(0, 1), direction = "<")
      auc <- as.numeric(pROC::auc(roc))
    } else {
      ## degenerate case
      cm <- matrix(0L, ncol = nb_levels, nrow = nb_levels)
      p_levels <- match(glm_levels(model), lev)
      cm[p_levels[2], p_levels] <- table(probs >= 0.5, target)[p_levels]
      roc <- NULL
      auc <- NA
    }
  } else {
    decisions <- apply(probs, 1, which.max)
    cm <- matrix(0L, ncol = ncol(probs), nrow = ncol(probs))
    for (k in 1:nrow(cm)) {
      cm[k, ] <- table(target[decisions == k])
    }
    if (any(colSums(cm) == 0)) {
      ## degenerate case
      auc <- NA
      roc <- NULL
    } else {
      roc <- pROC::multiclass.roc(target, probs)
      auc <- as.numeric(pROC::auc(roc))
    }
  }
  acc <- sum(diag(cm)) / length(target)
  list(accuracy = acc, conf_mat = cm, roc = roc, auc = auc)
}
