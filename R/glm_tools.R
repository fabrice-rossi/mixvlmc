prepare_covariate <- function(covariate, ctx_match, d, from, ...) {
  UseMethod("prepare_covariate")
}

#' @exportS3Method
prepare_covariate.matrix <- function(covariate, ctx_match, d, from, with_intercept = FALSE, ...) {
  if (d > 0) {
    ncols <- ncol(covariate) * d
    if (with_intercept) {
      ncols <- ncols + 1
    }
    mm <- matrix(0, nrow = length(ctx_match), ncol = ncols)
    if (with_intercept) {
      mm[, 1] <- 1
    }
    tcol <- ncols
    for (step in 1:d) {
      for (p in ncol(covariate):1) {
        mm[, tcol] <- covariate[ctx_match + from + step, p]
        tcol <- tcol - 1
      }
    }
    mm
  } else {
    matrix(double(), ncol = 0, nrow = 1)
  }
}

#' @exportS3Method
prepare_covariate.data.frame <- function(covariate, ctx_match, d, from, ...) {
  if (d > 0) {
    res <- list()
    the_names <- names(covariate)
    for (step in 1:d) {
      for (p in 1:ncol(covariate)) {
        res[[paste0(the_names[p], "_", step)]] <- covariate[ctx_match + from + d - step + 1, p]
      }
    }
    list2DF(res)
  } else {
    as.data.frame(matrix(double(), ncol = 0, nrow = 1))
  }
}

prepare_glm <- function(covariate, ctx_match, d, y, from = 0) {
  local_mm <- prepare_covariate(covariate, ctx_match, d, from)
  target <- y[ctx_match + from + d + 1]
  to_keep <- !is.na(target)
  list(local_mm = local_mm[to_keep, , drop = FALSE], target = target[to_keep])
}

fit_glm <- function(target, mm, nb_vals) {
  assertthat::assert_that(nrow(mm) > 0)
  if (nb_vals == 2) {
    if (ncol(mm) > 0) {
      suppressWarnings(result <-
        stats::glm(target ~ .,
          data = mm, family = stats::binomial(),
          method = spaMM::spaMM_glm.fit, x = FALSE, y = FALSE,
          model = FALSE
        ))
    } else {
      suppressWarnings(result <-
        stats::glm(target ~ 1,
          family = stats::binomial(),
          method = spaMM::spaMM_glm.fit, x = FALSE, y = FALSE,
          model = FALSE
        ))
    }
  } else {
    if (ncol(mm) > 0) {
      suppressWarnings(result <-
        VGAM::vglm(target ~ .,
          data = mm, family = VGAM::multinomial(),
          x.arg = FALSE, y.arg = FALSE, model = FALSE
        ))
    } else {
      suppressWarnings(result <-
        VGAM::vglm(target ~ 1,
          data = mm, family = VGAM::multinomial(),
          x.arg = FALSE, y.arg = FALSE, model = FALSE
        ))
    }
  }
  result
}

glm_likelihood <- function(model, mm, target) {
  if (inherits(model, "vglm")) {
    probs <- VGAM::predictvglm(model, mm, type = "response")
    sum(log(probs) * stats::model.matrix(~ target - 1))
  } else {
    probs <- stats::predict(model, mm, type = "response")
    sum(log(probs) * target + log(1 - probs) * (1 - target))
  }
}

is_glm_low_rank <- function(model) {
  if (inherits(model, "vglm")) {
    model@rank < length(model@coefficients)
  } else {
    model$rank < length(model$coefficients)
  }
}
