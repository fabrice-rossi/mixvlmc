prepare_covariate <- function(covariate, ctx_match, d, ...) {
  UseMethod("prepare_covariate")
}

#' @exportS3Method
prepare_covariate.matrix <- function(covariate, ctx_match, d, with_intercept = FALSE, ...) {
  ncols <- ncol(covariate) * d
  if (with_intercept) {
    ncols <- ncols + 1
  }
  mm <- matrix(0, nrow = length(ctx_match), ncol = ncols)
  if (with_intercept) {
    mm[, 1] <- 1
    tcol <- 2
  } else {
    tcol <- 1
  }
  for (step in 1:d) {
    for (p in 1:ncol(covariate)) {
      mm[, tcol] <- covariate[ctx_match + step, p]
      tcol <- tcol + 1
    }
  }
  mm
}

#' @exportS3Method
prepare_covariate.data.frame <- function(covariate, ctx_match, d, ...) {
  res <- list()
  the_names <- names(covariate)
  for (step in 1:d) {
    for (p in 1:ncol(covariate)) {
      res[[paste0(the_names[p], "_", step)]] <- covariate[ctx_match + step, p]
    }
  }
  list2DF(res)
}

prepare_glm <- function(covariate, ctx_match, d, y) {
  local_mm <- prepare_covariate(covariate, ctx_match, d)
  target <- y[ctx_match + d + 1]
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
  if(inherits(model, "vglm")) {
    probs <- VGAM::predictvglm(model, mm, type = "response")
    sum(log(probs) * stats::model.matrix(~ target - 1))
  } else {
    probs <- stats::predict(model, mm, type = "response")
    sum(log(probs) * target + log(1 - probs) * (1 - target))
  }
}
