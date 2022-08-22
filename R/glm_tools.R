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

glm_warning_ignore <- function(w) {
  if (stringr::str_detect(
    w$message,
    stringr::coll(gettext("glm.fit: fitted probabilities numerically 0 or 1 occurred", domain = "R-stats"))
  )) {
    rlang::cnd_muffle(w)
  }
}

vgam_warning_ignore <- function(w) {
  to_ignore <- c(
    ".* diagonal elements of the working weights variable 'wz' have been replaced by",
    stringr::fixed("fitted values close to 0 or 1"),
    stringr::fixed("fitted probabilities numerically 0 or 1 occurred"),
    stringr::fixed("some quantities such as z, residuals, SEs may be inaccurate due to convergence at a half-step"),
    stringr::fixed("iterations terminated because half-step sizes are very small")
  )
  for (msg in to_ignore) {
    if (stringr::str_detect(w$message, msg)) {
      rlang::cnd_muffle(w)
    }
  }
}

fit_glm <- function(target, mm, nb_vals, control) {
  assertthat::assert_that(nrow(mm) > 0)
  engine <- options()[["mixvlmc.predictive"]]
  assertthat::assert_that(engine %in% c("glm", "multinom"))
  target_dist <- table(target)
  target_dist <- target_dist[target_dist > 0]
  if (length(target_dist) == 1) {
    ## degenerate case
    constant_model(target, mm, nb_vals, control$pseudo_obs)
  } else {
    if (engine == "glm") {
      if (nb_vals == 2) {
        if (ncol(mm) > 0) {
          withCallingHandlers(
            warning = glm_warning_ignore,
            result <-
              stats::glm(target ~ .,
                data = mm, family = stats::binomial(),
                x = FALSE, y = FALSE,
                model = FALSE, control = stats::glm.control(maxit = 30)
              )
          )
        } else {
          result <-
            stats::glm(target ~ 1,
              family = stats::binomial(),
              x = FALSE, y = FALSE,
              model = FALSE
            )
        }
        assertthat::assert_that(result$converged)
      } else {
        if (ncol(mm) > 0) {
          try_vglm <- try(
            withCallingHandlers(
              warning = vgam_warning_ignore,
              result <- VGAM::vglm(target ~ .,
                data = mm, family = VGAM::multinomial(refLevel = 1),
                x.arg = FALSE, y.arg = FALSE, model = FALSE
              )
            ),
            silent = TRUE
          )
          if (inherits(try_vglm, "try-error")) {
            err_cond <- as.character(attr(try_vglm, "condition"))
            if (stringr::str_detect(err_cond, stringr::coll("vglm() only handles full-rank models (currently)"))) {
              ## fake result, interpreted as a low rank result
              result <- structure(list(coefficients = c(NA), ll = NA, rank = 0, target = NA, class = "constant_model"))
            } else {
              stop(attr(try_vglm, "condition"))
            }
          }
        } else {
          result <-
            VGAM::vglm(target ~ 1,
              data = mm, family = VGAM::multinomial(refLevel = 1),
              x.arg = FALSE, y.arg = FALSE, model = FALSE
            )
        }
        if (inherits(result, "vglm")) {
          assertthat::assert_that(result@iter < VGAM::vglm.control()$maxit)
        }
      }
      result
    } else if (engine == "multinom") {
      if (ncol(mm) > 0) {
        result <- nnet::multinom(target ~ ., data = mm, trace = FALSE)
      } else {
        result <- nnet::multinom(target ~ 1, trace = FALSE)
      }
      result$rank <- length(stats::coef(result))
      result
    } else {
      ## should not happen
      NULL
    }
  }
}

glm_drop_level_correction <- function(model, newdata) {
  xlevels <- model$xlevels
  if (!is.null(xlevels)) {
    for (var in names(xlevels)) {
      nv <- newdata[[var]]
      if (length(levels(nv)) > length(xlevels[[var]])) {
        to_replace <- is.na(match(newdata[[var]], xlevels[[var]]))
        newdata[[var]][to_replace] <- xlevels[[var]][1]
      }
    }
  }
  newdata
}

glm_likelihood <- function(model, mm, target) {
  UseMethod("glm_likelihood")
}

sum_log_prob <- function(prob, indic) {
  pre_res <- sum(log(prob)[indic != 0])
  if (length(pre_res) == 0) {
    0
  } else {
    pre_res
  }
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

is_glm_low_rank <- function(model) {
  UseMethod("is_glm_low_rank")
}

#' @exportS3Method
is_glm_low_rank.vglm <- function(model) {
  model@rank < length(stats::coefficients(model))
}

#' @exportS3Method
is_glm_low_rank.default <- function(model) {
  model$rank < length(stats::coefficients(model))
}

glm_coef <- function(model, data) {
  UseMethod("glm_coef")
}

#' @exportS3Method
glm_coef.default <- function(model, data) {
  stats::coef(model)
}

#' @exportS3Method
glm_coef.glm <- function(model, data) {
  pre_coeff <- stats::coef(model)
  xlevels <- model$xlevels
  if (is.null(xlevels)) {
    pre_coeff
  } else {
    pre_names <- names(pre_coeff)
    data_names <- names(data)
    res <- c(pre_coeff[1]) ## intercept
    res_name <- c(pre_names[1])
    for (var in data_names) {
      if (var %in% pre_names) {
        ## numerical variable, simple case
        res <- c(res, pre_coeff[[var]])
        res_name <- c(res_name, var)
      } else if (var %in% names(xlevels)) {
        ## non numerical
        all_levels <- levels(data[[var]])
        model_levels <- xlevels[[var]]
        ref_level <- model_levels[1]
        for (lv in all_levels) {
          if (lv != ref_level) {
            lv_name <- stringr::str_c(var, lv)
            if (lv %in% model_levels) {
              res <- c(res, pre_coeff[[lv_name]])
            } else {
              res <- c(res, 0)
            }
            res_name <- c(res_name, lv_name)
          }
        }
      }
    }
    names(res) <- res_name
    res
  }
}

#' @exportS3Method
glm_coef.vglm <- function(model, data) {
  nb_columns <- sum(sapply(model@assign, length))
  nb_rows <- length(stats::coef(model)) %/% nb_columns
  matrix(stats::coef(model), nrow = nb_rows)
}

glm_sample_one <- function(model, newdata) {
  UseMethod("glm_sample_one")
}

#' @exportS3Method
glm_sample_one.glm <- function(model, newdata) {
  newdata <- glm_drop_level_correction(model, newdata)
  probs <- stats::predict(model, newdata = newdata, type = "response")
  if (stats::runif(1) <= probs) {
    1
  } else {
    0
  }
}

#' @exportS3Method
glm_sample_one.vglm <- function(model, newdata) {
  if (ncol(newdata) == 0) {
    probs <- VGAM::predictvglm(model, type = "response")[1, ]
  } else {
    probs <- VGAM::predictvglm(model, newdata, type = "response")[1, ]
  }
  sample(0:(length(probs) - 1), 1, prob = probs)
}

#' @exportS3Method
glm_sample_one.multinom <- function(model, newdata) {
  probs <- stats::predict(model, newdata, type = "probs")
  if (length(probs) >= 2) {
    sample(0:(length(probs) - 1), 1, prob = probs)
  } else {
    if (stats::runif(1) <= probs) {
      1
    } else {
      0
    }
  }
}

glm_variable_names <- function(model, data) {
  UseMethod("glm_variable_names")
}

#' @exportS3Method
glm_variable_names.glm <- function(model, data) {
  c("(I)", names(glm_coef(model, data))[-1])
}

#' @exportS3Method
glm_variable_names.vglm <- function(model, data) {
  prenames <- names(stats::coef(model))
  nb_columns <- sum(sapply(model@assign, length))
  if (nb_columns > 1) {
    nb_rows <- length(prenames) %/% nb_columns
    prenames <- matrix(prenames, nrow = nb_rows)
    c("(I)", stringr::str_replace(prenames[1, 2:ncol(prenames)], ":1", ""))
  } else {
    "(I)"
  }
}

#' @exportS3Method
glm_variable_names.multinom <- function(model, data) {
  coef <- stats::coef(model)
  if (is.matrix(coef)) {
    c("(I)", colnames(coef)[-1])
  } else {
    c("(I)", names(coef)[-1])
  }
}

glm_levels <- function(model, vals) {
  UseMethod("glm_levels")
}

#' @exportS3Method
glm_levels.glm <- function(model, vals) {
  vals
}

#' @exportS3Method
glm_levels.vglm <- function(model, vals) {
  model@extra$colnames.y
}

#' @exportS3Method
glm_levels.multinom <- function(model, vals) {
  model$lev
}
