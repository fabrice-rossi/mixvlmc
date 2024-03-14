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
        ## factor variable
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
      } else {
        ## should be a logical variable
        lv_name <- stringr::str_c(var, "TRUE")
        if (lv_name %in% pre_names) {
          res <- c(res, pre_coeff[[lv_name]])
          res_name <- c(res_name, lv_name)
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


## check whether a model is constant
glm_is_constant <- function(model) {
  UseMethod("glm_is_constant")
}

#' @exportS3Method
glm_is_constant.default <- function(model) {
  length(attr(stats::terms(model), "term.labels")) == 0
}

## extract probability distributions from constant models
glm_to_probs <- function(model, lev) {
  UseMethod("glm_to_probs")
}

#' @exportS3Method
glm_to_probs.glm <- function(model, lev) {
  coefs <- stats::coef(model)
  if (length(coefs) > 1) {
    NULL
  } else {
    probs <- model$family$linkinv(as.numeric(coefs))
    c(1 - probs, probs)
  }
}

#' @exportS3Method
glm_to_probs.vglm <- function(model, lev) {
  if (glm_is_constant(model)) {
    pre <- VGAM::predictvglm(model, type = "response")[1, ]
    if (length(pre) < length(lev)) {
      model_lev <- model@extra$colnames.y
      res <- rep(0, length(lev))
      res[match(model_lev, lev)] <- pre
      res
    } else {
      pre
    }
  } else {
    NULL
  }
}

#' @exportS3Method
glm_to_probs.multinom <- function(model, lev) {
  if (glm_is_constant(model)) {
    newdata <- data.frame(tmp = 0L)[-1]
    probs <- stats::predict(model, newdata, type = "probs")
    if (length(lev) == 2) {
      c(1 - probs, probs)
    } else {
      if (length(probs) == 1) {
        ## fully degenerate case
        probs <- c(1 - probs, probs)
      }
      res <- rep(0, length(lev))
      model_lev <- model$lev
      res[match(model_lev, lev)] <- probs
      res
    }
  } else {
    NULL
  }
}
