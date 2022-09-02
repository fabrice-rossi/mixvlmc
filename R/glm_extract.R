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
