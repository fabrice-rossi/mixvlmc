glm_warning_ignore <- function(w) {
  to_ignore <- stringr::coll(
    c(
      gettext(c(
        "glm.fit: fitted probabilities numerically 0 or 1 occurred",
        "glm.fit: algorithm did not converge"
      ), domain = "R-stats"),
      ## keep the English text to circumvent inconsistency in locale setting
      "glm.fit: fitted probabilities numerically 0 or 1 occurred",
      "glm.fit: algorithm did not converge"
    )
  )
  for (msg in to_ignore) {
    if (stringr::str_detect(w$message, msg)) {
      rlang::cnd_muffle(w)
    }
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
          try_glm <- try(
            withCallingHandlers(
              warning = glm_warning_ignore,
              result <-
                stats::glm(target ~ .,
                  data = mm, family = stats::binomial(),
                  x = FALSE, y = FALSE,
                  model = FALSE, control = stats::glm.control(maxit = options()[["mixvlmc.maxit"]])
                )
            ),
            silent = TRUE
          )
          if (inherits(try_glm, "try-error")) {
            err_cond <- as.character(attr(try_glm, "condition"))
            if (stringr::str_detect(
              err_cond,
              stringr::coll(gettext("contrasts can be applied only to factors with 2 or more levels",
                domain = "R-stats"
              ))
            ) || stringr::str_detect(
              err_cond, "contrasts can be applied only to factors with 2 or more levels"
            )) {
              ## fake result, interpreted as a low rank result
              result <- structure(list(coefficients = c(NA), ll = NA, rank = 0, target = NA), class = "constant_model")
            } else {
              stop(attr(try_glm, "condition"))
            }
          }
        } else {
          result <-
            stats::glm(target ~ 1,
              family = stats::binomial(),
              x = FALSE, y = FALSE,
              model = FALSE
            )
        }
        if (inherits(result, "glm")) {
          if (!is_glm_low_rank(result)) {
            ## check only convergence for full rank models
            if (!result$converged) {
              warning("glm.fit did not converge")
            }
          } else {
            ## signal non convergence
            if (!result$converged) {
              message("glm.fit did not converge for a discarded low rank model")
            }
          }
        }
      } else {
        if (ncol(mm) > 0) {
          try_vglm <- try(
            withCallingHandlers(
              warning = vgam_warning_ignore,
              result <- VGAM::vglm(target ~ .,
                data = mm, family = VGAM::multinomial(refLevel = 1),
                x.arg = FALSE, y.arg = FALSE, model = FALSE,
                control = VGAM::vglm.control(maxit = options()[["mixvlmc.maxit"]])
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
          if (is_glm_low_rank(result)) {
            if (result@iter >= options()[["mixvlmc.maxit"]]) {
              message("vglm.fit did not converge for a discarded low rank model")
            }
          } else {
            if (result@iter >= options()[["mixvlmc.maxit"]]) {
              warning("vglm.fit did not converge")
            }
          }
        }
      }
      result
    } else if (engine == "multinom") {
      if (ncol(mm) > 0) {
        result <- nnet::multinom(target ~ ., data = mm, trace = FALSE, maxit = options()[["mixvlmc.maxit"]])
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
