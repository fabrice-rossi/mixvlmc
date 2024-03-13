draw_covlmc_model <- function(coefficients, p_value, hsize, names, lev, params,
                              control) {
  if (params[["model"]] == "coef" || params[["model"]] == "full") {
    if (params[["model"]] == "coef") {
      if (isTRUE(params$with_state)) {
        lev <- stringr::str_c(lev[-1], lev[1], sep = "/")
        coeffs <- pp_mat(coefficients, params$digits,
          sep = control$time_sep,
          groups = hsize, rownames = lev, rn_sep = control$level_sep,
          first_grp_sep = control$intercept_sep
        )
      } else {
        coeffs <- pp_mat(coefficients, params$digits,
          sep = control$time_sep,
          groups = hsize,
          first_grp_sep = control$intercept_sep
        )
      }
    } else {
      if (isTRUE(params$with_state)) {
        lev <- as.character(lev)
        lev[1] <- stringr::str_c("(", lev[1], ")")
        coeffs <- pp_mat(coefficients, params$digits,
          sep = control$time_sep,
          groups = hsize, colnames = names, rownames = lev,
          rn_sep = control$level_sep,
          first_grp_sep = control$intercept_sep
        )
      } else {
        coeffs <- pp_mat(coefficients, params$digits,
          sep = control$time_sep,
          groups = hsize, colnames = names,
          first_grp_sep = control$intercept_sep
        )
      }
    }
    if (length(coeffs) == 1) {
      if (isTRUE(params$p_value)) {
        p_value_str <- stringr::str_c(control$open_p_value,
          signif_null(p_value, params$digits),
          control$close_p_value,
          sep = ""
        )
        stringr::str_c(p_value_str, "[", coeffs, "]", sep = " ")
      } else {
        stringr::str_c("[", coeffs, "]", sep = " ")
      }
    } else {
      if (isTRUE(params$p_value)) {
        p_value_str <- stringr::str_c(control$open_p_value,
          as.character(signif_null(p_value, params$digits)),
          control$close_p_value,
          sep = ""
        )
        pad <- stringr::str_pad("", stringr::str_length(p_value_str) + 2)
        coeffs[1] <- stringr::str_c(p_value_str, "[", coeffs[1], sep = " ")
      } else {
        coeffs[1] <- stringr::str_c("[", coeffs[1], sep = " ")
        pad <- " "
      }
      for (k in 2:length(coeffs)) {
        coeffs[k] <- stringr::str_c(pad, coeffs[k], sep = " ")
      }
      coeffs[length(coeffs)] <- stringr::str_c(coeffs[length(coeffs)], "]", sep = " ")
      coeffs
    }
  } else {
    if (isTRUE(params$p_value)) {
      as.character(signif_null(p_value, params$digits))
    } else {
      NULL
    }
  }
}

rec_draw_covlmc <- function(label, prefix, ct, vals, control, node2txt, params) {
  cat(label)
  if (!is.null(node2txt)) {
    node_txt <- node2txt(ct, vals, params, control)
    if (!is.null(node_txt)) {
      cat_with_prefix(label, prefix, node_txt, control)
    }
  }
  cat("\n")
  if (!is.null(ct$children)) {
    c_symbol <- control$first_node
    idx <- 1
    nst <- nb_sub_tree(ct)
    if (is.null(ct[["merged_model"]])) {
      active_children <- seq_along(ct$children)
    } else {
      active_children <- setdiff(seq_along(ct$children), ct$merged)
    }
    for (v in active_children) {
      child <- ct$children[[v]]
      if (length(child) > 0) {
        c_prelabel <- stringr::str_c(c_symbol, control$hbranch, " ")
        if (idx < nst) {
          c_prefix <- control$vbranch
        } else {
          c_prefix <- stringr::str_pad("", stringr::str_length(control$vbranch))
        }
        c_prefix <- stringr::str_pad(c_prefix, stringr::str_length(c_prelabel), side = "right")
        ## recursive call
        rec_draw_covlmc(
          stringr::str_c(prefix, c_prelabel, vals[v]),
          stringr::str_c(prefix, c_prefix), child, vals, control, node2txt, params
        )
        ## prepare for next child
        c_symbol <- control$next_node
        idx <- idx + 1
      }
    }
    if (!is.null(ct[["merged_model"]])) {
      the_merged_vals <- stringr::str_c(vals[ct$merged], collapse = ", ")
      c_prelabel <- stringr::str_c(c_symbol, control$hbranch, " ")
      c_prefix <- stringr::str_pad("", stringr::str_length(control$vbranch))
      c_prefix <- stringr::str_pad(c_prefix, stringr::str_length(c_prelabel), side = "right")
      c_label <- stringr::str_c(prefix, c_prelabel, the_merged_vals)
      c_prefix <- stringr::str_c(prefix, c_prefix)
      cat(c_label)
      if (!is.null(node2txt)) {
        node_txt <- node2txt(list(model = ct[["merged_model"]]), vals, params, control)
        if (!is.null(node_txt)) {
          cat_with_prefix(c_label, c_prefix, node_txt, control)
        }
      }
      cat("\n")
    }
  }
}

covlmc_node2txt <- function(node, vals, params, control) {
  digits <- params$digits
  if (is.null(digits)) {
    digits <- 2
  }
  if (!is.null(node$model)) {
    if (!is.null(node$model$model)) {
      model_levels <- glm_levels(node$model$model, vals)
    } else {
      ## if the COVLMC has been trimmed
      model_levels <- node[["model_levels"]]
      if (is.null(model_levels)) {
        model_levels <- vals
      }
    }
    draw_covlmc_model(
      node$model$coefficients, node$model$p_value, node$model$hsize,
      node$model$var_names, model_levels, params, control
    )
  } else if (!is.null(node$p_value) && isTRUE(params$p_value)) {
    stringr::str_c("collapsing:", signif(node$p_value, params$digits), sep = " ")
  } else if (!is.null(node$merged_p_value) && isTRUE(params$p_value)) {
    stringr::str_c(
      "merging (", stringr::str_c(vals[node$merged_candidates], collapse = " and "), "): ",
      signif(node$merged_p_value, params$digits)
    )
  } else {
    NULL
  }
}

#' Text based representation of a covlmc model
#'
#' @inherit draw
#' @param ct a fitted covlmc model.
#' @param model this parameter controls the display of logistic models
#'   associated to nodes. The default `model="coef"` represents the coefficients
#'   of the logistic models associated to each context. `model="full"` includes
#'   the name of the variables in the representation (see details). Setting
#'   `model=NULL` removes the model representations. Additional parameters can
#'   be used to tweak model representations (see details).
#' @param p_value specifies whether the p-values of the likelihood ratio tests
#'   conducted during the covlmc construction must be included in the
#'   representation (defaults to `FALSE`).
#' @param digits numerical parameters and p-values are represented using the
#'   [base::signif] function, using the number of significant digits specified
#'   with this parameter.
#' @param with_state specifies whether to display the state associated to each
#'   dimension of the logistic model (see details).
#' @section Specific parameters in `control`:
#'
#'   Model representations are affected by the following additional
#'   fields of `control` that are specific to [covlmc()]:
#'
#'   - `intercept_sep`: character(s) used to separate the intercept from
#'   the other coefficients in model representation.
#'
#'   - `time_sep`: character(s) used to split the coefficients list by blocks
#'   associated to time delays in the covariate inclusion into the logistic
#'   model. The first block contains the intercept(s), the second block the
#'   covariate values a time t-1, the third block at time t-2, etc.
#'
#'   - `level_sep`: character(s) used separate levels from model, see below.
#'
#'   - `open_p_value` and `close_p_value`: delimiters used around the p-values
#'   when `p_value=TRUE`
#'
#' @section Variable representation:
#'
#'   When `model="full"`, the representation includes the names of the variables
#'   used by the logistic models. Names are the one generated by the underlying
#'   logistic model, e.g. [stats::glm()]. Numerical variable names are used as
#'   is, while factors have levels appended. The intercept is denoted `(I)` to
#'   save space. The time delays are represented by an underscore followed by
#'   the time delay. For instance if the model uses the numerical covariate `y`
#'   with two delays, it will appear with two variables `y_1` and `y_2`.
#'
#' @section State representation:
#'
#'   When `model` is not `NULL`, the coefficients of the logistic models are
#'   presented, organized in rows associated to states. One state is used as the
#'   reference state and the logistic model aims at predicting the ratio of
#'   probability between another state and the reference one (in log scale).
#'   When `with_state` is `TRUE`, the display includes for each row of
#'   coefficients the target state. This is useful when using e.g. `VGAM::vglm`
#'   as unused levels of the target variable will be automatically dropped from
#'   the model, leading to a reduce number of rows. The reference state is
#'   either shown on the first row if `model` is `"full"` or after the state on
#'   each row if `model` is `"coef"`. States are separated from the model
#'   representation by the character(s) specified in `level_sep` in the `control`
#'   list (see [draw_control()]).
#'
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
#' dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
#' m_cov <- covlmc(dts, dts_cov, min_size = 5)
#' draw(m_cov, digits = 3)
#' draw(m_cov, model = NULL)
#' draw(m_cov, p_value = TRUE)
#' draw(m_cov, p_value = FALSE, control = draw_control(time_sep = " ^ "))
#' draw(m_cov, model = "full", control = draw_control(time_sep = " ^ "))
#' @export
draw.covlmc <- function(ct, control = draw_control(), model = c("coef", "full"),
                        p_value = FALSE, digits = 4, with_state = FALSE,
                        ...) {
  if (is.null(model)) {
    model <- "none"
  } else {
    model <- match.arg(model)
  }
  dot_params <- list(...)
  dot_params$with_state <- with_state
  rec_draw_covlmc(control$root, "", ct, ct$vals, control, covlmc_node2txt, c(list(model = model, p_value = p_value, digits = digits), dot_params))
  invisible(ct)
}
