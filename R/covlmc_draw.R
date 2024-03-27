draw_covlmc_model <- function(coefficients, p_value, hsize, names, vals, lev,
                              control, model, probs) {
  collapsed <- FALSE
  if (control[["model"]] == "coef" || control[["model"]] == "full") {
    if (control[["model"]] == "coef") {
      if (isTRUE(control$collapse_constant) && hsize == 0) {
        if (!is.null(model)) {
          coeffs <- stringr::str_c(signif(glm_to_probs(model, vals), control$digit),
            collapse = ", "
          )
        } else {
          coeffs <- stringr::str_c(signif(probs, control$digit),
            collapse = ", "
          )
        }
        collapsed <- TRUE
      } else if (isTRUE(control$with_state)) {
        lev <- stringr::str_c(lev[-1], lev[1], sep = "/")
        coeffs <- pp_mat(coefficients, control$digits,
          sep = control$time_sep,
          groups = hsize, rownames = lev, rn_sep = control$level_sep,
          first_grp_sep = control$intercept_sep
        )
      } else {
        coeffs <- pp_mat(coefficients, control$digits,
          sep = control$time_sep,
          groups = hsize,
          first_grp_sep = control$intercept_sep
        )
      }
    } else {
      if (isTRUE(control$with_state)) {
        lev <- as.character(lev)
        lev[1] <- stringr::str_c("(", lev[1], ")")
        coeffs <- pp_mat(coefficients, control$digits,
          sep = control$time_sep,
          groups = hsize, colnames = names, rownames = lev,
          rn_sep = control$level_sep,
          first_grp_sep = control$intercept_sep
        )
      } else {
        coeffs <- pp_mat(coefficients, control$digits,
          sep = control$time_sep,
          groups = hsize, colnames = names,
          first_grp_sep = control$intercept_sep
        )
      }
    }
    if (length(coeffs) == 1) {
      if (isTRUE(control$p_value)) {
        p_value_str <- stringr::str_c(control$open_p_value,
          signif_null(p_value, control$digits),
          control$close_p_value,
          sep = ""
        )
        if (collapsed) {
          stringr::str_c(p_value_str, coeffs, sep = " ")
        } else {
          stringr::str_c(p_value_str, control$open_model, coeffs,
            control$close_model,
            sep = " "
          )
        }
      } else {
        if (collapsed) {
          coeffs
        } else {
          stringr::str_c(control$open_model, coeffs, control$close_model, sep = " ")
        }
      }
    } else {
      if (isTRUE(control$p_value)) {
        p_value_str <- stringr::str_c(control$open_p_value,
          as.character(signif_null(p_value, control$digits)),
          control$close_p_value,
          sep = ""
        )
        pre_pad_length <- cli::utf8_nchar(coeffs[1], "width")
        coeffs[1] <- stringr::str_c(
          p_value_str, " ", control$open_model, " ",
          coeffs[1]
        )
      } else {
        pre_pad_length <- cli::utf8_nchar(coeffs[1], "width")
        coeffs[1] <- stringr::str_c(control$open_model, " ", coeffs[1])
      }
      pad <- stringr::str_pad("", cli::utf8_nchar(coeffs[1], "width") - pre_pad_length)
      for (k in 2:length(coeffs)) {
        coeffs[k] <- stringr::str_c(pad, coeffs[k], sep = "")
      }
      coeffs[length(coeffs)] <- stringr::str_c(coeffs[length(coeffs)], control$close_model, sep = " ")
      coeffs
    }
  } else {
    if (isTRUE(control$p_value)) {
      as.character(signif_null(p_value, control$digits))
    } else {
      NULL
    }
  }
}

rec_draw_covlmc <- function(label, prefix, ct, vals, control, node2txt) {
  cat(label)
  if (!is.null(node2txt)) {
    node_txt <- node2txt(ct, vals, control)
    if (!is.null(node_txt)) {
      cat_with_prefix(label, prefix, node_txt, control)
    }
  }
  cat("\n")
  if (!is.null(ct$children)) {
    c_symbol <- control$first_node
    idx <- 1
    nst <- nb_sub_tree(ct)
    nb_nodes <- sum(sapply(ct$children, \(x) length(x) > 0))
    if (is.null(ct[["merged_model"]])) {
      active_children <- seq_along(ct$children)
    } else {
      active_children <- setdiff(seq_along(ct$children), ct$merged)
      nb_nodes <- nb_nodes + 1
    }
    for (v in active_children) {
      child <- ct$children[[v]]
      if (length(child) > 0) {
        c_prelabel <- stringr::str_c(c_symbol, control$hbranch, " ")
        if (idx < nst) {
          c_prefix <- control$vbranch
        } else {
          c_prefix <- stringr::str_pad("", cli::utf8_nchar(control$vbranch, "width"))
        }
        c_prefix <- utf8_pad(c_prefix, cli::utf8_nchar(c_prelabel, "width"), "right")
        ## recursive call
        rec_draw_covlmc(
          stringr::str_c(prefix, c_prelabel, vals[v]),
          stringr::str_c(prefix, c_prefix), child, vals, control, node2txt
        )
        ## prepare for next child
        idx <- idx + 1
        if (idx == nb_nodes) {
          c_symbol <- control$final_node
        } else {
          c_symbol <- control$next_node
        }
      }
    }
    if (!is.null(ct[["merged_model"]])) {
      the_merged_vals <- stringr::str_c(vals[ct$merged], collapse = ", ")
      c_prelabel <- stringr::str_c(control$final_node, control$hbranch, " ")
      c_prefix <- stringr::str_pad("", cli::utf8_nchar(control$vbranch, "width"))
      c_prefix <- utf8_pad(c_prefix, cli::utf8_nchar(c_prelabel, "width"), "right")
      c_label <- stringr::str_c(prefix, c_prelabel, the_merged_vals)
      c_prefix <- stringr::str_c(prefix, c_prefix)
      cat(c_label)
      if (!is.null(node2txt)) {
        mm <- ct[["merged_model"]]
        node_txt <- node2txt(list(
          model = mm, model_probs = mm$model_probs,
          model_levels = mm$model_levels
        ), vals, control)
        if (!is.null(node_txt)) {
          cat_with_prefix(c_label, c_prefix, node_txt, control)
        }
      }
      cat("\n")
    }
  }
}

covlmc_node2txt <- function(node, vals, control) {
  if (!is.null(node[["model"]])) {
    if (!is.null(node$model[["model"]])) {
      model_levels <- glm_levels(node$model$model, vals)
    } else {
      ## if the COVLMC has been trimmed
      model_levels <- node[["model_levels"]]
      if (is.null(model_levels)) {
        model_levels <- vals
      }
      model_probs <- node[["model_probs"]]
    }
    var_names <- node$model$var_names
    ## intercept
    var_names[1] <- control$intercept
    draw_covlmc_model(
      node$model$coefficients, node$model$p_value, node$model$hsize,
      var_names, vals, model_levels, control,
      node$model[["model"]], model_probs
    )
  } else if (!is.null(node$p_value) && isTRUE(control$p_value)) {
    stringr::str_c("collapsing:", signif(node$p_value, control$digits), sep = " ")
  } else if (!is.null(node$merged_p_value) && isTRUE(control$p_value)) {
    stringr::str_c(
      "merging (", stringr::str_c(vals[node$merged_candidates], collapse = " and "), "): ",
      signif(node$merged_p_value, control$digits)
    )
  } else {
    NULL
  }
}

#' Text based representation of a covlmc model
#'
#' This function 'draws' a covlmc as a text.
#'
#' The function uses different text based formats (plain "ascii art" and LaTeX)
#' to represent the context tree. Fine tuning of the representation can be done
#' via the [draw_control()] function.
#'
#' Contrarily to [draw()] functions adapted to context trees [draw.ctx_tree()]
#' and VLMC [draw.vlmc()], the present function does not try to produce similar
#' results for the `"text"` format and the `"latex"` format as the `"text"`
#' format is intrinsically more limited in terms of model representations. This
#' is detailed below.
#'
#' @param ct a fitted covlmc model.
#' @param model this parameter controls the display of logistic models
#'   associated to nodes (accepted values: `"coeff"`, `"full"` and `NULL`). The
#'   interpretation of the parameter depends on the format, see below for
#'   details.
#' @param p_value specifies whether the p-values of the likelihood ratio tests
#'   conducted during the covlmc construction must be included in the
#'   representation (defaults to `FALSE`).
#' @param with_state specifies whether to display the state associated to each
#'   dimension of the logistic model (see details).
#' @param constant_as_prob specifies how to represent constant logistic models
#'   for `format="text"` (defaults to `TRUE`, see details). Disregarded when
#'   `format="latex"`.
#'
#' @inheritParams draw
#' @section Format:
#'
#'   The `format` parameter specifies the format used for the textual output.
#'   With the default value `"text"` the output is produced in "ascii art" using
#'   the charset specified by the global option `mixvlmc.charset`.
#'
#'   With the `latex` value, the output is produced in LaTeX, leveraging the
#'   [forest](https://ctan.org/pkg/forest) Latex package (see
#'   <https://ctan.org/pkg/forest>). Each call to `draw.covlmc()` produces a full
#'   `forest` LaTeX environment. This can be included as is in a LaTeX document,
#'   provided the `forest` package  is loaded in the preamble of the document.
#'   The LaTeX output is sanitized to avoid potential problems induced by
#'   special characters in the names of the states of the context tree.
#'
#' @section `"text"` format:
#' ## Parameters
#'
#' When `format="text"` the parameters are interpreted as follows:
#'
#' - `model`: the default `model="coef"` represents only the *coefficients*
#'   of the logistic models associated to each context. `model="full"` includes
#'   the name of the variables in the representation. Setting `model=NULL`
#'   removes the model representations. Additional parameters can be used to
#'   tweak model representations (see below).
#'
#' - `constant_as_prob`: specifies whether to represent logistic models that
#'   do not use covariates (a.k.a. constant models) using the probability
#'   distributions they induce on the state space (default behaviour with
#'   `constant_as_prob=TRUE`) or as normal models (when set to `FALSE`). This is
#'   not taken into account when `model` is not set to `"coef"`.
#'
#' - fields of the `control` list (including the charset):
#'
#'    - `intercept` : character(s) used to represent the intercept when
#'    `model="full"`
#'
#'    - `intercept_sep`: character(s) used to separate the intercept from
#'   the other coefficients in model representation.
#'
#'    - `time_sep`: character(s) used to split the coefficients list by blocks
#'   associated to time delays in the covariate inclusion into the logistic
#'   model. The first block contains the intercept(s), the second block the
#'   covariate values a time t-1, the third block at time t-2, etc.
#'
#'    - `level_sep`: character(s) used separate levels from model, see below.
#'
#'    - `open_p_value` and `close_p_value`: delimiters used around the p-values
#'   when `p_value=TRUE`
#'
#'   - `open_model` and `close_model`: delimiters around the model when `model`
#'   is not `NULL`
#'
#' ## State representation
#'
#'   When `model` is not `NULL`, the coefficients of the logistic models are
#'   presented, organized in rows associated to states. One state is used as the
#'   reference state and the logistic model aims at predicting the ratio of
#'   probability between another state and the reference one (in log scale).
#'   When `with_state` is `TRUE`, the display includes for each row of
#'   coefficients the target state. This is useful when using e.g. `VGAM::vglm()`
#'   as unused levels of the target variable will be automatically dropped from
#'   the model, leading to a reduce number of rows. The reference state is
#'   either shown on the first row if `model` is `"full"` or after the state on
#'   each row if `model` is `"coef"`. States are separated from the model
#'   representation by the character(s) specified in `level_sep` in the
#'   `control` list.
#'
#' @section `"latex"` format:
#' ## Parameters
#' When `format="latex"` the parameters are interpreted as follows:
#'
#' - `model`: the models are always represented completely in the LaTeX export
#'   unless `model` is set to `NULL`.
#'
#' - `constant_as_prob`: in the LaTeX export, constant logistic models are
#'   always represented by the corresponding probability distribution on the
#'   state space, regardless of the value of `constant_as_prob`.
#'
#' - fields of the `control` list:
#'
#'   - `orientation`: specifies the orientation of the tree, either the default
#'     `"vertical"` (expanding from top to bottom) or `"horizontal"` (expanding
#'     from right to left);
#'
#'    - `tab_orientation`: specifies the orientation of the tables used to
#'     represent model coefficients in the tree, either the default `"vertical"`
#'     (covariates are listed on one column) or `"horizontal"` (covariates are listed
#'     on one row);
#'
#'    - `fontsize` and `prob_fontsize` handle the size of the fonts used for the
#'    states and for the models, see [draw_control()] for details;
#'
#'    - `decoration` can be used to add borders around states, see
#'     [draw_control()] for details;
#'
#' ## State representation
#'
#'   When `model` is not `NULL`, the coefficients of the logistic models are
#'   presented, organized in rows or in columns (depending `tab_orientation`) on
#'   associated to states. One state is used as the reference state and the
#'   logistic model aims at predicting the ratio of probability between another
#'   state and the reference one (in log scale). When `with_state` is `TRUE`,
#'   the display includes for each row/column of coefficients the target state.
#'   The reference state is shown on the first row/column.
#'
#' @section Variable representation:
#'
#' When the representation includes the names of the variables used by the
#' logistic models, they are the one generated by the underlying logistic model,
#' e.g. [stats::glm()]. Numerical variable names are used as is, while factors
#' have levels appended. The intercept is denoted by the `intercept` member
#' of the `control` list when`format="text"` (as part of the charset). It is
#' always represented by `(I)` when `format="latex"`.
#'
#' When `format="text"`, the time delays are represented by an underscore
#' followed by the time delay. For instance if the model uses the numerical
#' covariate `y` with two delays, it will appear with two variables `y_1` and
#' `y_2`.
#'
#' When `format="latex"`, the representation uses a temporal subscript of the
#' form `t-1`, `t-2`, etc.
#'
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' rdts <- cut(pc$active_power,
#'   breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1)))
#' )
#' rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
#' m_cov <- covlmc(rdts, rdts_cov, min_size = 5)
#' draw(m_cov, control = draw_control(digits = 3))
#' draw(m_cov, model = NULL)
#' draw(m_cov, p_value = TRUE)
#' draw(m_cov, p_value = FALSE, control = draw_control(digits = 2))
#' draw(m_cov, model = "full", control = draw_control(digits = 3))
#' draw(m_cov, format = "latex", control = draw_control(orientation = "h"))
#' @export
draw.covlmc <- function(ct, format,
                        control = draw_control(), model = c("coef", "full"),
                        p_value = FALSE, with_state = FALSE,
                        constant_as_prob = TRUE,
                        ...) {
  if (rlang::is_missing(format)) {
    format <- "text"
  } else {
    format <- match.arg(format, c("text", "latex"))
  }
  if (is.null(model)) {
    model <- "none"
  } else {
    model <- match.arg(model)
  }
  dot_params <- list(...)
  dot_params$with_state <- with_state
  if (format == "text") {
    rec_draw_covlmc(
      control$root, "", ct, ct$vals,
      c(control, list(
        model = model, p_value = p_value,
        collapse_constant = constant_as_prob
      ), dot_params), covlmc_node2txt
    )
  } else if (format == "latex") {
    draw_latex_covlmc(
      ct, xtable::sanitize(ct$vals, "latex"),
      c(control, list(
        model = model, p_value = p_value,
        collapse_constant = constant_as_prob
      ), dot_params),
      covlmc_node2latex
    )
  }
  invisible(ct)
}
