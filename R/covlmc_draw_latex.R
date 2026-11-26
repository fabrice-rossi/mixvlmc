## COVLMC case
covlmc_model_covar_names <- function(cov_names, cov_desc, cov_logical) {
  nb_vars <-
    sum(lengths(cov_desc)) - 2 * length(cov_desc) + length(cov_names)
  result <- rep("", nb_vars)
  spos <- 1
  for (v in cov_names) {
    vlevels <- cov_desc[[v]]
    if (is.null(vlevels)) {
      result[spos] <- v
      spos <- spos + 1
    } else if (cov_logical[[v]]) {
      ## the level is appended as in the text format (e.g. day_1TRUE)
      result[spos] <- stringr::str_c(v, "[TRUE]")
      spos <- spos + 1
    } else {
      result[spos:(spos + length(vlevels) - 2)] <-
        stringr::str_c(v, "[", vlevels[-1], "]")
      spos <- spos + length(vlevels) - 1
    }
  }
  result
}

## base names and suffixes of the parameters of one time step of the
## covariates, as used in the names of the coefficients of the models
## (e.g. z_1b for the level b of the factor z at time step 1)
covlmc_model_covar_parts <- function(cov_names, cov_desc, cov_logical) {
  base <- character()
  suffix <- character()
  for (v in cov_names) {
    vlevels <- cov_desc[[v]]
    if (is.null(vlevels)) {
      base <- c(base, v)
      suffix <- c(suffix, "")
    } else if (cov_logical[[v]]) {
      base <- c(base, v)
      suffix <- c(suffix, "TRUE")
    } else {
      base <- c(base, rep(v, length(vlevels) - 1))
      suffix <- c(suffix, vlevels[-1])
    }
  }
  list(base = base, suffix = suffix)
}

## VGAM::vglm() ignores the levels of factors absent from the data of a
## context: the corresponding coefficients are completed with zeros, as with
## stats::glm() and nnet::multinom(), so that each coefficient matches its
## variable
complete_covlmc_coefficients <- function(coefficients, var_names, hsize, parts) {
  if (is.matrix(coefficients) && !is.null(var_names)) {
    full_names <- c(
      "(I)",
      stringr::str_c(
        rep(parts$base, times = hsize), "_",
        rep(seq_len(hsize), each = length(parts$base)),
        rep(parts$suffix, times = hsize)
      )
    )
    if (ncol(coefficients) < length(full_names)) {
      pos <- match(var_names, full_names)
      if (anyNA(pos) || length(pos) != ncol(coefficients)) {
        stop("internal error: unexpected coefficient names in a covlmc model")
      }
      res <- matrix(0, nrow = nrow(coefficients), ncol = length(full_names))
      res[, pos] <- coefficients
      coefficients <- res
    }
  }
  coefficients
}

draw_latex_covlmc_model <- function(coefficients, hsize,
                                    covar_names, vals, lev,
                                    control, model, model_probs) {
  if (control[["model"]] == "none") {
    NULL
  } else {
    if (hsize > 0) {
      time_shifts <- stringr::str_c("{",
        stringr::str_c("t", 1:hsize, sep = "-"),
        "}",
        sep = ""
      )
      the_vars <- stringr::str_c("\\text{", covar_names, "}", sep = "")
      vars_coeffs <- stringr::str_c("$", rep(the_vars, times = hsize),
        "_", rep(time_shifts, each = length(covar_names)),
        "$",
        sep = ""
      )
      vars_coeffs <- add_fontsize(vars_coeffs, control)
      if (isTRUE(control$with_state)) {
        ## the same font size for all the states (and the variables)
        ref_state <- add_fontsize(lev[1], control)
        target_states <- stringr::str_c(
          "\\textbf{", add_fontsize(lev[-1], control), "}"
        )
      }
      if (control$tab_orientation == "horizontal") {
        ## header with variables and one row per level
        header <- stringr::str_c(add_fontsize("(I)", control),
          stringr::str_flatten(vars_coeffs, collapse = "&"),
          sep = "&"
        )
        if (isTRUE(control$with_state)) {
          header <- stringr::str_c(ref_state, "&", header)
        }
        if (length(lev) == 2) {
          values <- add_fontsize(signif(coefficients, control$digits), control)
          coeffs <- stringr::str_flatten(values, collapse = "&")
          if (isTRUE(control$with_state)) {
            coeffs <- stringr::str_c(target_states[1], "&", coeffs)
          }
          stringr::str_c(header, coeffs, sep = "\\\\\n")
        } else {
          tab <- header
          for (k in 1:(length(lev) - 1)) {
            values <- add_fontsize(signif(coefficients[k, ], control$digits), control)
            coeffs <- stringr::str_flatten(values, collapse = "&")
            if (isTRUE(control$with_state)) {
              coeffs <- stringr::str_c(target_states[k], "&", coeffs)
            }
            tab <- stringr::str_c(tab, coeffs, sep = "\\\\\n")
          }
          tab
        }
      } else {
        ## one column of variables and then one per level - 1
        if (length(lev) == 2) {
          if (isTRUE(control$with_state)) {
            tab <- stringr::str_c(ref_state, "&", target_states[1], "\\\\")
          } else {
            tab <- ""
          }
          values <- add_fontsize(signif(coefficients, control$digits), control)
          stringr::str_c(
            tab,
            stringr::str_c(c(add_fontsize("(I)", control), vars_coeffs), "&",
              values, "\\\\",
              collapse = "\n"
            )
          )
        } else {
          if (isTRUE(control$with_state)) {
            tab <- stringr::str_c(
              ref_state, "&",
              stringr::str_flatten(target_states, collapse = "&"),
              "\\\\"
            )
          } else {
            tab <- ""
          }
          ## one row per variable (the intercept and then the covariates)
          for (k in seq_len(ncol(coefficients))) {
            values <- add_fontsize(signif(coefficients[, k], control$digits), control)
            values <- stringr::str_c(values, collapse = "&")
            if (k == 1) {
              vars <- stringr::str_c(add_fontsize("(I)", control))
            } else {
              vars <- vars_coeffs[k - 1]
            }
            tab <- stringr::str_c(tab, vars, "&", values, "\\\\\n")
          }
          tab
        }
      }
    } else {
      if (!is.null(model)) {
        probs <- signif(glm_to_probs(model, vals), control$digits)
      } else {
        probs <- signif(model_probs, control$digits)
      }
      probs
    }
  }
}

covlmc_node2latex <- function(label, node, vals, covars, control) {
  the_node <- stringr::str_c("\\textbf{", label, "}", sep = "")
  if (!is.null(node$model)) {
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
    if (isTRUE(control$p_value)) {
      the_node <- stringr::str_c(
        the_node, " ",
        "(",
        signif_null(node$model$p_value, control$digits),
        ")"
      )
    }
    coefficients <- node$model$coefficients
    if (node$model$hsize > 0) {
      coefficients <- complete_covlmc_coefficients(
        coefficients, node$model$var_names, node$model$hsize, covars$parts
      )
    }
    model <- draw_latex_covlmc_model(
      coefficients,
      node$model$hsize,
      covars$latex, vals, model_levels,
      control, node$model[["model"]], model_probs
    )
    if (!is.null(model)) {
      if (node$model$hsize > 0) {
        if (control$tab_orientation == "horizontal") {
          nb <- node$model$hsize * length(covars$latex) + 1
          if (isTRUE(control$with_state)) {
            nb <- nb + 1
          }
        } else {
          ## the variables and one column per state but the reference one
          nb <- length(model_levels)
        }
        the_node <- stringr::str_glue("\\multicolumn{{{nb}}{{c}}{{{the_node}}}\\\\\\hline",
          "{xc}, align={al}",
          xc = model,
          al = stringr::str_c(rep("c", nb), collapse = ""),
          .sep = "\n"
        )
      } else {
        the_node <- tabular_content(the_node, model, control)
      }
    }
  } else if (!is.null(node$p_value) && isTRUE(control$p_value)) {
    the_node <- stringr::str_c(
      the_node, "\\\\\\hline ",
      add_fontsize(stringr::str_c(
        "(collapsing: ",
        signif_null(node$p_value, control$digits),
        ")"
      ), control),
      ", align={c}"
    )
  } else if (!is.null(node$merged_p_value) && isTRUE(control$p_value)) {
    the_node <- stringr::str_c(
      the_node, "\\\\\\hline ",
      add_fontsize(stringr::str_c(
        "merging ",
        stringr::str_c(vals[node$merged_candidates], collapse = " and "), ": ",
        signif(node$merged_p_value, control$digits)
      ), control), ", align={c}"
    )
  }
  the_node
}

format_latex_covlmc <- function(ct, vals, control, node2latex) {
  rec_draw_latex_covlmc <- function(node, label, covars) {
    res <- stringr::str_c("[", node2latex(label, node, vals, covars, control))
    if (!is.null(node$children)) {
      if (is.null(node[["merged_model"]])) {
        active_children <- seq_along(node$children)
      } else {
        active_children <- setdiff(seq_along(node$children), node$merged)
      }
      for (v in active_children) {
        child <- node$children[[v]]
        if (length(child) > 0) {
          res <- c(res, rec_draw_latex_covlmc(child, vals[v], covars))
        }
      }
      if (!is.null(node[["merged_model"]])) {
        mm <- node[["merged_model"]]
        the_merged_vals <- stringr::str_c(vals[node$merged], collapse = ", ")
        res <- c(res, stringr::str_c("[", node2latex(
          the_merged_vals,
          list(model = mm, model_probs = mm$model_probs, model_levels = mm$model_levels),
          vals, covars, control
        ), "]"))
      }
    }
    res[length(res)] <- stringr::str_c(res[length(res)], "]")
    res
  }
  ## let us compute first the covar names
  covar_names <- covlmc_model_covar_names(
    ct$cov_names,
    ct$cov_desc,
    ct$cov_logical
  )
  covar_names <- xtable::sanitize(covar_names, "latex")
  covars <- list(
    latex = covar_names,
    parts = covlmc_model_covar_parts(ct$cov_names, ct$cov_desc, ct$cov_logical)
  )
  ## then compute
  c(
    start_forest(control),
    rec_draw_latex_covlmc(ct, "$\\epsilon$", covars),
    end_forest(control)
  )
}
