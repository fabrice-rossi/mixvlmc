## COVLMC case
covlmc_model_covar_names <- function(cov_names, cov_desc, cov_logical) {
  nb_vars <-
    sum(lengths(cov_desc)) - 2 * length(cov_desc) + length(cov_names)
  result <- rep("", nb_vars)
  spos <- 1
  for (v in cov_names) {
    vlevels <- cov_desc[[v]]
    if (is.null(vlevels) || cov_logical[[v]]) {
      result[spos] <- v
      spos <- spos + 1
    } else {
      result[spos:(spos + length(vlevels) - 2)] <-
        stringr::str_c(v, "[", vlevels[-1], "]")
      spos <- spos + length(vlevels) - 1
    }
  }
  result
}

draw_latex_covlmc_model <- function(coefficients, hsize,
                                    covar_names, lev,
                                    control, model) {
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
      if (control$tab_orientation == "horizontal") {
        ## header with variables and one row per level
        header <- stringr::str_c(add_fontsize("$(I)$", control),
          stringr::str_flatten(vars_coeffs, collapse = "&"),
          sep = "&"
        )
        if (isTRUE(control$with_state)) {
          header <- stringr::str_c("{", lev[1], "}&", header)
        }
        if (length(lev) == 2) {
          values <- add_fontsize(signif(coefficients, control$digits), control)
          coeffs <- stringr::str_flatten(values, collapse = "&")
          if (isTRUE(control$with_state)) {
            coeffs <- stringr::str_c("\\textbf{", lev[2], "}&", coeffs)
          }
          stringr::str_c(header, coeffs, sep = "\\\\\n")
        } else {
          tab <- header
          for (k in 1:(length(lev) - 1)) {
            values <- add_fontsize(signif(coefficients[k, ], control$digits), control)
            coeffs <- stringr::str_flatten(values, collapse = "&")
            if (isTRUE(control$with_state)) {
              coeffs <- stringr::str_c("\\textbf{", add_fontsize(lev[k + 1], control), "}&", coeffs)
            }
            tab <- stringr::str_c(tab, coeffs, sep = "\\\\\n")
          }
          tab
        }
      } else {
        ## one column of variables and then one per level - 1
        if (isTRUE(control$with_state)) {
          the_levels <- add_fontsize(lev, control)
          the_levels[1] <- stringr::str_c("{", the_levels[1], "}")
          the_levels[-1] <- stringr::str_c("\\textbf{", the_levels[-1], "}")
        }
        if (length(lev) == 2) {
          if (isTRUE(control$with_state)) {
            tab <- stringr::str_c(the_levels[1], "&", the_levels[2], "\\\\")
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
              "{", the_levels[1], "}&",
              stringr::str_flatten(the_levels[-1], collapse = "&"),
              "\\\\"
            )
          } else {
            tab <- ""
          }
          for (k in 1:(length(lev) - 1)) {
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
      probs <- signif(glm_to_probs(model, lev), control$digit)
      probs
    }
  }
}

covlmc_node2latex <- function(label, node, vals, covars, control) {
  the_node <- stringr::str_c("\\textbf{", label, "}", sep = "")
  if (!is.null(node$model)) {
    if (isTRUE(control$p_value)) {
      the_node <- stringr::str_c(
        the_node, " ",
        "(",
        signif_null(node$model$p_value, control$digits),
        ")"
      )
    }
    model <- draw_latex_covlmc_model(
      node$model$coefficients,
      node$model$hsize,
      covars, vals,
      control, node$model$model
    )
    if (!is.null(model)) {
      if (node$model$hsize > 0) {
        if (control$tab_orientation == "horizontal") {
          nb <- node$model$hsize * length(covars) + 1
          if (isTRUE(control$with_state)) {
            nb <- nb + 1
          }
        } else {
          nb <- length(vals)
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

draw_latex_covlmc <- function(ct, vals, node2latex, params) {
  rec_draw_latex_covlmc <- function(node, label, covars) {
    cat("[", node2latex(label, node, vals, covars, params),
      sep = ""
    )
    if (!is.null(node$children)) {
      cat("\n")
      if (is.null(node[["merged_model"]])) {
        active_children <- seq_along(node$children)
      } else {
        active_children <- setdiff(seq_along(node$children), node$merged)
      }
      for (v in active_children) {
        child <- node$children[[v]]
        if (length(child) > 0) {
          rec_draw_latex_covlmc(child, vals[v], covars)
        }
      }
      if (!is.null(node[["merged_model"]])) {
        the_merged_vals <- stringr::str_c(vals[node$merged], collapse = ", ")
        cat("[", node2latex(
          the_merged_vals,
          list(model = node[["merged_model"]]), vals, covars, params
        ), "]\n")
      }
    }
    cat("]\n")
  }
  start_forest(params)
  ## let us compute first the covar names
  covar_names <- covlmc_model_covar_names(
    ct$cov_names,
    ct$cov_desc,
    ct$cov_logical
  )
  covar_names <- xtable::sanitize(covar_names, "latex")
  ## then compute
  rec_draw_latex_covlmc(ct, "$\\epsilon$", covar_names)
  end_forest(params)
}
