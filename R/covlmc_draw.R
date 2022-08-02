draw_covlmc_coef <- function(coef, digits) {
  if (is.matrix(coef)) {
    pp_mat(coef, digits)
  } else {
    stringr::str_c(signif(coef, digits), collapse = " ")
  }
}

draw_covlmc_model <- function(coefficients, p_value, digits) {
  coeffs <- draw_covlmc_coef(coefficients, digits)
  if (length(coeffs) == 1) {
    stringr::str_c(signif_null(p_value, digits), "[", coeffs, "]", sep = " ")
  } else {
    p_value_str <- as.character(signif_null(p_value, digits))
    pad <- stringr::str_pad("", stringr::str_length(p_value_str) + 3)
    coeffs[1] <- stringr::str_c(p_value_str, " [ ", coeffs[1])
    for (k in 2:length(coeffs)) {
      coeffs[k] <- stringr::str_c(pad, coeffs[k])
    }
    coeffs[length(coeffs)] <- stringr::str_c(coeffs[length(coeffs)], " ]")
    coeffs
  }
}

rec_draw_covlmc <- function(label, prefix, ct, vals, control, node2txt, ...) {
  cat(label)
  if (!is.null(node2txt)) {
    node_txt <- node2txt(ct, ...)
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
          stringr::str_c(prefix, c_prefix), child, vals, control, node2txt, ...
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
        node_txt <- node2txt(list(model = ct[["merged_model"]]), ...)
        if (!is.null(node_txt)) {
          cat_with_prefix(c_label, c_prefix, node_txt, control)
        }
      }
      cat("\n")
    }
  }
}

covlmc_node2txt <- function(node, ...) {
  params <- list(...)
  digits <- params$digits
  if (is.null(digits)) {
    digits <- 2
  }
  if (!is.null(node$model)) {
    draw_covlmc_model(node$model$coefficients, node$model$p_value, digits)
  } else if (!is.null(node$p_value)) {
    stringr::str_c("collapsing:", signif(node$p_value, digits), sep = " ")
  } else if (!is.null(node$merged_p_value)) {
    stringr::str_c(
      "merging (", stringr::str_c(node$merged_candidates, collapse = " "), "): ",
      signif(node$merged_p_value, digits)
    )
  } else {
    NULL
  }
}

#' Text based representation of a covlmc model
#'
#' @inherit draw
#' @param ct a fitted covlmc model.
#' @param node2txt an optional function called on each node to render it to a
#'   text representation. Defaults to a full representation of the logistic
#'   models associated to each context, including p-values of the likelihood
#'   ratio tests. Numerical parameters and p-values are represented using the
#'   [base::signif] function. The number of significant digits can be specified
#'   using a `digits` parameter.
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
#' dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
#' m_cov <- covlmc(dts, dts_cov, min_size = 5)
#' draw(m_cov, digits = 3)
#' @export
draw.covlmc <- function(ct, control = draw_control(), node2txt = covlmc_node2txt, ...) {
  rec_draw_covlmc(control$root, "", ct, ct$vals, control, node2txt, ...)
  invisible(ct)
}
