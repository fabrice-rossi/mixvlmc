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
    pad <- stringr::str_pad("", stringr::str_length(p_value_str) + 5)
    coeffs[1] <- stringr::str_c("( ", p_value_str, " [ ", coeffs[1])
    for (k in 2:length(coeffs)) {
      coeffs[k] <- stringr::str_c(pad, coeffs[k])
    }
    coeffs[length(coeffs)] <- stringr::str_c(coeffs[length(coeffs)], " ])")
    coeffs
  }
}

draw_covlmc_node <- function(node, ...) {
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
    ""
  }
}

draw_covlmc_merged <- function(node, ...) {
  params <- list(...)
  digits <- params$digits
  if (is.null(digits)) {
    digits <- 2
  }
  if (!is.null(node$merged_model)) {
    draw_covlmc_model(node$merged_model$coefficients, node$merged_model$p_value, digits)
  } else {
    ""
  }
}

rec_draw_covlmc <- function(prefix, rank, ival, nst, ct, vals, node2txt, merged_node2txt, ...) {
  ## check for pruned leaf
  if (length(ct) > 0) {
    # first print the current content
    if (rank > 0) {
      if (nst > 1 & rank == 1) {
        local_prefix <- "+ "
      } else {
        local_prefix <- "' "
      }
      local_prefix <- stringr::str_c(local_prefix, vals[ival])
    } else {
      local_prefix <- ""
    }
    cat(stringr::str_c(prefix, local_prefix))
    if (!is.null(node2txt)) {
      node_str <- node2txt(ct, ...)
      local_padding <- stringr::str_pad("", stringr::str_length(local_prefix))
      for (k in seq_along(node_str)) {
        cat(stringr::str_c(" ", node_str[k]))
        if (k < length(node_str)) {
          cat(stringr::str_c("\n", prefix, local_padding))
        }
      }
    }
    cat("\n")
    # then go down the tree
    nst <- nb_sub_tree(ct)
    if (nst > 1) {
      prefix <- stringr::str_c(prefix, "| ")
    } else {
      prefix <- stringr::str_c(prefix, "  ")
    }
    if (is.null(ct[["merged_model"]])) {
      active_children <- seq_along(ct$children)
    } else {
      active_children <- setdiff(seq_along(ct$children), ct$merged)
    }
    for (v in seq_along(active_children)) {
      rec_draw_covlmc(prefix, v, active_children[v], nst, ct$children[[active_children[v]]], vals, node2txt, merged_node2txt, ...)
    }
    if (!is.null(ct[["merged_model"]])) {
      cat(stringr::str_c(prefix, "' "))
      the_merged_vals <- stringr::str_c(vals[ct$merged], collapse = ", ")
      cat(the_merged_vals)
      if (!is.null(merged_node2txt)) {
        node_str <- merged_node2txt(ct, ...)
        local_padding <- stringr::str_pad("", 2 + stringr::str_length(the_merged_vals))
        for (k in seq_along(node_str)) {
          cat(stringr::str_c(" ", node_str[k]))
          if (k < length(node_str)) {
            cat(stringr::str_c("\n", prefix, local_padding))
          }
        }
      }
      cat("\n")
    }
  }
}

#' Text based representation of a covlmc model
#'
#' This function 'draws' a covlmc model as a text.
#'
#' Numerical parameters and p-values are represented using the [base::signif] function.
#' The number of significant digits can be specified using a `digits` parameter.
#'
#' @param ct a fitted covlmc model.
#' @param node2txt an optional function called on each node to render it to a text representation.
#' @param ... additional arguments for node2txt (see details).
#' @return the covlmc model (invisibly).
#'
#' @export
draw.covlmc <- function(ct, node2txt = draw_covlmc_node, ...) {
  rec_draw_covlmc("", 0, 1, length(ct$vals), ct, ct$vals, node2txt, draw_covlmc_merged, ...)
  invisible(ct)
}
