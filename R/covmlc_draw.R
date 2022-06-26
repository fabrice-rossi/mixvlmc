draw_covlmc_node <- function(node, ...) {
  if (!is.null(node$model)) {
    paste(node$model$p_value, "[", paste(round(node$model$coefficients, 2), collapse = " "), "]")
  } else {
    ""
  }
}

draw_covlmc_merged <- function(node, ...) {
  if (!is.null(node$merged_model)) {
    paste(node$merged_model$p_value, "[", paste(round(node$merged_model$coefficients, 2), collapse = " "), "]")
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
      local_prefix <- paste0(local_prefix, vals[ival])
    } else {
      local_prefix <- ""
    }
    cat(paste0(prefix, local_prefix))
    if (!is.null(node2txt)) {
      cat(" (", node2txt(ct, ...), ")")
    }
    cat("\n")
    # then go down the tree
    nst <- nb_sub_tree(ct)
    if (nst > 1) {
      prefix <- paste0(prefix, "| ")
    } else {
      prefix <- paste0(prefix, "  ")
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
      cat(paste0(prefix, "' "))
      cat(paste(vals[ct$merged], collapse = ", "))
      if (!is.null(merged_node2txt)) {
        cat(" (", merged_node2txt(ct, ...), ")")
      }
      cat("\n")
    }
  }
}

#' @export
draw.covlmc <- function(ct, node2txt = draw_covlmc_node, ...) {
  rec_draw_covlmc("", 0, 1, length(ct$vals), ct, ct$vals, node2txt, draw_covlmc_merged, ...)
  invisible(ct)
}
