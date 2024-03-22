rec_draw_cpp <- function(label, prefix, tree, ct, vals, control, node2txt) {
  cat(label)
  if (!is.null(node2txt)) {
    node_txt <- node2txt(ct, control)
    if (!is.null(node_txt)) {
      cat_with_prefix(label, prefix, node_txt, control)
    }
  }
  cat("\n")
  if (!is.null(ct$children)) {
    nst <- sum(!is.na(ct$children))
    if (nst > 1) {
      c_symbol <- control$first_node
    } else {
      c_symbol <- control$next_node
    }
    idx <- 1
    for (v in seq_along(ct$children)) {
      child_idx <- ct$children[[v]]
      if (!is.na(child_idx)) {
        child <- tree[[child_idx]]
        c_prelabel <- stringr::str_c(c_symbol, control$hbranch, " ")
        if (idx < nst) {
          c_prefix <- control$vbranch
        } else {
          c_prefix <- stringr::str_pad("", stringr::str_length(control$vbranch))
        }
        c_prefix <-
          stringr::str_pad(c_prefix, stringr::str_length(c_prelabel),
            side = "right"
          )
        ## recursive call
        rec_draw_cpp(
          stringr::str_c(prefix, c_prelabel, vals[v]),
          stringr::str_c(prefix, c_prefix),
          tree, child, vals, control, node2txt
        )
        ## prepare for next child
        c_symbol <- control$next_node
        idx <- idx + 1
      }
    }
  }
}


#' @export
#' @rdname draw.ctx_tree
draw.ctx_tree_cpp <- function(ct, format, control = draw_control(),
                              frequency = NULL, ...) {
  if (rlang::is_missing(format)) {
    format <- "ascii"
  } else {
    format <- match.arg(format, c("ascii", "latex"))
  }
  restore_model(ct)
  ct_r <- ct$root$representation()
  if (format == "ascii") {
    if (is.null(frequency)) {
      rec_draw_cpp(
        control$root, "", ct_r, ct_r[[1]], ct$vals,
        c(control, list(...)), NULL
      )
    } else {
      frequency <- match.arg(frequency, c("total", "detailed"))
      rec_draw_cpp(
        control$root, "", ct_r, ct_r[[1]], ct$vals,
        c(control, list(frequency = frequency), list(...)),
        ctx_tree_node2txt
      )
    }
  } else if (format == "latex") {
    draw_latex_ctx_tree_cpp(
      ct_r, ct_r[[1]], xtable::sanitize(ct$vals, "latex"),
      c(control, list(...), list(frequency = frequency)),
      ctx_tree_node2latex
    )
  }
  invisible(ct)
}
