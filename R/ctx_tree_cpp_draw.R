rec_draw_cpp <- function(label, prefix, tree, ct, vals, control, node2txt, params) {
  cat(label)
  if (!is.null(node2txt)) {
    node_txt <- node2txt(ct, params)
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
        c_prefix <- stringr::str_pad(c_prefix, stringr::str_length(c_prelabel), side = "right")
        ## recursive call
        rec_draw_cpp(
          stringr::str_c(prefix, c_prelabel, vals[v]),
          stringr::str_c(prefix, c_prefix), tree, child, vals, control, node2txt, params
        )
        ## prepare for next child
        c_symbol <- control$next_node
        idx <- idx + 1
      }
    }
  }
}


#' @export
#' @rdname ctx_tree
draw.ctx_tree_cpp <- function(ct, control = draw_control(), frequency = NULL, ...) {
  if (extptr_is_null(ct$root$.pointer)) {
    stop("Missing C++ representation!\nThis object was probably restored from a saved object.\n")
  }
  ct_r <- ct$root$representation()
  if (is.null(frequency)) {
    rec_draw_cpp(control$root, "", ct_r, ct_r[[1]], ct$vals, control, NULL, list(...))
  } else {
    frequency <- match.arg(frequency, c("total", "detailed"))
    rec_draw_cpp(control$root, "", ct_r, ct_r[[1]], ct$vals, control, ctx_tree_node2txt, c(list(frequency = frequency), list(...)))
  }
  invisible(ct)
}
