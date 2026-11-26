#' Format a context tree (C++ backend)
#'
#' This is a recursive function.
#'
#' @param label the label of the current node (initially set to the root)
#' @param prefix the current prefix that should be added to all the lines
#'   produced for the current node
#' @param tree the full tree (R representation)
#' @param ct the current node
#' @param vals printable values for the states
#' @param control list of control parameters
#' @param node2txt a function that maps the content of a node to a string
#'   representation possibly made of multiple lines (as a list of lines). The
#'   lines should be aligned but do not have to care about the prefix
#'
#' @return the formatted context tree as a list of character vectors (one per
#'   line)
#' @noRd
format_ctx_tree_cpp <- function(label, prefix, tree, ct, vals, control, node2txt) {
  if (!is.null(node2txt)) {
    node_txt <- node2txt(ct, vals, control)
  } else {
    node_txt <- NULL
  }
  full_node <- format_node(label, prefix, node_txt, control)
  if (!is.null(ct$children)) {
    nst <- sum(!is.na(ct$children))
    if (nst > 1) {
      c_symbol <- control$first_node
    } else {
      c_symbol <- control$final_node
    }
    idx <- 1
    result <- full_node
    for (v in seq_along(ct$children)) {
      child <- ct$children[[v]]
      child_idx <- ct$children[[v]]
      if (!is.na(child_idx)) {
        child <- tree[[child_idx]]
        c_prelabel <- stringr::str_c(c_symbol, control$hbranch, " ")
        if (idx < nst) {
          c_prefix <- control$vbranch
        } else {
          c_prefix <- stringr::str_pad("", cli::utf8_nchar(control$vbranch, "width"))
        }
        c_prefix <- utf8_pad(c_prefix, cli::utf8_nchar(c_prelabel, "width"), "right")
        ## recursive call
        sub_tree <-
          format_ctx_tree_cpp(
            stringr::str_c(prefix, c_prelabel, vals[v]),
            stringr::str_c(prefix, c_prefix),
            tree, child, vals, control, node2txt
          )
        result <- c(result, sub_tree)
        ## prepare for next child
        idx <- idx + 1
        if (idx == nst) {
          c_symbol <- control$final_node
        } else {
          c_symbol <- control$next_node
        }
      }
    }
    result
  } else {
    full_node
  }
}


#' @export
#' @rdname draw.ctx_tree
draw.ctx_tree_cpp <- function(ct, format, control = draw_control(),
                              frequency = NULL, ...) {
  rlang::check_dots_empty()
  format_control <- draw_format_control(format, control)
  format <- format_control$format
  control <- format_control$control
  restore_model(ct)
  ct_r <- ct$root$representation()
  if (format == "text") {
    if (is.null(frequency)) {
      res <- format_ctx_tree_cpp(
        control$root, "", ct_r, ct_r[[1]], ct$vals, control,
        NULL
      )
    } else {
      frequency <- match.arg(frequency, c("total", "detailed"))
      res <- format_ctx_tree_cpp(
        control$root, "", ct_r, ct_r[[1]], ct$vals,
        c(control, list(frequency = frequency)),
        ctx_tree_node2txt
      )
    }
  } else if (format == "latex") {
    res <- format_latex_ctx_tree_cpp(
      ct_r, ct_r[[1]], xtable::sanitize(ct$vals, "latex"),
      c(control, list(frequency = frequency)),
      ctx_tree_node2latex
    )
  }
  new_mixvlmc_draw(res, format)
}
