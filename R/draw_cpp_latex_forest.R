## C++ based context tree
format_latex_ctx_tree_cpp <- function(tree, ct, vals, control, node2latex) {
  rec_draw_latex_cpp <- function(node, label) {
    res <- stringr::str_c("[", node2latex(label, node, control))
    if (!is.null(node$children)) {
      for (v in seq_along(node$children)) {
        child_idx <- node$children[[v]]
        if (!is.na(child_idx)) {
          child <- tree[[child_idx]]
          res <- c(res, rec_draw_latex_cpp(child, vals[v]))
        }
      }
    }
    res[length(res)] <- stringr::str_c(res[length(res)], "]")
    res
  }
  c(
    start_forest(control),
    rec_draw_latex_cpp(ct, "$\\epsilon$"),
    end_forest(control)
  )
}
