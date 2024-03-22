## C++ based context tree
draw_latex_ctx_tree_cpp <- function(tree, ct, vals, node2latex, params) {
  rec_draw_latex_cpp <- function(node, label) {
    cat("[", node2latex(label, node, params),
      sep = ""
    )
    if (!is.null(node$children)) {
      cat("\n")
      for (v in seq_along(node$children)) {
        child_idx <- node$children[[v]]
        if (!is.na(child_idx)) {
          child <- tree[[child_idx]]
          rec_draw_latex_cpp(child, vals[v])
        }
      }
    }
    cat("]\n")
  }
  start_forest(params)
  rec_draw_latex_cpp(ct, "$\\epsilon$")
  end_forest(params)
}
