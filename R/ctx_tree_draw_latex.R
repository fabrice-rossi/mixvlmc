## R based context tree
ctx_tree_node2latex <- function(label, ct, control) {
  the_node <- stringr::str_c("\\textbf{", label, "}", sep = "")
  if (!is.null(ct[["f_by"]])) {
    if (isTRUE(control$tabular)) {
      if (!is.null(control[["frequency"]])) {
        if (control$frequency == "detailed") {
          the_node <- tabular_content(the_node, ct[["f_by"]], control)
        } else if (control$frequency == "total") {
          the_node <- tabular_content(the_node, sum(ct[["f_by"]]), control)
        }
      }
    } else {
      if (!is.null(control[["frequency"]])) {
        the_counts <- stringr::str_c(
          "(",
          stringr::str_flatten(ct[["f_by"]],
            collapse = ","
          ),
          ")"
        )
      } else {
        the_counts <- stringr::str_c("(", sum(ct[["f_by"]]), ")")
      }
      the_node <- stringr::str_glue(
        "{{{the_node} {content}}}",
        content = add_fontsize(the_counts, control)
      )
    }
  }
  the_node
}

draw_latex_ctx_tree <- function(ct, vals, control, node2latex) {
  rec_draw_latex <- function(node, label) {
    cat("[", node2latex(label, node, control),
      sep = ""
    )
    if (!is.null(node$children)) {
      cat("\n")
      for (v in seq_along(node$children)) {
        child <- node$children[[v]]
        if (length(child) > 0) {
          rec_draw_latex(child, vals[v])
        }
      }
    }
    cat("]\n")
  }
  start_forest(control)
  rec_draw_latex(ct, "$\\epsilon$")
  end_forest(control)
}
