## R based context tree
ctx_tree_node2latex <- function(label, ct, params) {
  the_node <- stringr::str_c("\\textbf{", label, "}", sep = "")
  if (!is.null(ct[["f_by"]])) {
    if (isTRUE(params$tabular)) {
      if (!is.null(params[["frequency"]])) {
        if (params$frequency == "detailed") {
          the_node <- tabular_content(the_node, ct[["f_by"]], params)
        } else if (params$frequency == "total") {
          the_node <- tabular_content(the_node, sum(ct[["f_by"]]), params)
        }
      }
    } else {
      if (!is.null(params[["frequency"]])) {
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
        content = add_fontsize(the_counts, params)
      )
    }
  }
  the_node
}

draw_latex_ctx_tree <- function(ct, vals, node2latex, params) {
  rec_draw_latex <- function(node, label) {
    cat("[", node2latex(label, node, params),
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
  start_forest(params)
  rec_draw_latex(ct, "$\\epsilon$")
  end_forest(params)
}
