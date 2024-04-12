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
      the_counts <- NULL
      if (!is.null(control[["frequency"]])) {
        if (control$frequency == "detailed") {
          the_counts <- stringr::str_c(
            "(",
            stringr::str_flatten(ct[["f_by"]],
              collapse = ","
            ),
            ")"
          )
        } else if (control$frequency == "total") {
          the_counts <- stringr::str_c("(", sum(ct[["f_by"]]), ")")
        }
      }
      if (!is.null(the_counts)) {
        the_node <- stringr::str_glue(
          "{{{the_node} {content}}}",
          content = add_fontsize(the_counts, control)
        )
      }
    }
  }
  the_node
}

format_latex_ctx_tree <- function(ct, vals, control, node2latex) {
  rec_draw_latex <- function(node, label) {
    res <- stringr::str_c("[", node2latex(label, node, control))
    if (!is.null(node$children)) {
      for (v in seq_along(node$children)) {
        child <- node$children[[v]]
        if (length(child) > 0) {
          res <- c(res, rec_draw_latex(child, vals[v]))
        }
      }
    }
    res[length(res)] <- stringr::str_c(res[length(res)], "]")
    res
  }
  c(
    start_forest(control),
    rec_draw_latex(ct, "$\\epsilon$"),
    end_forest(control)
  )
}
