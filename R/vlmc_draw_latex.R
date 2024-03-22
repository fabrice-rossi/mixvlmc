## VLMC case
vlmc_node2latex <- function(label, ct, control) {
  the_node <- stringr::str_c("\\textbf{", label, "}", sep = "")
  if (!is.null(ct[["f_by"]])) {
    if (isTRUE(control$prob)) {
      the_values <- signif(ct[["f_by"]] / sum(ct[["f_by"]]), control$digits)
    } else {
      the_values <- ct[["f_by"]]
    }
    if (isTRUE(control$tabular)) {
      the_node <- tabular_content(the_node, the_values, control)
    } else {
      the_counts <- stringr::str_c(
        "(",
        stringr::str_flatten(the_values,
          collapse = ","
        ),
        ")"
      )
      the_node <- stringr::str_glue(
        "{{{the_node} {content}}}",
        content = add_fontsize(the_counts, control)
      )
    }
  }
  the_node
}
