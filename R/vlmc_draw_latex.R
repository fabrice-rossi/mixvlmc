## VLMC case
vlmc_node2latex <- function(label, ct, params) {
  the_node <- stringr::str_c("\\textbf{", label, "}", sep = "")
  if (!is.null(ct[["f_by"]])) {
    if (isTRUE(params$prob)) {
      the_values <- signif(ct[["f_by"]] / sum(ct[["f_by"]]), params$digits)
    } else {
      the_values <- ct[["f_by"]]
    }
    if (isTRUE(params$tabular)) {
      the_node <- tabular_content(the_node, the_values, params)
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
        content = add_fontsize(the_counts, params)
      )
    }
  }
  the_node
}
