draw_vlmc_node <- function(node, probs = FALSE) {
  if (probs) {
    paste(node$f_by / sum(node$f_by), collapse = ", ")
  } else {
    paste(node$f_by, collapse = ", ")
  }
}

#' @export
draw.vlmc <- function(ct, node2txt = NULL, ...) {
  if (is.null(node2txt)) {
    NextMethod(node2txt = draw_vlmc_node, ...)
  } else {
    NextMethod(node2txt = node2txt, ...)
  }
  invisible(ct)
}
