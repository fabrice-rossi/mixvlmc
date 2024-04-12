#' @export
#' @rdname draw.vlmc
draw.vlmc_cpp <- function(ct, format, control = draw_control(), prob = TRUE, ...) {
  format_control <- draw_format_control(format, control)
  format <- format_control$format
  control <- format_control$control
  restore_model(ct)
  ct_r <- ct$root$representation()
  if (format == "text") {
    if (is.null(prob)) {
      res <- format_ctx_tree_cpp(
        control$root, "", ct_r, ct_r[[1]], ct$vals,
        c(control, list(...)), NULL
      )
    } else {
      res <- format_ctx_tree_cpp(
        control$root, "", ct_r, ct_r[[1]], ct$vals,
        c(control, list(prob = prob), list(...)),
        vlmc_node2txt
      )
    }
  } else if (format == "latex") {
    if (is.null(prob)) {
      res <- format_latex_ctx_tree_cpp(
        ct_r, ct_r[[1]], xtable::sanitize(ct$vals, "latex"),
        c(control, list(...)), ctx_tree_node2latex
      )
    } else {
      res <- format_latex_ctx_tree_cpp(
        ct_r, ct_r[[1]], xtable::sanitize(ct$vals, "latex"),
        c(control, list(prob = prob), list(...)), vlmc_node2latex
      )
    }
  }
  writeLines(res)
  invisible(ct)
}
