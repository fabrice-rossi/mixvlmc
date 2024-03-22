#' @export
#' @rdname draw.vlmc
draw.vlmc_cpp <- function(ct, format, control = draw_control(), prob = TRUE, ...) {
  if (rlang::is_missing(format)) {
    format <- "ascii"
  } else {
    format <- match.arg(format, c("ascii", "latex"))
  }
  restore_model(ct)
  ct_r <- ct$root$representation()
  if (format == "ascii") {
    if (is.null(prob)) {
      rec_draw_cpp(control$root, "", ct_r, ct_r[[1]], ct$vals, control, NULL, list(...))
    } else {
      rec_draw_cpp(
        control$root, "", ct_r, ct_r[[1]], ct$vals, control, vlmc_node2txt,
        c(list(prob = prob, digits = control$digits), list(...))
      )
    }
  } else if (format == "latex") {
    if (is.null(prob)) {
      draw_latex_ctx_tree_cpp(
        ct_r, ct_r[[1]], xtable::sanitize(ct$vals, "latex"),
        ctx_tree_node2latex,
        c(control, list(...))
      )
    } else {
      draw_latex_ctx_tree_cpp(
        ct_r, ct_r[[1]], xtable::sanitize(ct$vals, "latex"),
        vlmc_node2latex,
        c(control, list(prob = prob), list(...))
      )
    }
  }
  invisible(ct)
}
