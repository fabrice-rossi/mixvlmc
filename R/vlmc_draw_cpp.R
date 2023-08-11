#' @export
draw.vlmc_cpp <- function(ct, control = draw_control(), prob = TRUE, ...) {
  if (extptr_is_null(ct$root$.pointer)) {
    stop("Missing C++ representation!\nThis object was probably restored from a saved object.\n")
  }
  ct_r <- ct$root$representation()
  if (is.null(prob)) {
    rec_draw_cpp(control$root, "", ct_r, ct_r[[1]], ct$vals, control, NULL, list(...))
  } else {
    rec_draw_cpp(control$root, "", ct_r, ct_r[[1]], ct$vals, control, vlmc_node2txt, c(list(prob = prob), list(...)))
  }
  invisible(ct)
}