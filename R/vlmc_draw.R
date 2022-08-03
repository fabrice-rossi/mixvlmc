vlmc_node2txt <- function(ct, params) {
  if (is.null(ct[["f_by"]])) {
    NULL
  } else {
    if (isTRUE(params$prob)) {
      stringr::str_c(signif(ct[["f_by"]] / sum(ct[["f_by"]]), 4), collapse = ", ")
    } else {
      stringr::str_c(ct[["f_by"]], collapse = ", ")
    }
  }
}

#' Text based representation of a vlmc
#'
#' @inherit draw
#' @param ct a fitted vlmc.
#' @param prob this parameter controls the display of node level information in
#'   the tree. The default `prob=TRUE` represents the conditional distribution
#'   of the states given the (partial) context associated to the node. Setting
#'   `prob=FALSE` replaces the conditional distribution by the frequency of the
#'   states that follow the context as in [draw.ctx_tree()]. Setting`prob=NULL`
#'   removes all additional information.
#' @examples
#' dts <- sample(c("A", "B", "C"), 500, replace = TRUE)
#' model <- vlmc(dts, alpha = 0.05)
#' draw(model)
#' draw(model, prob = FALSE)
#' draw(model, prob = NULL)
#' @export
draw.vlmc <- function(ct, control = draw_control(), prob = TRUE, ...) {
  if (is.null(prob)) {
    rec_draw(control$root, "", ct, ct$vals, control, NULL, list(...))
  } else {
    rec_draw(control$root, "", ct, ct$vals, control, vlmc_node2txt, c(list(prob = prob), list(...)))
  }
  invisible(ct)
}
