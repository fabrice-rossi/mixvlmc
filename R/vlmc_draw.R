vlmc_node2txt <- function(ct, ...) {
  params <- list(...)
  if (is.null(ct[["f_by"]])) {
    NULL
  } else {
    if (isTRUE(params$prob)) {
      stringr::str_c(signif(ct[["f_by"]] / sum(ct[["f_by"]]), 4), collapse = ", ")
    } else {
      stringr::str_c(ct[["f_by"]], collapse = ",")
    }
  }
}

#' Text based representation of a vlmc
#'
#' @inherit draw
#' @param ct a fitted vlmc.
#' @param node2txt an optional function called on each node to render it to a
#'   text representation. Defaults to the frequencies of the states given the
#'   context. Adding `prob=TRUE` as an additional parameter normalizes the
#'   frequencies to probabilities.
#' @examples
#' dts <- sample(c("A", "B", "C"), 500, replace = TRUE)
#' model <- vlmc(dts, alpha = 0.05)
#' draw(model)
#' draw(model, prob = TRUE)
#' draw(model, node2txt = NULL)
#' @export
draw.vlmc <- function(ct, control = draw_control(), node2txt = vlmc_node2txt, ...) {
  rec_draw(control$root, "", ct, ct$vals, control, node2txt, ...)
  invisible(ct)
}
