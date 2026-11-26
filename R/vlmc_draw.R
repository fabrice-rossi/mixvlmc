vlmc_node2txt <- function(ct, vals, control) {
  if (is.null(ct[["f_by"]])) {
    NULL
  } else {
    if (isTRUE(control$prob)) {
      stringr::str_c(signif(ct[["f_by"]] / sum(ct[["f_by"]]), control$digits),
        collapse = ", "
      )
    } else {
      stringr::str_c(ct[["f_by"]], collapse = ", ")
    }
  }
}

#' Text based representation of a VLMC
#'
#' @inherit draw
#' @param ct a fitted VLMC.
#' @param prob this parameter controls the display of node level information in
#'   the tree. The default `prob=TRUE` represents the conditional distribution
#'   of the states given the (partial) context associated with the node.
#'   Setting `prob=FALSE` replaces the conditional distribution by the frequency
#'   of the states that follow the context as in [draw.ctx_tree()]. Setting
#'   `prob=NULL` removes all additional information.
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' rdts <- cut(pc$active_power,
#'   breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1)))
#' )
#' model <- vlmc(rdts, max_depth = 3)
#' draw(model)
#' draw(model, prob = FALSE)
#' draw(model, prob = NULL)
#' ## LaTeX output
#' draw(model, "latex")
#' @param ... not used (customisation options are given with [draw_control()]).
#' @export
draw.vlmc <- function(ct, format, control = draw_control(), prob = TRUE, ...) {
  rlang::check_dots_empty()
  format_control <- draw_format_control(format, control)
  format <- format_control$format
  control <- format_control$control
  if (format == "text") {
    if (is.null(prob)) {
      res <- format_ctx_tree(
        ct, control,
        NULL
      )
    } else {
      res <- format_ctx_tree(
        ct,
        c(control, list(prob = prob)),
        vlmc_node2txt
      )
    }
  } else if (format == "latex") {
    if (is.null(prob)) {
      res <- format_latex_ctx_tree(
        ct, xtable::sanitize(ct$vals, "latex"),
        control,
        ctx_tree_node2latex
      )
    } else {
      res <- format_latex_ctx_tree(
        ct, xtable::sanitize(ct$vals, "latex"),
        c(control, list(prob = prob)),
        vlmc_node2latex
      )
    }
  }
  new_mixvlmc_draw(res, format)
}
