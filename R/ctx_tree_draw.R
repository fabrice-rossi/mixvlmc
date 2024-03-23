latex_font_size <- c(
  "tiny", "scriptsize", "footnotesize",
  "small", "normalsize", "large", "Large",
  "LARGE", "huge", "Huge"
)

#' Control parameters for `draw`
#'
#' This function returns a list used to fine tune the [draw()] function
#' behaviour.
#'
#' Parameters are generally specific to the `format` used for [draw()]. If this
#' is the case, the format is given at the end of the parameter description.
#' Some parameters are also specific to some functions inheriting from [draw()].
#'
#' @section Decoration:
#'
#'   The LaTeX format (`"latex"`) can "decorate" the nodes of the context tree
#'   by drawing borders. We support only basic decorations, but in theory all
#'   TikZ possibilities could be used (see the documentation of the [forest
#'   LaTeX package](https://ctan.org/pkg/forest)). Supported decorations:
#'
#' - `"none"`: default, no decoration;
#' - `"rectangle"`: adds a rectangular border to all nodes;
#' - `"circle"`: adds a circular border to all nodes;
#' - `"ellipse"`: adds an ellipsoidal border to all nodes.
#'
#' @param digits numerical parameters and p-values are represented using the
#'   [base::signif()] function, using the number of significant digits specified
#'   with this parameter (defaults to 4).
#' @param root character used for the root node ("ascii").
#' @param first_node characters used for the first child of a node ("ascii").
#' @param next_node characters used for intermediate children of a node ("ascii").
#' @param final_node characters used for the last child of a node ("ascii").
#' @param vbranch characters used to represent a branch in a vertical way
#'   ("ascii").
#' @param hbranch characters used to represent a branch in a horizontal was
#'   ("ascii").
#' @param open_ct characters used to start each node specific text
#'   representation ("ascii").
#' @param close_ct characters used to end each node specific text representation
#'   ("ascii").
#' @param level_sep characters used to separate levels from models in
#'   [draw.covlmc()] ("ascii").
#' @param time_sep characters used to separate temporal blocks in
#'   [draw.covlmc()] ("ascii").
#' @param intercept_sep characters used to the intercept from the other
#'   parameters in [draw.covlmc()] ("ascii").
#' @param open_p_value characters used as opening delimiters for the p value of
#'   a node in [draw.covlmc()] ("ascii").
#' @param close_p_value characters used as closing delimiters for the p value of
#'   a node in [draw.covlmc()] ("ascii").
#' @param orientation specifies the global orientation of the tree, either
#'   "vertical" (default) or "horizontal" ("latex").
#' @param tabular if TRUE (default value), the "latex" format will use tables
#'   for each node, with one row for the state value and other rows for
#'   additional information (such as the conditional probability associated to
#'   the context). Notice that [draw.covlmc()] always uses tables regardless
#'   of the value of `tabular`.
#' @param tab_orientation specifies the way the models are represented when used
#'   by [draw.covlmc()] ("latex"). The default value is `"vertical"`: this is
#'   well adapted to models with long covariate dependencies (see
#'   `covariate_depth()`). The other possible value is `"horizontal"`.
#' @param decoration specifies node decoration in the "latex" format, see
#'   details.
#' @param fontsize font size for the state names in the "latex" format (using
#'   latex standard font size, default to `"normalsize"`).
#' @param prob_fontsize font size for the context counts, probabilities or
#'   models in the "latex" format (using latex standard font size, defaults to
#'   `"small"`).
#' @returns a list
#' @export
#'
#' @examples
#' draw_control(open_ct = "[", close_ct = "]")
draw_control <- function(digits = 4,
                         root = "*",
                         first_node = "+",
                         next_node = "'",
                         final_node = "'",
                         vbranch = "|",
                         hbranch = "--",
                         open_ct = "(",
                         close_ct = ")",
                         level_sep = " ~ ",
                         time_sep = " | ",
                         intercept_sep = " & ",
                         open_p_value = "<",
                         close_p_value = ">",
                         orientation = c("vertical", "horizontal"),
                         tabular = TRUE,
                         tab_orientation = c("vertical", "horizontal"),
                         decoration = c("none", "rectangle", "circle", "ellipse"),
                         fontsize = "normalsize",
                         prob_fontsize = "small") {
  list(
    digits = digits,
    root = root,
    first_node = first_node,
    next_node = next_node,
    final_node = final_node,
    vbranch = vbranch,
    hbranch = hbranch,
    open_ct = open_ct,
    close_ct = close_ct,
    level_sep = level_sep,
    time_sep = time_sep,
    intercept_sep = intercept_sep,
    open_p_value = open_p_value,
    close_p_value = close_p_value,
    orientation = match.arg(orientation),
    tabular = tabular,
    tab_orientation = match.arg(tab_orientation),
    decoration = match.arg(decoration),
    fontsize = match.arg(fontsize, choices = latex_font_size),
    prob_fontsize = match.arg(prob_fontsize, choices = latex_font_size)
  )
}

cat_with_prefix <- function(label, prefix, node_txt, control) {
  node_txt_lines <- unlist(stringr::str_split(node_txt, "\n"))
  cat(" ", control$open_ct, node_txt_lines[1], sep = "")
  if (length(node_txt_lines) > 1) {
    local_prefix <- utf8_pad(
      prefix,
      cli::utf8_nchar(label, "width") + 1 + cli::utf8_nchar(control$open_ct, "width"),
      "right"
    )
    for (k in seq_along(node_txt_lines)[-1]) {
      cat("\n", local_prefix, node_txt_lines[k], sep = "")
    }
  }
  cat(control$close_ct)
}

rec_draw <- function(label, prefix, ct, vals, control, node2txt) {
  cat(label)
  if (!is.null(node2txt)) {
    node_txt <- node2txt(ct, control)
    if (!is.null(node_txt)) {
      cat_with_prefix(label, prefix, node_txt, control)
    }
  }
  cat("\n")
  if (!is.null(ct$children)) {
    nst <- nb_sub_tree(ct)
    if (nst > 1) {
      c_symbol <- control$first_node
    } else {
      c_symbol <- control$final_node
    }
    idx <- 1
    nb_nodes <- sum(sapply(ct$children, \(x) length(x) > 0))
    for (v in seq_along(ct$children)) {
      child <- ct$children[[v]]
      if (length(child) > 0) {
        c_prelabel <- stringr::str_c(c_symbol, control$hbranch, " ")
        if (idx < nst) {
          c_prefix <- control$vbranch
        } else {
          c_prefix <- stringr::str_pad("", cli::utf8_nchar(control$vbranch, "width"))
        }
        c_prefix <- utf8_pad(c_prefix, cli::utf8_nchar(c_prelabel, "width"), "right")
        ## recursive call
        rec_draw(
          stringr::str_c(prefix, c_prelabel, vals[v]),
          stringr::str_c(prefix, c_prefix), child, vals, control, node2txt
        )
        ## prepare for next child
        idx <- idx + 1
        if (idx == nb_nodes) {
          c_symbol <- control$final_node
        } else {
          c_symbol <- control$next_node
        }
      }
    }
  }
}

#' Text based representation of a context tree
#'
#' This function 'draws' a context tree as a text.
#'
#' The function uses different text based formats (plain "ascii art" and LaTeX)
#' to represent the context tree. Fine tuning of the representation can be done
#' via the [draw_control()] function.
#'
#' In addition to the structure of the context tree, `draw()` can represent
#' information attached to the nodes (contexts and partial contexts). This is
#' controlled by additional parameters depending on the type of the context
#' tree. In general, parameters given directly to `draw()` specify *what*
#' information is represented while details on *how* this representation is made
#' can be controlled via the `control` parameter and the associated
#' [draw_control()] function.
#'
#' @param ct a context tree.
#' @param format a character string that specifies the output format of the
#'   function. Possible values are `ascii` (default) and `latex`. See details.
#' @param control a list of low level control parameters of the text
#'   representation. See details and [draw_control()].
#' @param ... additional arguments for draw.
#' @returns the context tree (invisibly).
#' @examples
#' dts <- sample(c(0, 1), 100, replace = TRUE)
#' ctree <- ctx_tree(dts, min_size = 10, max_depth = 2)
#' draw(ctree)
#' dts_c <- sample(c("A", "B", "CD"), 100, replace = TRUE)
#' ctree_c <- ctx_tree(dts_c, min_size = 10, max_depth = 2)
#' draw(ctree_c, control = draw_control(root = "x"))
#' ## LaTeX output
#' draw(ctree_c, "latex")
#' @section Format:
#'
#'   The `format` parameter specifies the format used for the textual output.
#'   With the default value `ascii` the output is produced in "ascii art" using
#'   by default only ascii characters (notice that `draw_control()` can be used
#'   to specified non ascii characters, but this is discouraged).
#'
#'   With the `latex` value, the output is produced in LaTeX, leveraging the
#'   [forest](https://ctan.org/pkg/forest) Latex package (see
#'   <https://ctan.org/pkg/forest>). Each call to `draw()` produces a full
#'   `forest` LaTeX environment. This can be included as is in a LaTeX document,
#'   provided the `forest` package is loaded in the preamble of the document.
#'   The LaTeX output is sanitized to avoid potential problems induced by
#'   special characters in the names of the states of the context tree.
#'
#' @export
draw <- function(ct, format, control = draw_control(), ...) {
  UseMethod("draw")
  invisible(ct)
}

ctx_tree_node2txt <- function(ct, control) {
  if (is.null(ct[["f_by"]])) {
    NULL
  } else {
    if (!is.null(control[["frequency"]])) {
      if (control$frequency == "detailed") {
        stringr::str_c(ct[["f_by"]], collapse = ",")
      } else if (control$frequency == "total") {
        as.character(sum(ct[["f_by"]]))
      } else {
        NULL
      }
    } else {
      NULL
    }
  }
}

#' @inherit draw
#' @param frequency this parameter controls the display of node level
#'   information in the tree. The default `NULL` value does not include
#'   anything. Setting `frequency` to `"total"` includes the frequency of the
#'   (partial) context of the node, while `"detailed"` includes the frequency of
#'   the states that follow the context (as in [contexts.ctx_tree()]).
#' @examples
#' dts_c <- sample(c("A", "B", "CD"), 100, replace = TRUE)
#' ctree_c <- ctx_tree(dts_c, min_size = 10, max_depth = 2)
#' draw(ctree_c, frequency = "total")
#' draw(ctree_c, frequency = "detailed")
#' ## LaTeX output
#' draw(ctree_c, "latex", frequency = "detailed")
#' dts_c <- sample(c("A$", "_{B", "{C}_{D}"), 100, replace = TRUE)
#' ctree_c <- ctx_tree(dts_c, min_size = 10, max_depth = 2)
#' ## the LaTeX output is sanitized
#' draw(ctree_c, "latex", frequency = "detailed")
#' @export
draw.ctx_tree <- function(ct, format, control = draw_control(),
                          frequency = NULL, ...) {
  if (rlang::is_missing(format)) {
    format <- "ascii"
  } else {
    format <- match.arg(format, c("ascii", "latex"))
  }
  if (format == "ascii") {
    if (is.null(frequency)) {
      rec_draw(control$root, "", ct, ct$vals, c(control, list(...)), NULL)
    } else {
      frequency <- match.arg(frequency, c("total", "detailed"))
      rec_draw(
        control$root, "", ct, ct$vals,
        c(control, list(frequency = frequency), list(...)), ctx_tree_node2txt
      )
    }
  } else if (format == "latex") {
    draw_latex_ctx_tree(
      ct, xtable::sanitize(ct$vals, "latex"),
      c(control, list(...), list(frequency = frequency)),
      ctx_tree_node2latex
    )
  }
  invisible(ct)
}
