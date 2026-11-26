latex_font_size <- c(
  "tiny", "scriptsize", "footnotesize",
  "small", "normalsize", "large", "Large",
  "LARGE", "huge", "Huge"
)

#' ASCII character set for context tree text representation
#'
#' This function returns a list of ASCII characters used to fine-tune the
#' behaviour of the [draw()] function when it is used with `format="text"`. It
#' can be used as is or customised using its parameters, and is generally
#' passed to [draw_control()] via its `charset` parameter.
#'
#' @param root character used for the root node.
#' @param first_node characters used for the first child of a node.
#' @param next_node characters used for intermediate children of a node.
#' @param final_node characters used for the last child of a node.
#' @param vbranch characters used to represent a branch in a vertical way.
#' @param hbranch characters used to represent a branch in a horizontal way.
#' @param open_ct characters used to start the node specific text
#'   representation of each node.
#' @param close_ct characters used to end the node specific text
#'   representation of each node.
#' @param level_sep characters used to separate levels from models in
#'   [draw.covlmc()].
#' @param time_sep characters used to separate temporal blocks in
#'   [draw.covlmc()].
#' @param intercept characters used to represent the intercept in
#'   [draw.covlmc()].
#' @param intercept_sep characters used to separate the intercept from the other
#'   parameters in [draw.covlmc()].
#' @param open_p_value characters used as opening delimiters for the p-value of
#'   a node in [draw.covlmc()].
#' @param close_p_value characters used as closing delimiters for the p-value of
#'   a node in [draw.covlmc()].
#' @param open_model characters used as opening delimiters for the
#'   representation of a model in [draw.covlmc()].
#' @param close_model characters used as closing delimiters for the
#'   representation of a model in [draw.covlmc()].
#' @returns a list of character strings, one per parameter.
#' @seealso [draw()], [draw_control()], [charset_utf8()].
#' @export
#' @examples
#' charset_ascii(root = "x")
#' rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
#' rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 2)
#' draw(rdts_ctree, control = draw_control(charset = charset_ascii(root = "x")))
charset_ascii <- function(root = "*",
                          first_node = "+",
                          next_node = "+",
                          final_node = "'",
                          vbranch = "|",
                          hbranch = "--",
                          open_ct = "(",
                          close_ct = ")",
                          level_sep = " ~ ",
                          time_sep = " | ",
                          intercept = "(I)",
                          intercept_sep = " & ",
                          open_p_value = "<",
                          close_p_value = ">",
                          open_model = "[",
                          close_model = "]") {
  list(
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
    intercept = intercept,
    intercept_sep = intercept_sep,
    open_p_value = open_p_value,
    close_p_value = close_p_value,
    open_model = open_model,
    close_model = close_model
  )
}

#' UTF-8 character set for context tree text representation
#'
#' This function returns a list of UTF-8 characters and symbols used to
#' fine-tune the behaviour of the [draw()] function when it is used with
#' `format="text"`. It can be used as is or customised using its parameters,
#' and is generally passed to [draw_control()] via its `charset` parameter.
#'
#' @param root character used for the root node.
#' @param first_node characters used for the first child of a node.
#' @param next_node characters used for intermediate children of a node.
#' @param final_node characters used for the last child of a node.
#' @param vbranch characters used to represent a branch in a vertical way.
#' @param hbranch characters used to represent a branch in a horizontal way.
#' @param open_ct characters used to start the node specific text
#'   representation of each node.
#' @param close_ct characters used to end the node specific text
#'   representation of each node.
#' @param level_sep characters used to separate levels from models in
#'   [draw.covlmc()].
#' @param time_sep characters used to separate temporal blocks in
#'   [draw.covlmc()].
#' @param intercept characters used to represent the intercept in
#'   [draw.covlmc()].
#' @param intercept_sep characters used to separate the intercept from the other
#'   parameters in [draw.covlmc()].
#' @param open_p_value characters used as opening delimiters for the p-value of
#'   a node in [draw.covlmc()].
#' @param close_p_value characters used as closing delimiters for the p-value of
#'   a node in [draw.covlmc()].
#' @param open_model characters used as opening delimiters for the
#'   representation of a model in [draw.covlmc()].
#' @param close_model characters used as closing delimiters for the
#'   representation of a model in [draw.covlmc()].
#' @returns a list of character strings, one per parameter.
#' @seealso [draw()], [draw_control()], [charset_ascii()].
#' @export
#' @examples
#' charset_utf8(root = "\u27E1")
#' rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
#' rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 2)
#' draw(rdts_ctree,
#'   control = draw_control(charset = charset_utf8(root = "\u27E1"))
#' )
charset_utf8 <- function(root = "\u25AA", # ▪
                         first_node = "\u251C", # ├
                         next_node = "\u251C", # ├ ,
                         final_node = "\u2514", # └
                         vbranch = "\u2502", # │
                         hbranch = "\u2500", # ─
                         open_ct = "(",
                         close_ct = ")",
                         level_sep = " ~ ",
                         time_sep = " \u205E ", # ⁞
                         #    time_sep = " \u2022 ", # •
                         #    time_sep = " \u250A ", # ┆
                         intercept = "(I)", #
                         intercept_sep = " \u2022 ", # •
                         open_p_value = "\u2039", # ‹
                         close_p_value = "\u203A", # ›
                         open_model = "[",
                         close_model = "]") {
  list(
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
    intercept = intercept,
    intercept_sep = intercept_sep,
    open_p_value = open_p_value,
    close_p_value = close_p_value,
    open_model = open_model,
    close_model = close_model
  )
}

#' Control parameters for `draw()`
#'
#' This function returns a list used to fine-tune the behaviour of the [draw()]
#' function.
#'
#' Parameters are generally specific to the `format` used for [draw()]. If this
#' is the case, the format is given at the end of the parameter description.
#' Some parameters are also specific to some methods of [draw()].
#'
#' @param digits numerical parameters, probabilities and p-values are
#'   represented using the [base::signif()] function, using the number of
#'   significant digits specified with this parameter (defaults to 4).
#' @param charset specifies the characters used for the "ascii art"
#'   representation when the format is "text", see the "Charset" section.
#' @param orientation specifies the global orientation of the tree, either
#'   "vertical" (default) or "horizontal" ("latex").
#' @param tabular if `TRUE` (default value), the "latex" format will use tables
#'   for each node, with one row for the state value and other rows for
#'   additional information (such as the conditional probability associated
#'   with the context). Notice that [draw.covlmc()] always uses tables
#'   regardless of the value of `tabular`.
#' @param tab_orientation specifies the way the models are represented when used
#'   by [draw.covlmc()] ("latex"). The default value is `"vertical"`: this is
#'   well adapted to models with long covariate dependencies (see
#'   [covariate_depth()]). The other possible value is `"horizontal"`.
#' @param decoration specifies node decoration in the "latex" format, see the
#'   "Decoration" section.
#' @param fontsize font size for the state names in the "latex" format (using
#'   LaTeX standard font sizes such as `"small"` or `"large"`, defaults to
#'   `"normalsize"`).
#' @param prob_fontsize font size for the context counts, probabilities or
#'   models in the "latex" format (using LaTeX standard font sizes, defaults to
#'   `"small"`).
#' @returns a list of control parameters.
#' @export
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
#' @section Charset:
#'
#'   The "ascii art" format (`"text"`) uses a collection of characters to
#'   display a context tree. The default collection is specified by the
#'   package option `"mixvlmc.charset"` and is used when `charset=NULL` (default
#'   value). If `charset` is set to a character value, this value is used to
#'   select the collection in the same way that `"mixvlmc.charset"` specifies
#'   it:
#'
#'   - `"ascii"`: the collection uses only standard ASCII characters and
#'     should be compatible with all environments;
#'   - `"utf8"`: the collection uses UTF-8 symbols and needs a compatible
#'     display.
#'
#'   Finally, `charset` can be a user supplied list of characters such as the
#'   ones returned by [charset_ascii()] and [charset_utf8()].
#'
#' @seealso [draw()], [charset_ascii()] and [charset_utf8()].
#' @examples
#' draw_control(digits = 2, tabular = FALSE)
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' rdts <- cut(pc$active_power,
#'   breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1)))
#' )
#' model <- vlmc(rdts, max_depth = 2)
#' draw(model, control = draw_control(digits = 2, charset = "ascii"))
#' draw(model, "latex",
#'   control = draw_control(
#'     orientation = "horizontal",
#'     decoration = "rectangle"
#'   )
#' )
draw_control <- function(digits = 4,
                         charset = NULL,
                         orientation = c("vertical", "horizontal"),
                         tabular = TRUE,
                         tab_orientation = c("vertical", "horizontal"),
                         decoration = c("none", "rectangle", "circle", "ellipse"),
                         fontsize = "normalsize",
                         prob_fontsize = "small") {
  if (is.null(charset)) {
    charset <- options("mixvlmc.charset")[[1]]
  }
  if (is.character(charset)) {
    charset <- match.arg(charset, choices = c("ascii", "utf8"))
    if (charset == "ascii") {
      charset <- charset_ascii()
    } else {
      charset <- charset_utf8()
    }
  }
  c(
    list(
      digits = digits,
      orientation = match.arg(orientation),
      tabular = tabular,
      tab_orientation = match.arg(tab_orientation),
      decoration = match.arg(decoration),
      fontsize = match.arg(fontsize, choices = latex_font_size),
      prob_fontsize = match.arg(prob_fontsize, choices = latex_font_size)
    ),
    charset
  )
}

#' Format the content of a node
#'
#' @param label the label of the node with its prefix
#' @param prefix the prefix to use for subsequent lines
#' @param node_txt the text of the node with one or several lines (as a
#'   character vector)
#' @param control list of control parameters
#'
#' @return the formatted content, a character vector (each string is a line)
#' @noRd
format_node <- function(label, prefix, node_txt, control) {
  if (is.null(node_txt) || length(node_txt) == 0) {
    label
  } else {
    first_prefix <- stringr::str_c(prefix, label, " ", control$open_ct, sep = "")
    result <- rep("", length(node_txt))
    result[1] <- stringr::str_c(label, " ", control$open_ct, node_txt[1])
    if (length(node_txt) > 1) {
      local_prefix <- utf8_pad(
        prefix,
        cli::utf8_nchar(stringr::str_c(label, " ", control$open_ct), "width"), "right"
      )
      result[2:length(node_txt)] <- stringr::str_c(local_prefix, node_txt[2:length(node_txt)])
    }
    result[length(node_txt)] <- stringr::str_c(
      result[length(node_txt)],
      control$close_ct
    )
    result
  }
}

#' Format a context tree
#' @noRd
format_ctx_tree <- function(ct, control, node2txt) {
  rec_format_ctx_tree(control$root, "", ct, ct$vals, control, node2txt)
}

#' Format a context tree
#'
#' This is a recursive function.
#'
#' @param label the label of the current node (initially set to the root)
#' @param prefix the current prefix that should be added to all the lines
#'   produced for the current node
#' @param ct the current node
#' @param vals printable values for the states
#' @param control list of control parameters
#' @param node2txt a function that maps the content of a node to a string
#'   representation possibly made of multiple lines (as character vector). The
#'   lines should be aligned but do not have to care about the prefix
#'
#' @return the formatted context tree as a list of character vectors (one per
#'   line)
#' @noRd
rec_format_ctx_tree <- function(label, prefix, ct, vals, control, node2txt) {
  if (!is.null(node2txt)) {
    node_txt <- node2txt(ct, vals, control)
  } else {
    node_txt <- NULL
  }
  full_node <- format_node(label, prefix, node_txt, control)
  if (!is.null(ct$children)) {
    nst <- nb_sub_tree(ct)
    idx <- 1
    nb_nodes <- sum(sapply(ct$children, \(x) length(x) > 0))
    if (is.null(ct[["merged_model"]])) {
      active_children <- seq_along(ct$children)
    } else {
      active_children <- setdiff(seq_along(ct$children), ct$merged)
      nb_nodes <- nb_nodes + 1
    }
    if (nb_nodes > 1) {
      c_symbol <- control$first_node
    } else {
      c_symbol <- control$final_node
    }
    result <- full_node
    for (v in active_children) {
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
        sub_tree <-
          rec_format_ctx_tree(
            stringr::str_c(prefix, c_prelabel, vals[v]),
            stringr::str_c(prefix, c_prefix), child, vals, control, node2txt
          )
        result <- c(result, sub_tree)
        ## prepare for next child
        idx <- idx + 1
        if (idx == nb_nodes) {
          c_symbol <- control$final_node
        } else {
          c_symbol <- control$next_node
        }
      }
    }
    if (!is.null(ct[["merged_model"]])) {
      the_merged_vals <- stringr::str_c(vals[ct$merged], collapse = ", ")
      c_prelabel <- stringr::str_c(control$final_node, control$hbranch, " ")
      c_prefix <- stringr::str_pad("", cli::utf8_nchar(control$vbranch, "width"))
      c_prefix <- utf8_pad(c_prefix, cli::utf8_nchar(c_prelabel, "width"), "right")
      c_label <- stringr::str_c(prefix, c_prelabel, the_merged_vals)
      c_prefix <- stringr::str_c(prefix, c_prefix)
      if (!is.null(node2txt)) {
        mm <- ct[["merged_model"]]
        node_txt <- node2txt(list(
          model = mm, model_probs = mm$model_probs,
          model_levels = mm$model_levels,
          f_by = rowSums(sapply(ct$children[ct$merged], \(x) x$f_by))
        ), vals, control)
      } else {
        node_txt <- NULL
      }
      pre_result <- format_node(c_label, c_prefix, node_txt, control)
      result <- c(result, pre_result)
    }
    result
  } else {
    full_node
  }
}


#' Text based representation of a context tree
#'
#' This function 'draws' a context tree as a text.
#'
#' The function uses different text based formats (plain "ascii art" and LaTeX)
#' to represent the context tree. Fine-tuning of the representation can be done
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
#'   function. Possible values are `"text"` (default) and `"latex"`. See details.
#' @param control a list of low level control parameters of the
#'   representation. See details and [draw_control()].
#' @param ... additional arguments for the methods of `draw()`.
#' @returns an object of class `mixvlmc_draw` that contains the representation
#'   of the context tree. As for other R objects, it is printed automatically
#'   when it is not assigned to a variable at top level (for instance when
#'   `draw(model)` is called in the console), but not inside a function or a
#'   loop where it must be printed explicitly with `print(draw(model))` (see
#'   [print.mixvlmc_draw()]).
#' @examples
#' set.seed(0)
#' rdts <- sample(c(0, 1), 100, replace = TRUE)
#' ctree <- ctx_tree(rdts, min_size = 10, max_depth = 2)
#' draw(ctree)
#' rdts_c <- sample(c("A", "B", "CD"), 100, replace = TRUE)
#' ctree_c <- ctx_tree(rdts_c, min_size = 10, max_depth = 2)
#' draw(ctree_c, control = draw_control(digits = 2))
#' ## LaTeX output
#' draw(ctree_c, "latex")
#' @section Format:
#'
#'   The `format` parameter specifies the format used for the textual output.
#'   With the default value `"text"` the output is produced in "ascii art". The
#'   characters used to represent the tree are given by the `charset` parameter
#'   of [draw_control()]. By default, they are UTF-8 characters when the output
#'   supports them and ASCII characters otherwise (see the `mixvlmc.charset`
#'   option in [mixvlmc-package]).
#'
#'   With the `"latex"` value, the output is produced in LaTeX, leveraging the
#'   [forest](https://ctan.org/pkg/forest) LaTeX package (see
#'   <https://ctan.org/pkg/forest>). Each call to `draw()` produces a full
#'   `forest` LaTeX environment. This can be included as is in a LaTeX document,
#'   provided the `forest` package is loaded in the preamble of the document.
#'   The LaTeX output is sanitised to avoid potential problems induced by
#'   special characters in the names of the states of the context tree.
#'
#'   In a knitr document (R Markdown, Quarto, ...) that produces LaTeX (e.g. a
#'   PDF output), the LaTeX representation is included as is in the document
#'   and the `forest` package is added automatically to its preamble (unless
#'   the document loads it, possibly with options): calling
#'   `draw(model, "latex")` in a chunk is enough to render the tree (see
#'   [print.mixvlmc_draw()]).
#'
#' @export
draw <- function(ct, format, control = draw_control(), ...) {
  UseMethod("draw")
}

ctx_tree_node2txt <- function(ct, vals, control) {
  if (is.null(ct[["f_by"]])) {
    NULL
  } else {
    if (!is.null(control[["frequency"]])) {
      if (control$frequency == "detailed") {
        stringr::str_c(ct[["f_by"]], collapse = ", ")
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

## Before mixvlmc 0.3.0, the second argument of draw() was the control list.
## Such calls are still supported, with a deprecation warning.
draw_format_control <- function(format, control, user_env = rlang::caller_env(2)) {
  if (rlang::is_missing(format)) {
    format <- "text"
  } else if (is.list(format)) {
    lifecycle::deprecate_soft(
      "0.3.0", I("Passing the control list as the second argument of `draw()`"),
      details = paste(
        "The second argument of `draw()` is now `format`:",
        "use `control = ` to specify the control list."
      ),
      user_env = user_env
    )
    control <- format
    format <- "text"
  } else {
    format <- match.arg(format, c("text", "latex"))
  }
  list(format = format, control = control)
}

#' @inherit draw
#' @param frequency this parameter controls the display of node level
#'   information in the tree. The default `NULL` value does not include
#'   anything. Setting `frequency` to `"total"` includes the frequency of the
#'   (partial) context of the node, while `"detailed"` includes the frequency of
#'   the states that follow the context (as in [contexts.ctx_tree()]).
#' @examples
#' set.seed(0)
#' rdts_c <- sample(c("A", "B", "CD"), 100, replace = TRUE)
#' ctree_c <- ctx_tree(rdts_c, min_size = 10, max_depth = 2)
#' draw(ctree_c, frequency = "total")
#' draw(ctree_c, frequency = "detailed")
#' ## LaTeX output
#' draw(ctree_c, "latex", frequency = "detailed")
#' rdts_c <- sample(c("A$", "_{B", "{C}_{D}"), 100, replace = TRUE)
#' ctree_c <- ctx_tree(rdts_c, min_size = 10, max_depth = 2)
#' ## the LaTeX output is sanitised
#' draw(ctree_c, "latex", frequency = "detailed")
#' @param ... not used (customisation options are given with [draw_control()]).
#' @export
draw.ctx_tree <- function(ct, format, control = draw_control(),
                          frequency = NULL, ...) {
  rlang::check_dots_empty()
  format_control <- draw_format_control(format, control)
  format <- format_control$format
  control <- format_control$control
  if (format == "text") {
    if (is.null(frequency)) {
      res <- format_ctx_tree(ct, control, NULL)
    } else {
      frequency <- match.arg(frequency, c("total", "detailed"))
      res <- format_ctx_tree(
        ct,
        c(control, list(frequency = frequency)),
        ctx_tree_node2txt
      )
    }
  } else if (format == "latex") {
    res <- format_latex_ctx_tree(
      ct, xtable::sanitize(ct$vals, "latex"),
      c(control, list(frequency = frequency)),
      ctx_tree_node2latex
    )
  }
  new_mixvlmc_draw(res, format)
}
