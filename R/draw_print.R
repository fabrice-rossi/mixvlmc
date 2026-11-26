## Representation of a context tree produced by draw(): the lines of the
## representation and its format
new_mixvlmc_draw <- function(lines, format) {
  ## some elements may contain several lines
  lines <- strsplit(paste(lines, collapse = "\n"), "\n", fixed = TRUE)[[1]]
  structure(lines, format = format, class = "mixvlmc_draw")
}

#' Print and convert the representation of a context tree
#'
#' [draw()] returns an object of class `mixvlmc_draw` that contains the text
#' or LaTeX representation of a context tree. These functions print it and
#' convert it to a character vector.
#'
#' `print()` writes the representation, one line after another. It is called
#' automatically when the result of [draw()] is not assigned to a variable,
#' for instance when `draw(model)` is called in the console.
#'
#' In a knitr document (R Markdown, Quarto, ...), the result of [draw()] is
#' printed as standard R output, except for a LaTeX representation
#' (`format = "latex"`) in a document that produces LaTeX (e.g. a PDF output).
#' In this case, the LaTeX code is included as is in the document and the
#' `forest` LaTeX package is loaded automatically at the end of its preamble
#' (with the `AtEndPreamble` command of the `etoolbox` package), so that the
#' tree is rendered. A document can also load `forest` in its preamble,
#' possibly with options: it is then not loaded a second time.
#'
#' @param x a representation of a context tree produced by [draw()].
#' @param ... additional arguments (not used).
#' @returns `print()` returns its argument (invisibly), `format()` and
#'   `as.character()` return the representation as a character vector, one
#'   string per line.
#' @seealso [draw()]
#' @examples
#' set.seed(0)
#' rdts <- sample(c(0, 1), 100, replace = TRUE)
#' ctree <- ctx_tree(rdts, min_size = 10, max_depth = 2)
#' tree_repr <- draw(ctree)
#' print(tree_repr)
#' as.character(draw(ctree, "latex"))
#' @export
print.mixvlmc_draw <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}

#' @rdname print.mixvlmc_draw
#' @export
format.mixvlmc_draw <- function(x, ...) {
  as.character(unclass(x))
}

#' @rdname print.mixvlmc_draw
#' @export
as.character.mixvlmc_draw <- function(x, ...) {
  format(x)
}

## LaTeX dependency of the representations of format "latex". The forest
## package is loaded at the end of the preamble (with etoolbox's
## \AtEndPreamble), and only if the document did not load it, so that a
## document can load forest itself, with its own options, anywhere in its
## preamble.
forest_latex_dependency <- function() {
  load_forest <- paste0(
    "\\AtEndPreamble{\\csname @ifpackageloaded\\endcsname{forest}{}",
    "{\\usepackage{forest}}}"
  )
  if (has_rmarkdown()) {
    rmarkdown::latex_dependency("etoolbox", extra_lines = load_forest)
  } else {
    structure(
      list(name = "etoolbox", options = NULL, extra_lines = load_forest),
      class = "latex_dependency"
    )
  }
}

## this function is introduced only to ease mocking in tests
has_rmarkdown <- function() {
  rlang::is_installed("rmarkdown")
}

## knitr printing (registered in .onLoad())
knit_print.mixvlmc_draw <- function(x, ...) {
  if (identical(attr(x, "format"), "latex") && knitr::is_latex_output()) {
    ## blank lines make each tree a paragraph of its own
    knitr::asis_output(
      paste0("\n\n", paste(format(x), collapse = "\n"), "\n\n"),
      meta = list(forest_latex_dependency())
    )
  } else {
    knitr::normal_print(x, ...)
  }
}
