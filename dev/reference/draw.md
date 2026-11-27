# Text based representation of a context tree

This function 'draws' a context tree as a text.

## Usage

``` r
draw(ct, format, control = draw_control(), ...)
```

## Arguments

- ct:

  a context tree.

- format:

  a character string that specifies the output format of the function.
  Possible values are `"text"` (default) and `"latex"`. See details.

- control:

  a list of low level control parameters of the representation. See
  details and
  [`draw_control()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/draw_control.md).

- ...:

  additional arguments for the methods of `draw()`.

## Value

an object of class `mixvlmc_draw` that contains the representation of
the context tree. As for other R objects, it is printed automatically
when it is not assigned to a variable at top level (for instance when
`draw(model)` is called in the console), but not inside a function or a
loop where it must be printed explicitly with `print(draw(model))` (see
[`print.mixvlmc_draw()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/print.mixvlmc_draw.md)).

## Details

The function uses different text based formats (plain "ascii art" and
LaTeX) to represent the context tree. Fine-tuning of the representation
can be done via the
[`draw_control()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/draw_control.md)
function.

In addition to the structure of the context tree, `draw()` can represent
information attached to the nodes (contexts and partial contexts). This
is controlled by additional parameters depending on the type of the
context tree. In general, parameters given directly to `draw()` specify
*what* information is represented while details on *how* this
representation is made can be controlled via the `control` parameter and
the associated
[`draw_control()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/draw_control.md)
function.

## Format

The `format` parameter specifies the format used for the textual output.
With the default value `"text"` the output is produced in "ascii art".
The characters used to represent the tree are given by the `charset`
parameter of
[`draw_control()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/draw_control.md).
By default, they are UTF-8 characters when the output supports them and
ASCII characters otherwise (see the `mixvlmc.charset` option in
[mixvlmc-package](https://fabrice-rossi.github.io/mixvlmc/dev/reference/mixvlmc-package.md)).

With the `"latex"` value, the output is produced in LaTeX, leveraging
the [forest](https://ctan.org/pkg/forest) LaTeX package (see
<https://ctan.org/pkg/forest>). Each call to `draw()` produces a full
`forest` LaTeX environment. This can be included as is in a LaTeX
document, provided the `forest` package is loaded in the preamble of the
document. The LaTeX output is sanitised to avoid potential problems
induced by special characters in the names of the states of the context
tree.

In a knitr document (R Markdown, Quarto, ...) that produces LaTeX (e.g.
a PDF output), the LaTeX representation is included as is in the
document and the `forest` package is added automatically to its preamble
(unless the document loads it, possibly with options): calling
`draw(model, "latex")` in a chunk is enough to render the tree (see
[`print.mixvlmc_draw()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/print.mixvlmc_draw.md)).

## Examples

``` r
set.seed(0)
rdts <- sample(c(0, 1), 100, replace = TRUE)
ctree <- ctx_tree(rdts, min_size = 10, max_depth = 2)
draw(ctree)
#> ▪
#> ├─ 0
#> │  ├─ 0
#> │  └─ 1
#> └─ 1
#>    ├─ 0
#>    └─ 1
rdts_c <- sample(c("A", "B", "CD"), 100, replace = TRUE)
ctree_c <- ctx_tree(rdts_c, min_size = 10, max_depth = 2)
draw(ctree_c, control = draw_control(digits = 2))
#> ▪
#> ├─ A
#> │  ├─ B
#> │  └─ CD
#> ├─ B
#> │  ├─ A
#> │  ├─ B
#> │  └─ CD
#> └─ CD
#>    ├─ A
#>    ├─ B
#>    └─ CD
## LaTeX output
draw(ctree_c, "latex")
#> \begin{forest}
#> [\textbf{$\epsilon$}
#> [\textbf{A}
#> [\textbf{B}]
#> [\textbf{CD}]]
#> [\textbf{B}
#> [\textbf{A}]
#> [\textbf{B}]
#> [\textbf{CD}]]
#> [\textbf{CD}
#> [\textbf{A}]
#> [\textbf{B}]
#> [\textbf{CD}]]]
#> \end{forest}
```
