# Text based representation of a context tree

This function 'draws' a context tree as a text.

## Usage

``` r
# S3 method for class 'ctx_tree_cpp'
draw(ct, format, control = draw_control(), frequency = NULL, ...)

# S3 method for class 'ctx_tree'
draw(ct, format, control = draw_control(), frequency = NULL, ...)
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

- frequency:

  this parameter controls the display of node level information in the
  tree. The default `NULL` value does not include anything. Setting
  `frequency` to `"total"` includes the frequency of the (partial)
  context of the node, while `"detailed"` includes the frequency of the
  states that follow the context (as in
  [`contexts.ctx_tree()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.ctx_tree.md)).

- ...:

  not used (customisation options are given with
  [`draw_control()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/draw_control.md)).

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

In addition to the structure of the context tree,
[`draw()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/draw.md)
can represent information attached to the nodes (contexts and partial
contexts). This is controlled by additional parameters depending on the
type of the context tree. In general, parameters given directly to
[`draw()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/draw.md)
specify *what* information is represented while details on *how* this
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
<https://ctan.org/pkg/forest>). Each call to
[`draw()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/draw.md)
produces a full `forest` LaTeX environment. This can be included as is
in a LaTeX document, provided the `forest` package is loaded in the
preamble of the document. The LaTeX output is sanitised to avoid
potential problems induced by special characters in the names of the
states of the context tree.

In a knitr document (R Markdown, Quarto, ...) that produces LaTeX (e.g.
a PDF output), the LaTeX representation is included as is in the
document and the `forest` package is added automatically to its preamble
(unless the document loads it, possibly with options): calling
`draw(model, "latex")` in a chunk is enough to render the tree (see
[`print.mixvlmc_draw()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/print.mixvlmc_draw.md)).

## Examples

``` r
set.seed(0)
rdts_c <- sample(c("A", "B", "CD"), 100, replace = TRUE)
ctree_c <- ctx_tree(rdts_c, min_size = 10, max_depth = 2)
draw(ctree_c, frequency = "total")
#> ▪ (100)
#> ├─ A (32)
#> │  ├─ A (15)
#> │  └─ B (10)
#> ├─ B (38)
#> │  ├─ B (19)
#> │  └─ CD (12)
#> └─ CD (29)
#>    └─ A (11)
draw(ctree_c, frequency = "detailed")
#> ▪ (32, 38, 30)
#> ├─ A (15, 6, 11)
#> │  ├─ A (7, 4, 4)
#> │  └─ B (4, 1, 5)
#> ├─ B (10, 19, 9)
#> │  ├─ B (3, 10, 6)
#> │  └─ CD (3, 6, 3)
#> └─ CD (7, 12, 10)
#>    └─ A (3, 5, 3)
## LaTeX output
draw(ctree_c, "latex", frequency = "detailed")
#> \begin{forest}
#> [\multicolumn{3}{c}{\textbf{$\epsilon$}}\\\hline
#> {\small 32}&{\small 38}&{\small 30}, align=ccc
#> [\multicolumn{3}{c}{\textbf{A}}\\\hline
#> {\small 15}&{\small 6}&{\small 11}, align=ccc
#> [\multicolumn{3}{c}{\textbf{A}}\\\hline
#> {\small 7}&{\small 4}&{\small 4}, align=ccc]
#> [\multicolumn{3}{c}{\textbf{B}}\\\hline
#> {\small 4}&{\small 1}&{\small 5}, align=ccc]]
#> [\multicolumn{3}{c}{\textbf{B}}\\\hline
#> {\small 10}&{\small 19}&{\small 9}, align=ccc
#> [\multicolumn{3}{c}{\textbf{B}}\\\hline
#> {\small 3}&{\small 10}&{\small 6}, align=ccc]
#> [\multicolumn{3}{c}{\textbf{CD}}\\\hline
#> {\small 3}&{\small 6}&{\small 3}, align=ccc]]
#> [\multicolumn{3}{c}{\textbf{CD}}\\\hline
#> {\small 7}&{\small 12}&{\small 10}, align=ccc
#> [\multicolumn{3}{c}{\textbf{A}}\\\hline
#> {\small 3}&{\small 5}&{\small 3}, align=ccc]]]
#> \end{forest}
rdts_c <- sample(c("A$", "_{B", "{C}_{D}"), 100, replace = TRUE)
ctree_c <- ctx_tree(rdts_c, min_size = 10, max_depth = 2)
## the LaTeX output is sanitised
draw(ctree_c, "latex", frequency = "detailed")
#> \begin{forest}
#> [\multicolumn{3}{c}{\textbf{$\epsilon$}}\\\hline
#> {\small 33}&{\small 31}&{\small 36}, align=ccc
#> [\multicolumn{3}{c}{\textbf{A\$}}\\\hline
#> {\small 8}&{\small 13}&{\small 12}, align=ccc
#> [\multicolumn{3}{c}{\textbf{\_\{B}}\\\hline
#> {\small 1}&{\small 3}&{\small 7}, align=ccc]
#> [\multicolumn{3}{c}{\textbf{\{C\}\_\{D\}}}\\\hline
#> {\small 5}&{\small 5}&{\small 3}, align=ccc]]
#> [\multicolumn{3}{c}{\textbf{\_\{B}}\\\hline
#> {\small 11}&{\small 10}&{\small 10}, align=ccc
#> [\multicolumn{3}{c}{\textbf{A\$}}\\\hline
#> {\small 5}&{\small 6}&{\small 2}, align=ccc]
#> [\multicolumn{3}{c}{\textbf{\_\{B}}\\\hline
#> {\small 3}&{\small 4}&{\small 3}, align=ccc]]
#> [\multicolumn{3}{c}{\textbf{\{C\}\_\{D\}}}\\\hline
#> {\small 13}&{\small 8}&{\small 14}, align=ccc
#> [\multicolumn{3}{c}{\textbf{A\$}}\\\hline
#> {\small 5}&{\small 3}&{\small 3}, align=ccc]
#> [\multicolumn{3}{c}{\textbf{\_\{B}}\\\hline
#> {\small 2}&{\small 2}&{\small 6}, align=ccc]
#> [\multicolumn{3}{c}{\textbf{\{C\}\_\{D\}}}\\\hline
#> {\small 6}&{\small 3}&{\small 5}, align=ccc]]]
#> \end{forest}
```
