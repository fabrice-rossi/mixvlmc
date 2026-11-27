# Text based representation of a VLMC

This function 'draws' a context tree as a text.

## Usage

``` r
# S3 method for class 'vlmc'
draw(ct, format, control = draw_control(), prob = TRUE, ...)

# S3 method for class 'vlmc_cpp'
draw(ct, format, control = draw_control(), prob = TRUE, ...)
```

## Arguments

- ct:

  a fitted VLMC.

- format:

  a character string that specifies the output format of the function.
  Possible values are `"text"` (default) and `"latex"`. See details.

- control:

  a list of low level control parameters of the representation. See
  details and
  [`draw_control()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/draw_control.md).

- prob:

  this parameter controls the display of node level information in the
  tree. The default `prob=TRUE` represents the conditional distribution
  of the states given the (partial) context associated with the node.
  Setting `prob=FALSE` replaces the conditional distribution by the
  frequency of the states that follow the context as in
  [`draw.ctx_tree()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/draw.ctx_tree.md).
  Setting `prob=NULL` removes all additional information.

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
pc <- powerconsumption[powerconsumption$week == 5, ]
rdts <- cut(pc$active_power,
  breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1)))
)
model <- vlmc(rdts, max_depth = 3)
draw(model)
#> ▪ (0.25, 0.25, 0.25, 0.25)
#> ├─ (0,0.458] (0.7968, 0.1912, 0.007968, 0.003984)
#> ├─ (0.458,1.34] (0.1984, 0.6667, 0.1071, 0.02778)
#> │  ├─ (0,0.458] (0.375, 0.4167, 0.1458, 0.0625)
#> │  └─ (0.458,1.34] (0.1369, 0.75, 0.09524, 0.01786)
#> │     └─ (0,0.458] (0.4, 0.55, 0.05, 0)
#> ├─ (1.34,2.13] (0.003968, 0.123, 0.7262, 0.1468)
#> │  └─ (2.13,7.54] (0, 0.2, 0.525, 0.275)
#> └─ (2.13,7.54] (0.003968, 0.01587, 0.1587, 0.8214)
#>    └─ (0.458,1.34] (0, 0.1429, 0.5714, 0.2857)
draw(model, prob = FALSE)
#> ▪ (252, 252, 252, 252)
#> ├─ (0,0.458] (200, 48, 2, 1)
#> ├─ (0.458,1.34] (50, 168, 27, 7)
#> │  ├─ (0,0.458] (18, 20, 7, 3)
#> │  └─ (0.458,1.34] (23, 126, 16, 3)
#> │     └─ (0,0.458] (8, 11, 1, 0)
#> ├─ (1.34,2.13] (1, 31, 183, 37)
#> │  └─ (2.13,7.54] (0, 8, 21, 11)
#> └─ (2.13,7.54] (1, 4, 40, 207)
#>    └─ (0.458,1.34] (0, 1, 4, 2)
draw(model, prob = NULL)
#> ▪
#> ├─ (0,0.458]
#> ├─ (0.458,1.34]
#> │  ├─ (0,0.458]
#> │  └─ (0.458,1.34]
#> │     └─ (0,0.458]
#> ├─ (1.34,2.13]
#> │  └─ (2.13,7.54]
#> └─ (2.13,7.54]
#>    └─ (0.458,1.34]
## LaTeX output
draw(model, "latex")
#> \begin{forest}
#> [\multicolumn{4}{c}{\textbf{$\epsilon$}}\\\hline
#> {\small 0.25}&{\small 0.25}&{\small 0.25}&{\small 0.25}, align=cccc
#> [\multicolumn{4}{c}{\textbf{(0,0.458]}}\\\hline
#> {\small 0.7968}&{\small 0.1912}&{\small 0.007968}&{\small 0.003984}, align=cccc]
#> [\multicolumn{4}{c}{\textbf{(0.458,1.34]}}\\\hline
#> {\small 0.1984}&{\small 0.6667}&{\small 0.1071}&{\small 0.02778}, align=cccc
#> [\multicolumn{4}{c}{\textbf{(0,0.458]}}\\\hline
#> {\small 0.375}&{\small 0.4167}&{\small 0.1458}&{\small 0.0625}, align=cccc]
#> [\multicolumn{4}{c}{\textbf{(0.458,1.34]}}\\\hline
#> {\small 0.1369}&{\small 0.75}&{\small 0.09524}&{\small 0.01786}, align=cccc
#> [\multicolumn{4}{c}{\textbf{(0,0.458]}}\\\hline
#> {\small 0.4}&{\small 0.55}&{\small 0.05}&{\small 0}, align=cccc]]]
#> [\multicolumn{4}{c}{\textbf{(1.34,2.13]}}\\\hline
#> {\small 0.003968}&{\small 0.123}&{\small 0.7262}&{\small 0.1468}, align=cccc
#> [\multicolumn{4}{c}{\textbf{(2.13,7.54]}}\\\hline
#> {\small 0}&{\small 0.2}&{\small 0.525}&{\small 0.275}, align=cccc]]
#> [\multicolumn{4}{c}{\textbf{(2.13,7.54]}}\\\hline
#> {\small 0.003968}&{\small 0.01587}&{\small 0.1587}&{\small 0.8214}, align=cccc
#> [\multicolumn{4}{c}{\textbf{(0.458,1.34]}}\\\hline
#> {\small 0}&{\small 0.1429}&{\small 0.5714}&{\small 0.2857}, align=cccc]]]
#> \end{forest}
```
