# Print and convert the representation of a context tree

[`draw()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/draw.md)
returns an object of class `mixvlmc_draw` that contains the text or
LaTeX representation of a context tree. These functions print it and
convert it to a character vector.

## Usage

``` r
# S3 method for class 'mixvlmc_draw'
print(x, ...)

# S3 method for class 'mixvlmc_draw'
format(x, ...)

# S3 method for class 'mixvlmc_draw'
as.character(x, ...)
```

## Arguments

- x:

  a representation of a context tree produced by
  [`draw()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/draw.md).

- ...:

  additional arguments (not used).

## Value

[`print()`](https://rdrr.io/r/base/print.html) returns its argument
(invisibly), [`format()`](https://rdrr.io/r/base/format.html) and
[`as.character()`](https://rdrr.io/r/base/character.html) return the
representation as a character vector, one string per line.

## Details

[`print()`](https://rdrr.io/r/base/print.html) writes the
representation, one line after another. It is called automatically when
the result of
[`draw()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/draw.md)
is not assigned to a variable, for instance when `draw(model)` is called
in the console.

In a knitr document (R Markdown, Quarto, ...), the result of
[`draw()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/draw.md)
is printed as standard R output, except for a LaTeX representation
(`format = "latex"`) in a document that produces LaTeX (e.g. a PDF
output). In this case, the LaTeX code is included as is in the document
and the `forest` LaTeX package is loaded automatically at the end of its
preamble (with the `AtEndPreamble` command of the `etoolbox` package),
so that the tree is rendered. A document can also load `forest` in its
preamble, possibly with options: it is then not loaded a second time.

## See also

[`draw()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/draw.md)

## Examples

``` r
set.seed(0)
rdts <- sample(c(0, 1), 100, replace = TRUE)
ctree <- ctx_tree(rdts, min_size = 10, max_depth = 2)
tree_repr <- draw(ctree)
print(tree_repr)
#> ▪
#> ├─ 0
#> │  ├─ 0
#> │  └─ 1
#> └─ 1
#>    ├─ 0
#>    └─ 1
as.character(draw(ctree, "latex"))
#> [1] "\\begin{forest}"        "[\\textbf{$\\epsilon$}" "[\\textbf{0}"          
#> [4] "[\\textbf{0}]"          "[\\textbf{1}]]"         "[\\textbf{1}"          
#> [7] "[\\textbf{0}]"          "[\\textbf{1}]]]"        "\\end{forest}"         
```
