# Contexts of a context tree

This function extracts from a context tree a description of all of its
contexts.

## Usage

``` r
contexts(ct, sequence = FALSE, reverse = FALSE, ...)
```

## Arguments

- ct:

  a context tree.

- sequence:

  if `TRUE` the function returns its results as a `data.frame`, if
  `FALSE` (default) as a list of `ctx_node` objects (see details).

- reverse:

  logical (defaults to `FALSE`). If `TRUE`, contexts are given in
  reverse temporal order. See the dedicated section.

- ...:

  additional arguments for the contexts function.

## Value

A list of class `contexts` containing the contexts represented in this
tree (as `ctx_node`) or a data.frame.

## Details

The default behaviour consists in returning a list of all the contexts
contained in the tree using `ctx_node` objects (as returned by e.g.
[`find_sequence()`](https://fabrice-rossi.github.io/mixvlmc/reference/find_sequence.md)).
The properties of the contexts can then be explored using adapted
functions such as
[`counts()`](https://fabrice-rossi.github.io/mixvlmc/reference/counts.md)
and
[`positions()`](https://fabrice-rossi.github.io/mixvlmc/reference/positions.md).
The result list is of class `contexts`. When `sequence=TRUE`, the method
returns a data.frame whose first column, named `context`, contains the
contexts as vectors (i.e. the value returned by
[`as_sequence()`](https://fabrice-rossi.github.io/mixvlmc/reference/as_sequence.md)
applied to a `ctx_node` object). Other columns contain context specific
values which depend on the actual class of the tree and on additional
parameters. In all implementations of `contexts()`, setting the
additional parameters to any non-default value leads to a `data.frame`
result.

## State order in a context

Notice that contexts are given by default in the temporal order and not
in the "reverse" order used by many VLMC research papers: older values
are on the left. For instance, the context `c(1, 0)` is reported if the
sequence 0, then 1 appeared in the time series used to build the context
tree. Set `reverse` to `TRUE` for the reverse convention which is
somewhat easier to relate to the way the context trees are represented
by [`draw()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.md)
(i.e. recent values at the top of the tree).

## See also

[`find_sequence()`](https://fabrice-rossi.github.io/mixvlmc/reference/find_sequence.md)
and
[`find_sequence.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/find_sequence.covlmc.md)
for direct access to a specific context, and
[`contexts.ctx_tree()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.ctx_tree.md),
[`contexts.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.vlmc.md)
and
[`contexts.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.covlmc.md)
for concrete implementations of `contexts()`.

## Examples

``` r
set.seed(0)
rdts <- sample(as.factor(c("A", "B", "C")), 100, replace = TRUE)
rdts_tree <- ctx_tree(rdts, max_depth = 3, min_size = 5)
contexts(rdts_tree)
#> Contexts:
#>  C, A
#>  A, A, A
#>  A, A
#>  B, A
#>  A, B
#>  C, B, B
#>  B, B, B
#>  B, B
#>  A, C, B
#>  C, C, B
#>  C, B
#>  B, A, C
#>  A, C
#>  B, C, C
#>  C, C
#>  B, B, C
#>  B, C
contexts(rdts_tree, sequence = TRUE, reverse = TRUE)
#>    context
#> 1     A, C
#> 2  A, A, A
#> 3     A, A
#> 4     A, B
#> 5     B, A
#> 6  B, B, C
#> 7  B, B, B
#> 8     B, B
#> 9  B, C, A
#> 10 B, C, C
#> 11    B, C
#> 12 C, A, B
#> 13    C, A
#> 14 C, C, B
#> 15    C, C
#> 16 C, B, B
#> 17    C, B
```
