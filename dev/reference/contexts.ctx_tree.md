# Contexts of a context tree

This function extracts from a context tree a description of all of its
contexts.

## Usage

``` r
# S3 method for class 'ctx_tree'
contexts(
  ct,
  sequence = FALSE,
  reverse = FALSE,
  frequency = NULL,
  positions = FALSE,
  ...
)

# S3 method for class 'ctx_tree_cpp'
contexts(
  ct,
  sequence = FALSE,
  reverse = FALSE,
  frequency = NULL,
  positions = FALSE,
  ...
)
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

- frequency:

  specifies the counts to be included in the result data.frame. The
  default value of `NULL` does not include anything. `"total"` gives the
  number of occurrences of each context in the original sequence.
  `"detailed"` includes in addition the breakdown of these occurrences
  into all the possible states.

- positions:

  logical (defaults to `FALSE`). Specifies whether the positions of each
  context in the time series used to build the context tree should be
  reported in a `positions` column of the result data frame. The
  availability of the positions depends on the way the context tree was
  built (see the `keep_position` parameter of
  [`ctx_tree()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/ctx_tree.md)).
  See the dedicated section for the definition of a position.

- ...:

  additional arguments for the contexts function.

## Value

A list of class `contexts` containing the contexts represented in this
tree (as `ctx_node`) or a data.frame.

## Details

The default behaviour of the function is to return a list of all the
contexts using `ctx_node` objects (as returned by
[`find_sequence()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/find_sequence.md)).
The properties of the contexts can then be explored using adapted
functions such as
[`counts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/counts.md)
and
[`positions()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/positions.md).

When `sequence=TRUE` the method returns a data.frame whose first column,
named `context`, contains the contexts as vectors (i.e. the value
returned by
[`as_sequence()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/as_sequence.md)
applied to a `ctx_node` object). Other columns contain context specific
values specified by the additional parameters. Setting any of those
parameters to a value that asks for reporting information will toggle
the result type of the function to `data.frame`.

If `frequency="total"`, an additional column named `freq` gives the
number of occurrences of each context in the series used to build the
tree. If `frequency="detailed"`, one additional column is added per
state in the context space. Each column records the number of times a
given context is followed by the corresponding value in the original
series.

## Positions

A position of a context `ctx` in the time series `x` is an index value
`t` such that the context ends with `x[t]`. Thus `x[t+1]`, when it
exists, is the value that follows the context. For instance if
`x=c(0, 0, 1, 1)` and `ctx=c(0, 1)` (in standard state order), then the
position of `ctx` in `x` is 3.

Positions include all the occurrences of a context, including an
occurrence at the end of the time series (`t=length(x)`), which is not
followed by any value. Such an occurrence is not included in the
frequencies, which count the values that follow the context: the number
of positions of a context can therefore exceed its frequency by one.

## State order in a context

Notice that contexts are given by default in the temporal order and not
in the "reverse" order used by many VLMC research papers: older values
are on the left. For instance, the context `c(1, 0)` is reported if the
sequence 0, then 1 appeared in the time series used to build the context
tree. Set `reverse` to `TRUE` for the reverse convention which is
somewhat easier to relate to the way the context trees are represented
by
[`draw()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/draw.md)
(i.e. recent values at the top of the tree).

## See also

[`find_sequence()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/find_sequence.md)
and
[`find_sequence.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/find_sequence.covlmc.md)
for direct access to a specific context, and `contexts.ctx_tree()`,
[`contexts.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.vlmc.md)
and
[`contexts.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.covlmc.md)
for concrete implementations of
[`contexts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.md).

## Examples

``` r
set.seed(0)
rdts <- sample(as.factor(c("A", "B", "C")), 100, replace = TRUE)
rdts_tree <- ctx_tree(rdts, max_depth = 3, min_size = 5)
## direct representation with ctx_node objects
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
## data.frame format
contexts(rdts_tree, sequence = TRUE)
#>    context
#> 1     C, A
#> 2  A, A, A
#> 3     A, A
#> 4     B, A
#> 5     A, B
#> 6  C, B, B
#> 7  B, B, B
#> 8     B, B
#> 9  A, C, B
#> 10 C, C, B
#> 11    C, B
#> 12 B, A, C
#> 13    A, C
#> 14 B, C, C
#> 15    C, C
#> 16 B, B, C
#> 17    B, C
contexts(rdts_tree, frequency = "total")
#>    context freq
#> 1     C, A    7
#> 2  A, A, A    7
#> 3     A, A   15
#> 4     B, A   10
#> 5     A, B    6
#> 6  C, B, B    6
#> 7  B, B, B   10
#> 8     B, B   19
#> 9  A, C, B    5
#> 10 C, C, B    5
#> 11    C, B   12
#> 12 B, A, C    5
#> 13    A, C   11
#> 14 B, C, C    5
#> 15    C, C    9
#> 16 B, B, C    6
#> 17    B, C    9
contexts(rdts_tree, frequency = "detailed")
#>    context freq A  B C
#> 1     C, A    7 4  1 2
#> 2  A, A, A    7 3  2 2
#> 3     A, A   15 7  4 4
#> 4     B, A   10 4  1 5
#> 5     A, B    6 3  3 0
#> 6  C, B, B    6 0  3 3
#> 7  B, B, B   10 3  4 3
#> 8     B, B   19 3 10 6
#> 9  A, C, B    5 2  2 1
#> 10 C, C, B    5 0  3 2
#> 11    C, B   12 3  6 3
#> 12 B, A, C    5 2  1 2
#> 13    A, C   11 3  5 3
#> 14 B, C, C    5 2  2 1
#> 15    C, C    9 2  5 2
#> 16 B, B, C    6 1  2 3
#> 17    B, C    9 2  2 5
contexts(rdts_tree, frequency = "total", positions = TRUE)
#>    context freq    positions
#> 1     C, A    7 4, 13, 2....
#> 2  A, A, A    7 15, 25, ....
#> 3     A, A   15 14, 15, ....
#> 4     B, A   10 2, 6, 28....
#> 5     A, B    6 5, 16, 2....
#> 6  C, B, B    6 10, 38, ....
#> 7  B, B, B   10 18, 19, ....
#> 8     B, B   19 10, 17, ....
#> 9  A, C, B    5 37, 45, ....
#> 10 C, C, B    5 9, 53, 6....
#> 11    C, B   12 9, 37, 4....
#> 12 B, A, C    5 3, 7, 34....
#> 13    A, C   11 3, 7, 22....
#> 14 B, C, C    5 12, 52, ....
#> 15    C, C    9 8, 12, 5....
#> 16 B, B, C    6 11, 20, ....
#> 17    B, C    9 11, 20, ....
```
