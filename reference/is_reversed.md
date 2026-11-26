# Report the ordering convention of the node

This function returns `TRUE` if the node is using a reverse temporal
ordering and `FALSE` in the other case.

## Usage

``` r
is_reversed(node)
```

## Arguments

- node:

  a `ctx_node` object as returned by
  [`find_sequence()`](https://fabrice-rossi.github.io/mixvlmc/reference/find_sequence.md)

## Value

`TRUE` if the node `node` uses a reverse temporal ordering, `FALSE` when
this is not the case

## See also

[`rev.ctx_node()`](https://fabrice-rossi.github.io/mixvlmc/reference/rev.ctx_node.md)

## Examples

``` r
rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 3)
is_reversed(find_sequence(rdts_ctree, c(0, 0)))
#> [1] FALSE
is_reversed(find_sequence(rdts_ctree, c(1, 0), reverse = TRUE))
#> [1] TRUE
```
