# Report the nature of a node in a context tree

This function returns `TRUE` if the node is a proper context, `FALSE` in
the other case.

## Usage

``` r
is_context(node)
```

## Arguments

- node:

  a `ctx_node` object as returned by
  [`find_sequence()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/find_sequence.md)

## Value

`TRUE` if the node `node` is a proper context, `FALSE` when this is not
the case

## Examples

``` r
rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 3)
draw(rdts_ctree)
#> ▪
#> ├─ 0
#> │  ├─ 0
#> │  │  └─ 1
#> │  └─ 1
#> │     ├─ 0
#> │     └─ 1
#> └─ 1
#>    ├─ 0
#>    │  ├─ 0
#>    │  └─ 1
#>    └─ 1
#>       ├─ 0
#>       └─ 1
## 0, 0 is a context but 1, 0 is not
is_context(find_sequence(rdts_ctree, c(0, 0)))
#> [1] TRUE
is_context(find_sequence(rdts_ctree, c(1, 0)))
#> [1] FALSE
```
