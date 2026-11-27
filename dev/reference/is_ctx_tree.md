# Test if the object is a context tree

This function returns `TRUE` for context trees and `FALSE` for other
objects.

## Usage

``` r
is_ctx_tree(x)
```

## Arguments

- x:

  an R object.

## Value

`TRUE` for context trees.

## Examples

``` r
rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 2)
is_ctx_tree(rdts_ctree)
#> [1] TRUE
is_ctx_tree(rdts)
#> [1] FALSE
```
