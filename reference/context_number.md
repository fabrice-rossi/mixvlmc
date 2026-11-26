# Number of contexts of a context tree

This function returns the number of distinct contexts in a context tree.

## Usage

``` r
context_number(ct)
```

## Arguments

- ct:

  a context tree.

## Value

the number of contexts of the tree.

## Examples

``` r
rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 3)
# should be 8
context_number(rdts_ctree)
#> [1] 8
```
