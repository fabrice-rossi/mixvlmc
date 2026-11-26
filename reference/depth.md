# Depth of a context tree

This function returns the depth of a context tree, i.e. the length of
the longest context represented in the tree.

## Usage

``` r
depth(ct)
```

## Arguments

- ct:

  a context tree.

## Value

the depth of the tree.

## Examples

``` r
rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 3)
## should be 3
depth(rdts_ctree)
#> [1] 3
```
