# Trim a context tree

This function returns a trimmed context tree from which the positions of
the contexts in the time series (see the `keep_position` parameter of
[`ctx_tree()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/ctx_tree.md))
have been removed.

## Usage

``` r
trim(ct, ...)
```

## Arguments

- ct:

  a context tree.

- ...:

  additional arguments for the `trim()` methods.

## Value

a trimmed context tree.

## Examples

``` r
## context tree trimming
set.seed(0)
rdts <- sample(as.factor(c("A", "B", "C")), 1000, replace = TRUE)
rdts_tree <- ctx_tree(rdts,
  max_depth = 10, min_size = 5,
  keep_position = TRUE
)
print(object.size(rdts_tree))
#> 7984 bytes
rdts_tree <- trim(rdts_tree)
print(object.size(rdts_tree))
#> 7984 bytes
```
