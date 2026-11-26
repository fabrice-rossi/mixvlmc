# Length of the sequence of a node

This function returns the length of the sequence represented by a node
of a context tree, that is its depth in the tree.

## Usage

``` r
# S3 method for class 'ctx_node'
length(x)
```

## Arguments

- x:

  a `ctx_node` object as returned by
  [`find_sequence()`](https://fabrice-rossi.github.io/mixvlmc/reference/find_sequence.md).

## Value

the length of the sequence represented by `x`.

## See also

[`find_sequence()`](https://fabrice-rossi.github.io/mixvlmc/reference/find_sequence.md),
[`as_sequence()`](https://fabrice-rossi.github.io/mixvlmc/reference/as_sequence.md)

## Examples

``` r
rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 3)
length(find_sequence(rdts_ctree, c(1, 0)))
#> [1] 2
```
