# Extract the sequence encoded by a node

This function returns the sequence represented by the `node` object.

## Usage

``` r
as_sequence(node, reverse)
```

## Arguments

- node:

  a `ctx_node` object as returned by
  [`find_sequence()`](https://fabrice-rossi.github.io/mixvlmc/reference/find_sequence.md)

- reverse:

  specifies whether the sequence should be reported in reverse temporal
  order (`TRUE`) or in the temporal order (`FALSE`). Defaults to the
  order associated with the `ctx_node` which is determined by the
  parameters of the call to
  [`contexts()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.md)
  or
  [`find_sequence()`](https://fabrice-rossi.github.io/mixvlmc/reference/find_sequence.md)
  (see
  [`is_reversed()`](https://fabrice-rossi.github.io/mixvlmc/reference/is_reversed.md)).

## Value

the sequence represented by the `node` object, a vector

## Examples

``` r
rdts <- c("A", "B", "C", "A", "A", "B", "B", "C", "C", "A")
rdts_tree <- ctx_tree(rdts, max_depth = 3)
res <- find_sequence(rdts_tree, c("A", "B"))
as_sequence(res)
#> [1] "A" "B"
as_sequence(res, reverse = TRUE)
#> [1] "B" "A"
```
