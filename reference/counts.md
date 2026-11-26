# Report the distribution of values that follow occurrences of a sequence

This function reports the number of occurrences of the sequence
represented by `node` in the original time series used to build the
associated context tree (not including a possible final occurrence not
followed by any value at the end of the original time series). In
addition if `frequency="detailed"` (default), the function reports the
frequencies of each of the possible values of the time series when they
appear just after the sequence.

## Usage

``` r
counts(node, frequency = c("detailed", "total"), local = FALSE)

# S3 method for class 'ctx_node'
counts(node, frequency = c("detailed", "total"), local = FALSE)

# S3 method for class 'ctx_node_cpp'
counts(node, frequency = c("detailed", "total"), local = FALSE)
```

## Arguments

- node:

  a `ctx_node` object as returned by
  [`find_sequence()`](https://fabrice-rossi.github.io/mixvlmc/reference/find_sequence.md)

- frequency:

  specifies the counts to be included in the result. `"total"` gives the
  number of occurrences of the sequence in the original sequence.
  `"detailed"` (default) includes in addition the breakdown of these
  occurrences into all the possible states.

- local:

  specifies how the counts are computed. When `local` is `FALSE`
  (default value) the counts include both counts that are specific to
  the context (if any) and counts from the descendants of the context in
  the tree. When `local` is `TRUE` the counts include only the number of
  times the context appears without being the last part of a longer
  context.

## Value

either an integer when `frequency="total"` which gives the total number
of occurrences of the sequence represented by `node` or a `data.frame`
with a `total` column with the same value and a column for each of the
possible values of the original time series, reporting counts in each
column (see the description above).

## See also

[`contexts()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.md)
and
[`contexts.ctx_tree()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.ctx_tree.md)

## Examples

``` r
set.seed(0)
rdts <- sample(as.factor(c("A", "B", "C")), 100, replace = TRUE)
rdts_tree <- ctx_tree(rdts, max_depth = 3, min_size = 5)
subseq <- find_sequence(rdts_tree, factor(c("A", "A"), levels = c("A", "B", "C")))
if (!is.null(subseq)) {
  print(counts(subseq))
  print(counts(subseq, frequency = "total"))
  ## counts of "A", "A" when it is not the end of a longer context
  print(counts(subseq, local = TRUE))
}
#>   total A B C
#> 1    15 7 4 4
#> [1] 15
#>   total A B C
#> 1     8 4 2 2
```
