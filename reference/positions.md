# Report the positions of a sequence associated with a node

This function returns the positions of the sequence represented by
`node` in the time series used to build the context tree in which the
sequence is represented. This is only possible if those positions were
saved during the construction of the context tree (see the
`keep_position` parameter of
[`ctx_tree()`](https://fabrice-rossi.github.io/mixvlmc/reference/ctx_tree.md)).
If positions were not saved, a call to this function produces an error.

## Usage

``` r
positions(node)

# S3 method for class 'ctx_node'
positions(node)

# S3 method for class 'ctx_node_cpp'
positions(node)
```

## Arguments

- node:

  a `ctx_node` object as returned by
  [`find_sequence()`](https://fabrice-rossi.github.io/mixvlmc/reference/find_sequence.md)

## Value

positions of the sequence represented by `node` in the original time
series, as an integer vector

## Details

A position of a sequence `ctx` in the time series `x` is an index value
`t` such that the sequence ends with `x[t]`. Thus `x[t+1]`, when it
exists, is the value that follows the sequence. For instance if
`x=c(0, 0, 1, 1)` and `ctx=c(0, 1)` (in standard state order), then the
position of `ctx` in `x` is 3.

Positions include all the occurrences of the sequence, including an
occurrence at the end of the time series (`t=length(x)`), which is not
followed by any value. Such an occurrence is not taken into account by
[`counts()`](https://fabrice-rossi.github.io/mixvlmc/reference/counts.md),
which reports the values that follow the sequence: the number of
positions can therefore exceed the total count by one.

## Examples

``` r
set.seed(0)
rdts <- sample(as.factor(c("A", "B", "C")), 100, replace = TRUE)
rdts_tree <- ctx_tree(rdts, max_depth = 3, min_size = 5)
subseq <- find_sequence(rdts_tree, factor(c("B", "A"), levels = c("A", "B", "C")))
if (!is.null(subseq)) {
  positions(subseq)
}
#>  [1]  2  6 28 33 43 46 57 62 81 87
```
