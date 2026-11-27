# Contexts of a VLMC

This function extracts all the contexts from a fitted VLMC, possibly
with some associated data.

## Usage

``` r
# S3 method for class 'vlmc'
contexts(
  ct,
  sequence = FALSE,
  reverse = FALSE,
  frequency = NULL,
  positions = FALSE,
  local = FALSE,
  cutoff = NULL,
  metrics = FALSE,
  ...
)

# S3 method for class 'vlmc_cpp'
contexts(
  ct,
  sequence = FALSE,
  reverse = FALSE,
  frequency = NULL,
  positions = FALSE,
  local = FALSE,
  cutoff = NULL,
  metrics = FALSE,
  ...
)
```

## Arguments

- ct:

  a context tree.

- sequence:

  if `TRUE` the function returns its results as a `data.frame`, if
  `FALSE` (default) as a list of `ctx_node` objects (see details).

- reverse:

  logical (defaults to `FALSE`). If `TRUE`, contexts are given in
  reverse temporal order. See the dedicated section.

- frequency:

  specifies the counts to be included in the result data.frame. The
  default value of `NULL` does not include anything. `"total"` gives the
  number of occurrences of each context in the original sequence.
  `"detailed"` includes in addition the breakdown of these occurrences
  into all the possible states.

- positions:

  logical (defaults to `FALSE`). Specifies whether the positions of each
  context in the time series used to build the context tree should be
  reported in a `positions` column of the result data frame. The
  availability of the positions depends on the way the context tree was
  built (see the `keep_position` parameter of
  [`ctx_tree()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/ctx_tree.md)).
  See the dedicated section for the definition of a position.

- local:

  specifies how the counts reported by `frequency` are computed. When
  `local` is `FALSE` (default value) the counts include both counts that
  are specific to the context (if any) and counts from the descendants
  of the context in the tree. When `local` is `TRUE` the counts include
  only the number of times the context appears without being the last
  part of a longer context.

- cutoff:

  specifies whether to include the cut off value associated with each
  context (see
  [`cutoff()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.md)
  and
  [`prune()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/prune.md)).
  The default result with `cutoff=NULL` does not include those values.
  Setting `cutoff` to `"quantile"` adds the cut off values in quantile
  scale, while `cutoff="native"` adds them in the native scale. The
  returned values are directly based on the log likelihood ratio
  computed in the context tree and are not modified to ensure pruning
  (as when
  [`cutoff()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.md)
  is called with `raw=TRUE`).

- metrics:

  if `TRUE`, adds predictive metrics for each context (see
  [`metrics()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/metrics.md)
  for the definition of predictive metrics). As in
  [`metrics.ctx_node()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/metrics.ctx_node.md),
  the AUC of a context is `NA`.

- ...:

  additional arguments for the contexts function.

## Value

A list of class `contexts` containing the contexts represented in this
tree (as `ctx_node`) or a data.frame.

## Details

The default behaviour of the function is to return a list of all the
contexts using `ctx_node` objects (as returned by
[`find_sequence()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/find_sequence.md)).
The properties of the contexts can then be explored using adapted
functions such as
[`counts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/counts.md),
[`cutoff.ctx_node()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.ctx_node.md),
[`metrics.ctx_node()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/metrics.ctx_node.md)
and
[`positions()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/positions.md).

When `sequence=TRUE` the method returns a data.frame whose first column,
named `context`, contains the contexts as vectors (i.e. the value
returned by
[`as_sequence()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/as_sequence.md)
applied to a `ctx_node` object). Other columns contain context specific
values specified by the additional parameters. Setting any of those
parameters to a value that asks for reporting information will toggle
the result type of the function to `data.frame`.

The `frequency` parameter is described in detail in the documentation of
[`contexts.ctx_tree()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.ctx_tree.md).
When `cutoff` is non `NULL`, the resulting `data.frame` contains a
`cutoff` column with the cut off values, either in quantile or in native
scale. See
[`cutoff.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.vlmc.md)
and
[`prune.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/prune.md)
for the definitions of cut off values and of the two scales.

## Cut off values

The cut off values reported by `contexts.vlmc()` can be different from
the ones reported by
[`cutoff.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.vlmc.md)
for three reasons:

1.  [`cutoff.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.vlmc.md)
    reports only useful cut off values, i.e., cut off values that should
    induce a simplification of the VLMC when used in
    [`prune()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/prune.md).
    This excludes cut off values associated with simple contexts that
    are smaller than the ones of their descendants in the context tree.
    Those values are reported by `contexts.vlmc()`.

2.  `contexts.vlmc()` reports only cut off values of actual contexts,
    while
    [`cutoff.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.vlmc.md)
    reports cut off values for all nodes of the context tree.

3.  values are not modified to induce pruning, unlike the default
    behaviour of
    [`cutoff.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.vlmc.md).

## Positions

A position of a context `ctx` in the time series `x` is an index value
`t` such that the context ends with `x[t]`. Thus `x[t+1]`, when it
exists, is the value that follows the context. For instance if
`x=c(0, 0, 1, 1)` and `ctx=c(0, 1)` (in standard state order), then the
position of `ctx` in `x` is 3.

Positions include all the occurrences of a context, including an
occurrence at the end of the time series (`t=length(x)`), which is not
followed by any value. Such an occurrence is not included in the
frequencies, which count the values that follow the context: the number
of positions of a context can therefore exceed its frequency by one.

## State order in a context

Notice that contexts are given by default in the temporal order and not
in the "reverse" order used by many VLMC research papers: older values
are on the left. For instance, the context `c(1, 0)` is reported if the
sequence 0, then 1 appeared in the time series used to build the context
tree. Set `reverse` to `TRUE` for the reverse convention which is
somewhat easier to relate to the way the context trees are represented
by
[`draw()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/draw.md)
(i.e. recent values at the top of the tree).

## See also

[`find_sequence()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/find_sequence.md)
and
[`find_sequence.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/find_sequence.covlmc.md)
for direct access to a specific context, and
[`contexts.ctx_tree()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.ctx_tree.md),
`contexts.vlmc()` and
[`contexts.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.covlmc.md)
for concrete implementations of
[`contexts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.md).

## Examples

``` r
set.seed(0)
rdts <- sample(as.factor(c("A", "B", "C")), 100, replace = TRUE)
model <- vlmc(rdts, alpha = 0.5)
## direct representation with ctx_node objects
model_ctxs <- contexts(model)
model_ctxs
#> Contexts:
#>  B, A, C, A
#>  A, C, A
#>  C, C, A
#>  C, A
#>  C, A, A
#>  C, B, A, A
#>  A, B, A, A
#>  B, A, A
#>  C, A, A, A
#>  A, A, A, A
#>  A, A, A
#>  B, B, A
#>  A, C, B, A
#>  C, B, A
#>  A, A, B, A
#>  A, B, A
#>  A, B
#>  A, B, B
#>  C, B, B
#>  A, B, B, B
#>  C, B, B, B
#>  B, B, B, B
#>  A, A, A, C, B
#>  A, A, C, B
#>  A, C, B
#>  B, C, B
#>  C, C, B
#>  C, A, C
#>  A, A, C
#>  B, A, C
#>  B, A, C, C
#>  A, C, C
#>  C, C
#>  C, B, B, C
#>  B, B, B, C
#>  B, B, C
#>  C, C, B, C
#>  C, B, C
#>  B, C
sapply(model_ctxs, cutoff, scale = "quantile")
#>  [1] 0.4444444 0.6297376 0.3265306 0.8602766 0.2569599 0.2500000 0.2500000
#>  [8] 0.5663605 0.3542274 0.1574344 0.9797106 0.1687500 0.4444444 0.5400000
#> [15] 0.4444444 0.5400000 0.1457938 0.1457938 0.2938408 0.3240000 0.3240000
#> [22] 0.1296000 0.4444444 0.8640000 0.7629395 0.5000000 0.2260561 0.4958678
#> [29] 0.2428492 0.4911585 0.4444444 0.4629630 0.6512278 0.1250000 0.1250000
#> [36] 0.8128842 0.4444444 0.4629630 0.3768679
sapply(model_ctxs, cutoff, scale = "native")
#>  [1] 0.81093022 0.46245204 1.11923158 0.15050126 1.35883542 1.38629436
#>  [7] 1.38629436 0.56852449 1.03781618 1.84874640 0.02049806 1.77933695
#> [13] 0.81093022 0.61618614 0.81093022 0.61618614 1.92556166 1.92556166
#> [19] 1.22471710 1.12701176 1.12701176 2.04330250 0.81093022 0.14618251
#> [25] 0.27057660 0.69314718 1.48697193 0.70144598 1.41531449 0.71098846
#> [31] 0.81093022 0.77010822 0.42889583 2.07944154 2.07944154 0.20716660
#> [37] 0.81093022 0.77010822 0.97586050
sapply(model_ctxs, function(x) metrics(x)$accuracy)
#>  [1] 0.5000000 1.0000000 1.0000000 0.5000000 0.7500000 1.0000000 0.5000000
#>  [8]       NaN 0.6666667 0.6666667 1.0000000 0.6666667 1.0000000 0.0000000
#> [15] 1.0000000 0.0000000 0.5000000 1.0000000 0.5000000 0.6666667 0.6666667
#> [22] 0.5000000 0.5000000 0.0000000 0.5000000 0.5000000 0.6000000 0.5000000
#> [29] 0.7500000 0.4000000 1.0000000 0.0000000 0.5000000 1.0000000 0.6666667
#> [36]       NaN 1.0000000 0.0000000       NaN
## data.frame format
contexts(model, frequency = "total")
#>         context freq
#> 1    B, A, C, A    2
#> 2       A, C, A    3
#> 3       C, C, A    2
#> 4          C, A    7
#> 5       C, A, A    4
#> 6    C, B, A, A    2
#> 7    A, B, A, A    2
#> 8       B, A, A    4
#> 9    C, A, A, A    3
#> 10   A, A, A, A    3
#> 11      A, A, A    7
#> 12      B, B, A    3
#> 13   A, C, B, A    2
#> 14      C, B, A    3
#> 15   A, A, B, A    2
#> 16      A, B, A    3
#> 17         A, B    6
#> 18      A, B, B    3
#> 19      C, B, B    6
#> 20   A, B, B, B    3
#> 21   C, B, B, B    3
#> 22   B, B, B, B    4
#> 23 A, A, A,....    2
#> 24   A, A, C, B    3
#> 25      A, C, B    5
#> 26      B, C, B    2
#> 27      C, C, B    5
#> 28      C, A, C    2
#> 29      A, A, C    4
#> 30      B, A, C    5
#> 31   B, A, C, C    2
#> 32      A, C, C    3
#> 33         C, C    9
#> 34   C, B, B, C    3
#> 35   B, B, B, C    3
#> 36      B, B, C    6
#> 37   C, C, B, C    2
#> 38      C, B, C    3
#> 39         B, C    9
contexts(model, cutoff = "quantile")
#>         context    cutoff
#> 1    B, A, C, A 0.4444444
#> 2       A, C, A 0.6297376
#> 3       C, C, A 0.3265306
#> 4          C, A 0.8602766
#> 5       C, A, A 0.2569599
#> 6    C, B, A, A 0.2500000
#> 7    A, B, A, A 0.2500000
#> 8       B, A, A 0.5663605
#> 9    C, A, A, A 0.3542274
#> 10   A, A, A, A 0.1574344
#> 11      A, A, A 0.9797106
#> 12      B, B, A 0.1687500
#> 13   A, C, B, A 0.4444444
#> 14      C, B, A 0.5400000
#> 15   A, A, B, A 0.4444444
#> 16      A, B, A 0.5400000
#> 17         A, B 0.1457938
#> 18      A, B, B 0.1457938
#> 19      C, B, B 0.2938408
#> 20   A, B, B, B 0.3240000
#> 21   C, B, B, B 0.3240000
#> 22   B, B, B, B 0.1296000
#> 23 A, A, A,.... 0.4444444
#> 24   A, A, C, B 0.8640000
#> 25      A, C, B 0.7629395
#> 26      B, C, B 0.5000000
#> 27      C, C, B 0.2260561
#> 28      C, A, C 0.4958678
#> 29      A, A, C 0.2428492
#> 30      B, A, C 0.4911585
#> 31   B, A, C, C 0.4444444
#> 32      A, C, C 0.4629630
#> 33         C, C 0.6512278
#> 34   C, B, B, C 0.1250000
#> 35   B, B, B, C 0.1250000
#> 36      B, B, C 0.8128842
#> 37   C, C, B, C 0.4444444
#> 38      C, B, C 0.4629630
#> 39         B, C 0.3768679
contexts(model, cutoff = "native", metrics = TRUE)
#>         context     cutoff  accuracy auc
#> 1    B, A, C, A 0.81093022 0.5000000  NA
#> 2       A, C, A 0.46245204 1.0000000  NA
#> 3       C, C, A 1.11923158 1.0000000  NA
#> 4          C, A 0.15050126 0.5000000  NA
#> 5       C, A, A 1.35883542 0.7500000  NA
#> 6    C, B, A, A 1.38629436 1.0000000  NA
#> 7    A, B, A, A 1.38629436 0.5000000  NA
#> 8       B, A, A 0.56852449       NaN  NA
#> 9    C, A, A, A 1.03781618 0.6666667  NA
#> 10   A, A, A, A 1.84874640 0.6666667  NA
#> 11      A, A, A 0.02049806 1.0000000  NA
#> 12      B, B, A 1.77933695 0.6666667  NA
#> 13   A, C, B, A 0.81093022 1.0000000  NA
#> 14      C, B, A 0.61618614 0.0000000  NA
#> 15   A, A, B, A 0.81093022 1.0000000  NA
#> 16      A, B, A 0.61618614 0.0000000  NA
#> 17         A, B 1.92556166 0.5000000  NA
#> 18      A, B, B 1.92556166 1.0000000  NA
#> 19      C, B, B 1.22471710 0.5000000  NA
#> 20   A, B, B, B 1.12701176 0.6666667  NA
#> 21   C, B, B, B 1.12701176 0.6666667  NA
#> 22   B, B, B, B 2.04330250 0.5000000  NA
#> 23 A, A, A,.... 0.81093022 0.5000000  NA
#> 24   A, A, C, B 0.14618251 0.0000000  NA
#> 25      A, C, B 0.27057660 0.5000000  NA
#> 26      B, C, B 0.69314718 0.5000000  NA
#> 27      C, C, B 1.48697193 0.6000000  NA
#> 28      C, A, C 0.70144598 0.5000000  NA
#> 29      A, A, C 1.41531449 0.7500000  NA
#> 30      B, A, C 0.71098846 0.4000000  NA
#> 31   B, A, C, C 0.81093022 1.0000000  NA
#> 32      A, C, C 0.77010822 0.0000000  NA
#> 33         C, C 0.42889583 0.5000000  NA
#> 34   C, B, B, C 2.07944154 1.0000000  NA
#> 35   B, B, B, C 2.07944154 0.6666667  NA
#> 36      B, B, C 0.20716660       NaN  NA
#> 37   C, C, B, C 0.81093022 1.0000000  NA
#> 38      C, B, C 0.77010822 0.0000000  NA
#> 39         B, C 0.97586050       NaN  NA
```
