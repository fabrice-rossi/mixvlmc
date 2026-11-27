# Merging status of a COVLMC context

The function returns `TRUE` if the context represented by this node is
merged with at least one other context and `FALSE` if this is not the
case.

## Usage

``` r
is_merged(node)
```

## Arguments

- node:

  A `ctx_node_covlmc` object as returned by
  [`find_sequence.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/find_sequence.covlmc.md)
  or
  [`contexts.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.covlmc.md)

## Value

`TRUE` or `FALSE`, depending on the nature of the context

## Details

When a COVLMC is built on a time series with at least three distinct
states, some contexts can be merged: they use the same logistic model,
leading to a more parsimonious model. Those contexts are reported
individually by functions such as
[`contexts.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.covlmc.md).
The present function can be used to detect such merging, while
[`merged_with()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/merged_with.md)
can be used to recover the other contexts.

## See also

[`merged_with()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/merged_with.md)

## Examples

``` r
pc <- powerconsumption[powerconsumption$week == 15, ]
rdts <- cut(pc$active_power, breaks = c(0, 1, 2, 3, 8))
rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
m_cov <- covlmc(rdts, rdts_cov, min_size = 5, alpha = 0.1)
ctxs <- contexts(m_cov)
## no merging
sapply(ctxs, is_merged)
#> [1] FALSE FALSE FALSE FALSE
```
