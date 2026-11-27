# Cut off values for pruning a node in the context tree of a COVLMC

This function returns the cut off values associated with a specific node
in the context tree of a VLMC with covariates. The node is represented
by a `ctx_node_covlmc` object as returned by
[`find_sequence()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/find_sequence.md)
or
[`contexts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.md).

## Usage

``` r
# S3 method for class 'ctx_node_covlmc'
cutoff(model, raw = FALSE, tolerance = .Machine$double.eps^0.5, ...)
```

## Arguments

- model:

  a `ctx_node_covlmc` object as returned by
  [`find_sequence()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/find_sequence.md).

- raw:

  specifies whether the returned values should be limit values computed
  in the model or modified values that guarantee pruning (defaults to
  `FALSE`, see details).

- tolerance:

  specifies the minimum separation between two consecutive values of the
  cut off in quantile scale (before any transformation). See details.

- ...:

  additional arguments for the `cutoff` function.

## Value

a vector of cut off values in quantile scale (in decreasing order),
`NULL` if none can be computed.

## Details

The values are the p-values of the likelihood ratio tests associated
with the node (see
[`cutoff.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.covlmc.md)
for details), in quantile scale:

- for a context, the p-value of the test used to reduce the memory of
  its logistic model (when it can lead to pruning);

- for a context merged with other ones (see
  [`is_merged()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/is_merged.md)),
  the p-value of the test used to reduce the memory of the shared
  logistic model;

- for a node with children, the p-values of the tests used to collapse
  its children (i.e. to replace them by the node) and to merge some of
  its children.

As for
[`cutoff.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.covlmc.md),
the native scale is not available. The values obtained for all the nodes
are those reported by
[`cutoff.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.covlmc.md)
for the full model.

## See also

[`cutoff.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.covlmc.md),
[`prune.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/prune.covlmc.md)

## Examples

``` r
pc <- powerconsumption[powerconsumption$week %in% 15:16, ]
rdts <- cut(pc$active_power,
  breaks = c(0, 0.4, 2, 8),
  labels = c("low", "typical", "high")
)
rdts_cov <- data.frame(day = (pc$hour >= 7 & pc$hour <= 18))
m_cov <- covlmc(rdts, rdts_cov, alpha = 1e-7)
draw(m_cov, p_value = TRUE)
#> ▪ (merging (low and high): 8.121e-230)
#> ├─ low (‹0.03454› 0.8088, 0.1821, 0.009105)
#> ├─ typical
#> │  ├─ typical (collapsing: 3.101e-08)
#> │  │  ├─ low (‹0.2323› 0.4058, 0.5507, 0.04348)
#> │  │  ├─ typical (‹6.269e-05› 0.07481, 0.8672, 0.05802)
#> │  │  └─ high (‹0.6393› 0.05172, 0.8276, 0.1207)
#> │  └─ low, high (‹0.6996› 0.2408, 0.6702, 0.08901)
#> └─ high (‹0.776› 0, 0.1864, 0.8136)
## the root node has a merging p-value
cutoff(find_sequence(m_cov, character()))
#> [1] 8.120775e-230
## a node with a collapsing p-value
cutoff(find_sequence(m_cov, c("typical", "typical")))
#> [1] 3.100669e-08
cutoff(m_cov)
#> [1]  6.996220e-01  3.100669e-08 8.120775e-230
```
