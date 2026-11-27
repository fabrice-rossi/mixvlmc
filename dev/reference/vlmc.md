# Fit a Variable Length Markov Chain (VLMC)

This function fits a Variable Length Markov Chain (VLMC) to a discrete
time series.

## Usage

``` r
vlmc(
  x,
  alpha = 0.05,
  cutoff = NULL,
  min_size = 2L,
  max_depth = 100L,
  prune = TRUE,
  keep_match = FALSE,
  backend = getOption("mixvlmc.backend", "C++")
)
```

## Arguments

- x:

  an object that can be interpreted as a discrete time series: a
  numeric, character, factor or logical vector, or a `dts` object (see
  [`dts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/dts.md)).
  Its state space must contain at least two states.

- alpha:

  number in (0,1\] (default: 0.05) cut off value in quantile scale in
  the pruning phase.

- cutoff:

  non negative number: cut off value in native (log likelihood ratio)
  scale in the pruning phase. Defaults to the value obtained from
  `alpha`. Takes precedence over `alpha` if specified.

- min_size:

  integer \>= 1 (default: 2). Minimum number of observations for a
  context in the growing phase of the context tree.

- max_depth:

  integer \>= 0 (default: 100). Longest context considered in the
  growing phase of the context tree.

- prune:

  logical: specifies whether the context tree should be pruned (default:
  `TRUE`).

- keep_match:

  logical: specifies whether to keep the positions of the contexts in
  the time series (default: `FALSE`).

- backend:

  "R" or "C++" (default: as specified by the "mixvlmc.backend" option).
  Specifies the implementation used to represent the context tree and to
  build it. See the "Back ends" section.

## Value

a fitted VLMC model.

## Details

The VLMC is built using Bühlmann and Wyner's algorithm which consists of
fitting a context tree (see
[`ctx_tree()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/ctx_tree.md))
to a time series and then pruning it in such a way that the conditional
distribution of the next state of the time series given the context is
significantly different from the distribution given a truncated version
of the context.

The construction of the context tree is controlled by `min_size` and
`max_depth`, exactly as in
[`ctx_tree()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/ctx_tree.md).
Significance is measured using a likelihood ratio test whose threshold
can be specified either in terms of the ratio itself with `cutoff` or in
quantile scale with `alpha`.

Pruning can be postponed by setting `prune=FALSE`. Using a combination
of
[`cutoff()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.md)
and
[`prune()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/prune.md),
the complexity of the VLMC can then be adjusted. Any VLMC model can be
pruned after construction: `prune=FALSE` is a convenience parameter to
avoid setting `alpha=1` (which essentially prevents any pruning).
Automated model selection is provided by
[`tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/tune_vlmc.md).

## Back ends

Two back ends are available to compute context trees:

- the "R" back end represents the tree in pure R data structures (nested
  lists) that can be easily processed further in pure R (C++ helper
  functions are used to speed up the construction).

- the "C++" back end (the default one) represents the tree with C++
  classes. The tree is built with an optimised suffix tree algorithm
  which speeds up the construction by a factor of at least 10 in
  standard settings. As the tree is kept outside of the direct reach of
  R, context trees built with the C++ back end must be restored after a
  [`saveRDS()`](https://rdrr.io/r/base/readRDS.html)/[`readRDS()`](https://rdrr.io/r/base/readRDS.html)
  sequence. This is done automatically by recomputing the context tree
  completely.

## References

Bühlmann, P. and Wyner, A. J. (1999), "Variable length Markov chains."
Ann. Statist. 27 (2) 480-513
[doi:10.1214/aos/1018031204](https://doi.org/10.1214/aos/1018031204)

## See also

[`cutoff()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.md),
[`prune()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/prune.md)
and
[`tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/tune_vlmc.md)

## Examples

``` r
pc <- powerconsumption[powerconsumption$week == 5, ]
rdts <- cut(pc$active_power,
  breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1)))
)
model <- vlmc(rdts)
draw(model)
#> ▪ (0.25, 0.25, 0.25, 0.25)
#> ├─ (0,0.458] (0.7968, 0.1912, 0.007968, 0.003984)
#> │  └─ (0,0.458] (0.809, 0.1809, 0.005025, 0.005025)
#> │     └─ (0,0.458] (0.8188, 0.1688, 0.00625, 0.00625)
#> │        └─ (0,0.458] (0.8462, 0.1385, 0.007692, 0.007692)
#> │           └─ (0.458,1.34] (0.55, 0.4, 0, 0.05)
#> ├─ (0.458,1.34] (0.1984, 0.6667, 0.1071, 0.02778)
#> │  ├─ (0,0.458] (0.375, 0.4167, 0.1458, 0.0625)
#> │  │  └─ (0,0.458] (0.3611, 0.4444, 0.1389, 0.05556)
#> │  │     └─ (0.458,1.34] (0, 0.8889, 0.1111, 0)
#> │  └─ (0.458,1.34] (0.1369, 0.75, 0.09524, 0.01786)
#> │     └─ (0,0.458] (0.4, 0.55, 0.05, 0)
#> ├─ (1.34,2.13] (0.003968, 0.123, 0.7262, 0.1468)
#> │  └─ (2.13,7.54] (0, 0.2, 0.525, 0.275)
#> └─ (2.13,7.54] (0.003968, 0.01587, 0.1587, 0.8214)
#>    └─ (0.458,1.34] (0, 0.1429, 0.5714, 0.2857)
depth(model)
#> [1] 5
## reduce the depth of the model
shallow_model <- vlmc(rdts, max_depth = 3)
draw(shallow_model, prob = FALSE)
#> ▪ (252, 252, 252, 252)
#> ├─ (0,0.458] (200, 48, 2, 1)
#> ├─ (0.458,1.34] (50, 168, 27, 7)
#> │  ├─ (0,0.458] (18, 20, 7, 3)
#> │  └─ (0.458,1.34] (23, 126, 16, 3)
#> │     └─ (0,0.458] (8, 11, 1, 0)
#> ├─ (1.34,2.13] (1, 31, 183, 37)
#> │  └─ (2.13,7.54] (0, 8, 21, 11)
#> └─ (2.13,7.54] (1, 4, 40, 207)
#>    └─ (0.458,1.34] (0, 1, 4, 2)
## improve probability estimates
robust_model <- vlmc(rdts, min_size = 25)
draw(robust_model, prob = FALSE) ## show the frequencies
#> ▪ (252, 252, 252, 252)
#> ├─ (0,0.458] (200, 48, 2, 1)
#> ├─ (0.458,1.34] (50, 168, 27, 7)
#> │  └─ (0,0.458] (18, 20, 7, 3)
#> ├─ (1.34,2.13] (1, 31, 183, 37)
#> │  └─ (2.13,7.54] (0, 8, 21, 11)
#> └─ (2.13,7.54] (1, 4, 40, 207)
draw(robust_model)
#> ▪ (0.25, 0.25, 0.25, 0.25)
#> ├─ (0,0.458] (0.7968, 0.1912, 0.007968, 0.003984)
#> ├─ (0.458,1.34] (0.1984, 0.6667, 0.1071, 0.02778)
#> │  └─ (0,0.458] (0.375, 0.4167, 0.1458, 0.0625)
#> ├─ (1.34,2.13] (0.003968, 0.123, 0.7262, 0.1468)
#> │  └─ (2.13,7.54] (0, 0.2, 0.525, 0.275)
#> └─ (2.13,7.54] (0.003968, 0.01587, 0.1587, 0.8214)
```
