# Cut off values for pruning the context tree of a VLMC with covariates

This function returns all the cut off values that should induce a
pruning of the context tree of a VLMC with covariates.

## Usage

``` r
# S3 method for class 'covlmc'
cutoff(model, raw = FALSE, tolerance = .Machine$double.eps^0.5, ...)
```

## Arguments

- model:

  a fitted COVLMC model.

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
`NULL` if none can be computed

## Details

Notice that the list of cut off values returned by the function is not
as complete as the one computed for a VLMC without covariates. Indeed,
pruning the COVLMC tree creates new pruning opportunities that are not
evaluated during the construction of the initial model, while all
pruning opportunities are computed during the construction of a VLMC
context tree. Nevertheless, the largest value returned by the function
is guaranteed to produce the least pruned tree consistent with the
reference one.

For large COVLMC, some cut off values can be almost identical, with a
difference of the order of the machine epsilon value. The `tolerance`
parameter is used to keep only values that are different enough. This is
done in the quantile scale, before transformations implemented when
`raw` is `FALSE`.

Notice that the native scale (log-likelihood ratio) is not directly
useful in COVLMC as the differences in model sizes are not constant
through the pruning process. As a consequence, unlike
[`cutoff.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/cutoff.vlmc.md),
this function does not provide a `scale` parameter: values are always
given in quantile scale (i.e. as p-values of the likelihood ratio
tests).

Setting `raw` to `TRUE` removes the small perturbations that are
subtracted from the p-values of the likelihood ratio tests computed from
the COVLMC (in quantile scale).

As automated model selection is provided by
[`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_covlmc.md),
the direct use of `cutoff` should be reserved to advanced exploration of
the set of trees that can be obtained from a complex one, e.g. to
implement model selection techniques that are not provided by
[`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_covlmc.md).

## See also

[`prune.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/prune.covlmc.md)
and
[`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_covlmc.md)

## Examples

``` r
pc <- powerconsumption[powerconsumption$week == 5, ]
rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
m_cov <- covlmc(rdts, rdts_cov, min_size = 5)
draw(m_cov)
#> ▪
#> ├─ (0,1.34]
#> │  ├─ (0,1.34]
#> │  │  ├─ (0,1.34] ([ -2.813 • 2.178 ⁞ -4.449 ⁞ 2.418 ])
#> │  │  └─ (1.34,7.54] ([ -19.57 • 18.39 ])
#> │  └─ (1.34,7.54] (0.8649, 0.1351)
#> └─ (1.34,7.54] (0.07341, 0.9266)
cutoff(m_cov)
#> [1] 0.02328831 0.01834317
## raw values are slightly larger than the default ones
cutoff(m_cov, raw = TRUE) - cutoff(m_cov)
#> [1] 3.469447e-18 3.469447e-18
## pruning with the largest cut off value
draw(prune(m_cov, alpha = cutoff(m_cov)[1]))
#> ▪
#> ├─ (0,1.34]
#> │  ├─ (0,1.34] ([ -2.759 • 2.785 ⁞ -2.525 ])
#> │  └─ (1.34,7.54] (0.8649, 0.1351)
#> └─ (1.34,7.54] (0.07341, 0.9266)
```
