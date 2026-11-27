# Prune a Variable Length Markov Chain with covariates

This function prunes a VLMC with covariates. This model must have been
estimated with `keep_data=TRUE` to enable the pruning (and must not have
been trimmed, see
[`trim.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/trim.covlmc.md)).

## Usage

``` r
# S3 method for class 'covlmc'
prune(vlmc, alpha = 0.05, cutoff = NULL, ...)
```

## Arguments

- vlmc:

  a fitted VLMC with covariates (a `covlmc` object).

- alpha:

  number in (0,1\] (default: 0.05) cut off value in quantile scale for
  pruning. A warning is issued if it is larger than the `alpha` of the
  model, as the result may then differ from a model built directly with
  this value.

- cutoff:

  not supported for VLMC with covariates (must be `NULL`): pruning is
  specified only with `alpha`.

- ...:

  additional arguments for the prune function.

## Value

a pruned `covlmc` object.

## Details

Post-pruning a VLMC with covariates is not as straightforward as the
same procedure applied to
[`vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/vlmc.md)
(see
[`cutoff.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.vlmc.md)
and
[`prune.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/prune.md)).
For efficiency reasons,
[`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/covlmc.md)
estimates only the logistic models that are considered useful for a
given set of construction parameters. With a more aggressive pruning
threshold, some contexts become leaves of the context tree and new
logistic models must be estimated. Thus the pruning opportunities given
by
[`cutoff.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.covlmc.md)
are only a subset of interesting cut offs for a given COVLMC.

Contrarily to the case of
[`vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/vlmc.md),
post-pruning a COVLMC does not always give the same model as building
the COVLMC directly with the post-pruning alpha (even if it is smaller
than the alpha used to build the initial model). Indeed the pruning
decisions depend on the logistic models available in the initial COVLMC.
For the same reason, successive pruning operations (as in
[`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/tune_covlmc.md))
can give a different model than a single pruning operation with the
final alpha.

## Examples

``` r
pc <- powerconsumption[powerconsumption$week == 5, ]
rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
m_cov <- covlmc(rdts, rdts_cov, min_size = 5, keep_data = TRUE)
draw(m_cov)
#> ▪
#> ├─ (0,1.34]
#> │  ├─ (0,1.34]
#> │  │  ├─ (0,1.34] ([ -2.813 • 2.178 ⁞ -4.449 ⁞ 2.418 ])
#> │  │  └─ (1.34,7.54] ([ -19.57 • 18.39 ])
#> │  └─ (1.34,7.54] (0.8649, 0.1351)
#> └─ (1.34,7.54] (0.07341, 0.9266)
m_cov_cuts <- cutoff(m_cov)
p_cov <- prune(m_cov, alpha = m_cov_cuts[1])
draw(p_cov)
#> ▪
#> ├─ (0,1.34]
#> │  ├─ (0,1.34] ([ -2.759 • 2.785 ⁞ -2.525 ])
#> │  └─ (1.34,7.54] (0.8649, 0.1351)
#> └─ (1.34,7.54] (0.07341, 0.9266)
```
