# Covariate memory length for a COVLMC context

This function returns the length of the covariate memory of a COVLMC
context represented by a `ctx_node_covlmc` object, i.e. the number of
past values of the covariates used by the logistic model of the context.

## Usage

``` r
covariate_memory(node)
```

## Arguments

- node:

  A `ctx_node_covlmc` object as returned by
  [`find_sequence.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/find_sequence.covlmc.md)
  or
  [`contexts.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.covlmc.md)

## Value

the memory length, an integer (`NULL` if `node` is not a context)

## See also

[`covariate_depth()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/covariate_depth.md)

## Examples

``` r
pc <- powerconsumption[powerconsumption$week == 5, ]
rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
m_cov <- covlmc(rdts, rdts_cov, min_size = 10)
ctxs <- contexts(m_cov)
## get all the memory lengths
sapply(ctxs, covariate_memory)
#> [1] 2 0 0
```
