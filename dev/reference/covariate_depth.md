# Maximal covariate memory of a VLMC with covariates

This function returns the longest covariate memory used by a VLMC with
covariates, i.e. the largest number of past values of the covariates
used by the logistic models of its contexts (see
[`covariate_memory()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/covariate_memory.md)).

## Usage

``` r
covariate_depth(model)
```

## Arguments

- model:

  a fitted VLMC with covariates (a `covlmc` object)

## Value

the longest covariate memory of this model (an integer, 0 when no
logistic model uses the covariates)

## Examples

``` r
pc <- powerconsumption[powerconsumption$week == 5, ]
rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
m_cov <- covlmc(rdts, rdts_cov, min_size = 10)
covariate_depth(m_cov)
#> [1] 2
## memory of each context
sapply(contexts(m_cov), covariate_memory)
#> [1] 2 0 0
```
