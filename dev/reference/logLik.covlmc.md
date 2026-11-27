# Log-Likelihood of a VLMC with covariates

This function evaluates the log-likelihood of a VLMC with covariates
fitted on a discrete time series.

## Usage

``` r
# S3 method for class 'covlmc'
logLik(object, initial = c("truncated", "specific", "extended"), ...)
```

## Arguments

- object:

  a fitted VLMC with covariates (a `covlmc` object).

- initial:

  specifies the likelihood function, more precisely the way the first
  few observations for which contexts cannot be calculated are
  integrated in the likelihood. Defaults to `"truncated"`. See
  [`loglikelihood()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/loglikelihood.md)
  for details.

- ...:

  additional parameters (not used).

## Value

an object of class `logLik`. This is a number, the log-likelihood of the
(CO)VLMC with the following attributes:

- `df`: the number of parameters used by the VLMC for this likelihood
  calculation

- `nobs`: the number of observations included in this likelihood
  calculation

- `initial`: the value of the `initial` parameter used to compute this
  likelihood

## See also

[`loglikelihood()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/loglikelihood.md)

## Examples

``` r

## Likelihood for a fitted VLMC with covariates.
pc <- powerconsumption[powerconsumption$week == 5, ]
breaks <- c(
  0,
  median(powerconsumption$active_power, na.rm = TRUE),
  max(powerconsumption$active_power, na.rm = TRUE)
)
labels <- c(0, 1)
rdts <- cut(pc$active_power, breaks = breaks, labels = labels)
rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
m_cov <- covlmc(rdts, rdts_cov, min_size = 5)
ll <- logLik(m_cov)
attributes(ll)
#> $nobs
#> [1] 1005
#> 
#> $df
#> [1] 5
#> 
#> $initial
#> [1] "truncated"
#> 
#> $class
#> [1] "logLik"
#> 
```
