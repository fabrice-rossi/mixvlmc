# Log-Likelihood of a VLMC

This function evaluates the log-likelihood of a VLMC fitted on a
discrete time series.

## Usage

``` r
# S3 method for class 'vlmc'
logLik(object, initial = c("truncated", "specific", "extended"), ...)

# S3 method for class 'vlmc_cpp'
logLik(object, initial = c("truncated", "specific", "extended"), ...)
```

## Arguments

- object:

  a fitted VLMC.

- initial:

  specifies the likelihood function, more precisely the way the first
  few observations for which contexts cannot be calculated are
  integrated in the likelihood. Defaults to `"truncated"`. See
  [`loglikelihood()`](https://fabrice-rossi.github.io/mixvlmc/reference/loglikelihood.md)
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

[`loglikelihood()`](https://fabrice-rossi.github.io/mixvlmc/reference/loglikelihood.md)

## Examples

``` r
pc <- powerconsumption[powerconsumption$week == 5, ]
breaks <- c(
  0,
  median(powerconsumption$active_power, na.rm = TRUE),
  max(powerconsumption$active_power, na.rm = TRUE)
)
labels <- c(0, 1)
rdts <- cut(pc$active_power, breaks = breaks, labels = labels)
m_nocovariate <- vlmc(rdts)
ll <- logLik(m_nocovariate)
ll
#> 'log Lik.' -207.9581 (df=69)
attributes(ll)
#> $nobs
#> [1] 945
#> 
#> $df
#> [1] 69
#> 
#> $initial
#> [1] "truncated"
#> 
#> $class
#> [1] "logLik"
#> 
## log-likelihood with the extended likelihood function
logLik(m_nocovariate, initial = "extended")
#> 'log Lik.' -223.4258 (df=71)
## logLik() enables the use of AIC() and BIC()
AIC(m_nocovariate)
#> [1] 553.9162
BIC(m_nocovariate)
#> [1] 888.648
```
