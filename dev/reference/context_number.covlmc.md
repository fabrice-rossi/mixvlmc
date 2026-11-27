# Number of contexts of a VLMC with covariates

This function returns the total number of contexts of a VLMC with
covariates.

## Usage

``` r
# S3 method for class 'covlmc'
context_number(ct)
```

## Arguments

- ct:

  a fitted VLMC with covariates (a `covlmc` object).

## Value

the number of contexts present in the VLMC with covariates.

## Examples

``` r
pc <- powerconsumption[powerconsumption$week == 5, ]
dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
m_cov <- covlmc(dts, dts_cov, min_size = 10)
# should be 3
context_number(m_cov)
#> [1] 3
```
