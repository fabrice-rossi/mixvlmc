# Test if the object is a VLMC with covariates

This function returns `TRUE` for VLMC with covariates (`covlmc` objects)
and `FALSE` for other objects.

## Usage

``` r
is_covlmc(x)
```

## Arguments

- x:

  an R object.

## Value

`TRUE` for VLMC with covariates, `FALSE` for other objects.

## Examples

``` r
pc <- powerconsumption[powerconsumption$week == 5, ]
rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
m_cov <- covlmc(rdts, rdts_cov, min_size = 5)
# should be true
is_ctx_tree(m_cov)
#> [1] TRUE
# should be true
is_covlmc(m_cov)
#> [1] TRUE
# should be false
is_vlmc(m_cov)
#> [1] FALSE
```
