# Convert an object to a Variable Length Markov Chain with covariates (COVLMC)

This generic function converts an object into a VLMC with covariates (a
`covlmc` object).

## Usage

``` r
as_covlmc(x, ...)

# S3 method for class 'tune_covlmc'
as_covlmc(x, ...)
```

## Arguments

- x:

  an object to convert into a `covlmc` object, for instance the result
  of
  [`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_covlmc.md).

- ...:

  additional arguments for conversion functions.

## Value

a `covlmc` object. For a `tune_covlmc` object, this is the best model
selected by
[`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_covlmc.md).

## See also

[`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_covlmc.md)

## Examples

``` r
## conversion from the results of tune_covlmc()
pc <- powerconsumption[powerconsumption$week == 5, ]
rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
rdts_best_model_tune <- tune_covlmc(rdts, rdts_cov)
rdts_best_model <- as_covlmc(rdts_best_model_tune)
draw(rdts_best_model)
#> ▪
#> ├─ (0,1.34] (0.9264, 0.07356)
#> └─ (1.34,7.54] (0.07341, 0.9266)
```
