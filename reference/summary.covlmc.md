# Summary of a VLMC with covariates

This function computes summary statistics of a VLMC with covariates,
which are displayed by the associated
[`print()`](https://rdrr.io/r/base/print.html) method.

## Usage

``` r
# S3 method for class 'covlmc'
summary(object, ...)

# S3 method for class 'summary.covlmc'
print(x, ...)
```

## Arguments

- object:

  a fitted VLMC with covariates (a `covlmc` object).

- ...:

  additional arguments (not used).

- x:

  a summary of a VLMC with covariates, as returned by
  [`summary()`](https://rdrr.io/r/base/summary.html).

## Value

[`summary()`](https://rdrr.io/r/base/summary.html) returns an object of
class `summary.covlmc`, a list with the following components:

- `state_space`: the state space of the model;

- `nb`: the number of contexts;

- `depth`: the length of the longest context;

- `avg_depth`: the average length of the contexts;

- `alpha`: the cut off of the model in quantile scale;

- `covariates`: the names of the covariates;

- `no_cov`: the number of contexts whose model does not use the
  covariates;

- `avg_hsize`: the average length of the memory of the covariates (see
  [`covariate_memory()`](https://fabrice-rossi.github.io/mixvlmc/reference/covariate_memory.md)),
  computed on the contexts whose model uses the covariates (`NaN` if
  there is none);

- `max_hsize`: the longest memory of the covariates (see
  [`covariate_depth()`](https://fabrice-rossi.github.io/mixvlmc/reference/covariate_depth.md));

- `delta`: the average difference between the length of a context and
  the memory of its model, computed on the contexts whose model uses the
  covariates (`NaN` if there is none).

[`print()`](https://rdrr.io/r/base/print.html) displays the summary and
returns `x` invisibly.

## See also

[`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc.md),
[`contexts.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.covlmc.md),
[`context_number.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/context_number.covlmc.md),
[`depth()`](https://fabrice-rossi.github.io/mixvlmc/reference/depth.md),
[`covariate_depth()`](https://fabrice-rossi.github.io/mixvlmc/reference/covariate_depth.md)

## Examples

``` r
pc <- powerconsumption[powerconsumption$week == 5, ]
rdts <- cut(pc$active_power,
  breaks = c(0, quantile(pc$active_power, probs = c(1 / 3, 2 / 3, 1)))
)
rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
m_cov <- covlmc(rdts, rdts_cov, min_size = 5)
summary(m_cov)
#> VLMC with covariates context tree on (0,0.556], (0.556,1.78], (1.78,7.54] 
#>  Covariates: day_night 
#>  Cutoff in quantile scale: 0.05
#>  Number of contexts: 5 
#>  Contexts without covariate: 3 
#>  Maximum context length: 2 
#>  Average context length: 1.6 
#>  For nodes with covariates: 
#>   Maximum covariate history length: 2 
#>   Average covariate history length: 1.5 
#>   Average difference between context length and history length: 0 
```
