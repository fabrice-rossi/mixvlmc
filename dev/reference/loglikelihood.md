# Log-Likelihood of a VLMC

This function evaluates the log-likelihood of a VLMC fitted on a
discrete time series. When the optional argument `newdata` is provided,
the function evaluates instead the log-likelihood for this (new)
discrete time series.

## Usage

``` r
loglikelihood(
  vlmc,
  newdata,
  initial = c("truncated", "specific", "extended"),
  ignore,
  ...
)

# S3 method for class 'vlmc'
loglikelihood(
  vlmc,
  newdata,
  initial = c("truncated", "specific", "extended"),
  ignore,
  ...
)

# S3 method for class 'vlmc_cpp'
loglikelihood(
  vlmc,
  newdata,
  initial = c("truncated", "specific", "extended"),
  ignore,
  ...
)
```

## Arguments

- vlmc:

  a fitted VLMC.

- newdata:

  an optional object that can be interpreted as a discrete time series
  (for instance a `dts` object), with values in the state space of the
  model.

- initial:

  specifies the likelihood function, more precisely the way the first
  few observations for which contexts cannot be calculated are
  integrated in the likelihood. Defaults to `"truncated"`. See below for
  details.

- ignore:

  specifies the number of initial values for which the log-likelihood
  will not be computed. The minimal number depends on the likelihood
  function as detailed below.

- ...:

  additional parameters (not used).

## Value

an object of class `logLikMixVLMC` and `logLik`. This is a number, the
log-likelihood of the (CO)VLMC with the following attributes:

- `df`: the number of parameters used by the VLMC for this likelihood
  calculation

- `nobs`: the number of observations included in this likelihood
  calculation

- `initial`: the value of the `initial` parameter used to compute this
  likelihood

## Details

The definition of the likelihood function depends on the value of the
`initial` parameter, see the section below as well as the dedicated
vignette:
[`vignette("likelihood", package = "mixvlmc")`](https://fabrice-rossi.github.io/mixvlmc/dev/articles/likelihood.md).

For VLMC objects, the method `loglikelihood.vlmc` will be used. For VLMC
with covariates, `loglikelihood.covlmc` will instead be called. For more
information on `loglikelihood` methods, use `methods(loglikelihood)` and
their associated documentation.

## Likelihood calculation

In a (CO)VLMC of
[`depth()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/depth.md)=k,
we need k past values in order to compute the context of a given
observation. As a consequence, in a time series `x`, the contexts of
`x[1]` to `x[k]` are unknown. Depending on the value of `initial`
different likelihood functions are used to tackle this difficulty:

- `initial=="truncated"` (default): the likelihood is computed using
  only `x[(k+1):length(x)]`

- `initial=="specific"`: the likelihood is computed on the full time
  series using a specific context for the initial values, `x[1]` to
  `x[k]`. Each of the specific contexts is unique, leading to a perfect
  likelihood of 1 (0 in log scale). Thus the numerical value of the
  likelihood is identical to the one obtained with
  `initial=="truncated"` but it is computed on `length(x)` observations
  with a model with more parameters than in this previous case.

- `initial=="extended"`: the likelihood is computed on the full time
  series using an extended context matching for the initial values,
  `x[1]` to `x[k]`. This can be seen as a compromise between the two
  other possibilities: the relaxed context matching needs in general to
  turn internal nodes of the context tree into actual contexts,
  increasing the number of parameters, but not as much as with
  "specific". However, the likelihood of say `x[1]` with an empty
  context is generally not 1 and thus the full likelihood is smaller
  than the one computed with "specific".

In all cases, the first `ignore` values of the time series are not
included in the computed likelihood, but are still used to compute
contexts. If `ignore` is not specified, it is set to the minimal
possible value, that is k for the `truncated` likelihood and 0 for the
other ones. If it is specified, it must be larger than or equal to k for
`truncated`. When `newdata` is not specified, `ignore` cannot be larger
than k.

When `newdata` contains a transition that has a zero probability
according to the model (in a context or, for the `extended` likelihood,
in an extended context), the log-likelihood is `-Inf`.

See the dedicated vignette for a more mathematically oriented
discussion:
[`vignette("likelihood", package = "mixvlmc")`](https://fabrice-rossi.github.io/mixvlmc/dev/articles/likelihood.md).

## See also

[`stats::logLik()`](https://rdrr.io/r/stats/logLik.html)

## Examples

``` r
## Likelihood for a fitted VLMC.
pc <- powerconsumption[powerconsumption$week == 5, ]
breaks <- c(
  0,
  median(powerconsumption$active_power, na.rm = TRUE),
  max(powerconsumption$active_power, na.rm = TRUE)
)
labels <- c(0, 1)
rdts <- cut(pc$active_power, breaks = breaks, labels = labels)
m_nocovariate <- vlmc(rdts)
ll <- loglikelihood(m_nocovariate)
ll
#> 'log Lik.' -207.9581 (df= 69, nb obs.= 945, initial="truncated")
attr(ll, "nobs")
#> [1] 945
attr(ll, "df")
#> [1] 69

## Likelihood for a new time series with previously fitted VLMC.
pc_new <- powerconsumption[powerconsumption$week == 11, ]
rdts_new <- cut(pc_new$active_power, breaks = breaks, labels = labels)
ll_new <- loglikelihood(m_nocovariate, newdata = rdts_new)
ll_new
#> 'log Lik.' -240.5671 (df= 69, nb obs.= 945, initial="truncated")
attributes(ll_new)
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
#> [1] "logLikMixVLMC" "logLik"       
#> 
ll_new_specific <- loglikelihood(m_nocovariate,
  initial = "specific",
  newdata = rdts_new
)
ll_new_specific
#> 'log Lik.' -240.5671 (df= 132, nb obs.= 1008, initial="specific")
attributes(ll_new_specific)
#> $nobs
#> [1] 1008
#> 
#> $df
#> [1] 132
#> 
#> $initial
#> [1] "specific"
#> 
#> $class
#> [1] "logLikMixVLMC" "logLik"       
#> 
ll_new_extended <- loglikelihood(m_nocovariate,
  initial = "extended",
  newdata = rdts_new
)
ll_new_extended
#> 'log Lik.' -254.0705 (df= 71, nb obs.= 1008, initial="extended")
attributes(ll_new_extended)
#> $nobs
#> [1] 1008
#> 
#> $df
#> [1] 71
#> 
#> $initial
#> [1] "extended"
#> 
#> $class
#> [1] "logLikMixVLMC" "logLik"       
#> 
```
