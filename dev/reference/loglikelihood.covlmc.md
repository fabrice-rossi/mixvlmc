# Log-Likelihood of a VLMC with covariates

This function evaluates the log-likelihood of a VLMC with covariates
fitted on a discrete time series. When the optional argument `newdata`
is provided, the function evaluates instead the log-likelihood for this
(new) discrete time series on the new covariates which must be provided
through the `newcov` parameter.

## Usage

``` r
# S3 method for class 'covlmc'
loglikelihood(
  vlmc,
  newdata,
  initial = c("truncated", "specific", "extended"),
  ignore,
  newcov,
  ...
)
```

## Arguments

- vlmc:

  a fitted VLMC with covariates (a `covlmc` object).

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

- newcov:

  an optional data frame with the new values for the covariates
  (mandatory when `newdata` is provided, with one row per observation in
  `newdata`). Evaluating the likelihood on new data is not supported by
  fully trimmed models (see
  [`trim.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/trim.covlmc.md)).

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

## Number of parameters

The `df` attribute of the result is the number of parameters of the
logistic models of the contexts (see
[`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/covlmc.md)),
used by [`stats::AIC()`](https://rdrr.io/r/stats/AIC.html) and
[`stats::BIC()`](https://rdrr.io/r/stats/AIC.html). With `s` states, the
model of a context has:

- `s-1` intercepts, as the transition probabilities of a context of a
  VLMC (see
  [`loglikelihood.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/loglikelihood.md)),
  even if some states never follow the context in the data;

- `(k-1)*p` coefficients for the covariates, where `k` is the number of
  different states that follow the context in the data and `p` is the
  number of parameters of the covariates: for each time step of the
  memory of the model, one parameter per numerical or logical covariate
  and the number of levels minus one per factor covariate, counting only
  the levels present in the data of the context.

The second term is the number of coefficients actually estimated by the
computing engines (see the "Logistic models" section of
[`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/covlmc.md)),
which ignore the states that do not follow the context and the unused
levels of the factors: it does not depend on the engine. For instance, a
context followed by a single state has a constant model with `s-1`
parameters. Counting all the intercepts ensures that each context has a
cost in AIC and BIC, as in the theory of context tree selection:
otherwise contexts followed by a single state would have no cost,
favouring overly complex models.

The likelihood ratio tests used to build the model (see
[`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/covlmc.md))
use instead the nominal number of parameters, `(s-1)*(1+p)` where `p`
counts all the levels of the factor covariates, as in the original
algorithm.

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
ll <- loglikelihood(m_cov)
ll
#> 'log Lik.' -224.1199 (df= 5, nb obs.= 1005, initial="truncated")
attr(ll, "nobs")
#> [1] 1005

## Likelihood for new time series and covariates with previously
## fitted VLMC with covariates
pc_new <- powerconsumption[powerconsumption$week == 11, ]
rdts_new <- cut(pc_new$active_power, breaks = breaks, labels = labels)
rdts_cov_new <- data.frame(day_night = (pc_new$hour >= 7 & pc_new$hour <= 17))
ll_new <- loglikelihood(m_cov, newdata = rdts_new, newcov = rdts_cov_new)
ll_new
#> 'log Lik.' -248.89 (df= 5, nb obs.= 1005, initial="truncated")
attributes(ll_new)
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
#> [1] "logLikMixVLMC" "logLik"       
#> 
## extended likelihood on the same data
ll_new_extended <- loglikelihood(m_cov,
  newdata = rdts_new, newcov = rdts_cov_new,
  initial = "extended"
)
attributes(ll_new_extended)
#> $nobs
#> [1] 1008
#> 
#> $df
#> [1] 9
#> 
#> $initial
#> [1] "extended"
#> 
#> $class
#> [1] "logLikMixVLMC" "logLik"       
#> 
```
