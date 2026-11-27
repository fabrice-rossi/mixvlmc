# Simulate a discrete time series for a VLMC with covariates

This function simulates a time series from the distribution estimated by
the given VLMC with covariates (a `covlmc` object).

## Usage

``` r
# S3 method for class 'covlmc'
simulate(object, nsim = 1, seed = NULL, covariate, init = NULL, ...)
```

## Arguments

- object:

  a fitted VLMC with covariates (a `covlmc` object). Fully trimmed
  models are not supported (see
  [`trim.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/trim.covlmc.md)).

- nsim:

  length of the simulated time series (defaults to 1).

- seed:

  an optional random seed (see the dedicated section).

- covariate:

  a data frame with the values of the covariates, with at least `nsim`
  rows.

- init:

  an optional initial sequence for the time series given by an object
  that can be interpreted as a discrete time series. Its length must not
  exceed `nsim`.

- ...:

  additional arguments.

## Value

a simulated discrete time series of the same type as the one used to
build the model, with a `seed` attribute (see the Random seed section).
The result also has the `dts_simulated` class to hide the `seed`
attribute when using `print` or similar functions.

## Details

A VLMC with covariates needs covariates to compute its transition
probabilities. The covariates must be submitted as a data frame using
the `covariate` argument. In addition, the time series can be initiated
by a fixed sequence specified via the `init` parameter.

## Extended contexts

As explained in detail in the
[`loglikelihood.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/loglikelihood.covlmc.md)
documentation and in the dedicated
[`vignette("likelihood", package = "mixvlmc")`](https://fabrice-rossi.github.io/mixvlmc/dev/articles/likelihood.md),
the first initial values of a time series do not in general have a
proper context for a COVLMC with a non-zero order. In order to simulate
something meaningful for those values, we rely on the notion of extended
context defined in the documents mentioned above. This follows the same
logic as using
[`loglikelihood.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/loglikelihood.covlmc.md)
with the parameter `initial="extended"`. All COVLMC functions that need
to manipulate initial values with no proper context use the same
approach.

## Random seed

This function reproduces the behaviour of
[`stats::simulate()`](https://rdrr.io/r/stats/simulate.html). If `seed`
is `NULL`, the simulation uses the current state of the random number
generator (which is therefore modified) and the value of
[.Random.seed](https://rdrr.io/r/base/Random.html) before the simulation
is returned as a `seed` attribute of the result. This can be used to
reproduce exactly the simulation results by setting
[.Random.seed](https://rdrr.io/r/base/Random.html) to this value.
Otherwise, `seed` is used to initialise the random number generator for
the simulation, and the state of the generator is restored afterwards.
Notice that if the random seed has not been initialised by R so far, the
function issues a call to `runif(1)` to perform this initialisation (as
is done in
[`stats::simulate()`](https://rdrr.io/r/stats/simulate.html)).

If `seed` is an integer, it is used in a call to
[`set.seed()`](https://rdrr.io/r/base/Random.html) before the simulation
takes place. The integer is saved as a `seed` attribute in the return
value. The integer seed is completed by an attribute `kind` which
contains the value `as.list([RNGkind()])` exactly as with
[`stats::simulate()`](https://rdrr.io/r/stats/simulate.html). The random
generator state is reset to its original value at the end of the call.

## See also

[`stats::simulate()`](https://rdrr.io/r/stats/simulate.html) for details
and examples on the random number generator setting.

## Examples

``` r
pc <- powerconsumption[powerconsumption$week == 5, ]
rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
m_cov <- covlmc(rdts, rdts_cov, min_size = 5)
# new week with daylight from 6:00 to 18:00 (144 observations per day, one
# every 10 minutes)
new_cov <- data.frame(
  day_night = rep(c(rep(FALSE, 36), rep(TRUE, 72), rep(FALSE, 36)), times = 7)
)
new_rdts <- simulate(m_cov, nrow(new_cov), seed = 0, covariate = new_cov)
table(new_rdts)
#> new_rdts
#>    (0,1.34] (1.34,7.54] 
#>         577         431 
new_rdts_2 <- simulate(m_cov, nrow(new_cov), seed = 0, covariate = new_cov, init = rdts[1:10])
## the simulated time series starts with the initial values
identical(as.character(new_rdts_2[1:10]), as.character(rdts[1:10]))
#> [1] TRUE
```
