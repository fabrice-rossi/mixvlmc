# Simulate a discrete time series for a VLMC

This function simulates a time series from the distribution estimated by
the given VLMC.

## Usage

``` r
# S3 method for class 'vlmc'
simulate(object, nsim = 1L, seed = NULL, init = NULL, burnin = 0L, ...)
```

## Arguments

- object:

  a fitted VLMC.

- nsim:

  length of the simulated time series (defaults to 1).

- seed:

  an optional random seed (see the dedicated section).

- init:

  an optional initial sequence for the time series given by an object
  that can be interpreted as a discrete time series, with values in the
  state space of the model.

- burnin:

  number of initial observations to discard or `"auto"` (see the
  dedicated section). Defaults to 0.

- ...:

  additional arguments (not used).

## Value

a simulated discrete time series of the same type as the one used to
build the VLMC with a `seed` attribute (see the Random seed section).
The result has also the `dts_simulated` class to hide the `seed`
attribute when using [`print()`](https://rdrr.io/r/base/print.html) or
similar functions.

## Details

The time series can be initiated by a fixed sequence specified via the
`init` parameter: the simulated time series then starts with this
sequence (whose length counts in the `nsim + burnin` values, see below).

## Burn in (Warm up) period

When using a VLMC for simulation purposes, we are generally interested
in the stationary distribution of the corresponding Markov chain. To
reduce the dependence of the samples on the initial values and get
closer to this stationary distribution (if it exists), it is recommended
to discard the first samples which are produced in a so-called "burn in"
(or "warm up") period. The `burnin` parameter can be used to implement
this approach. The VLMC is used to produce a sample of size
`burnin + nsim` but the first `burnin` values are discarded. Notice that
these burn in values can be partially given by the `init` parameter if
it is specified.

If `burnin` is set to `"auto"`, the `burnin` period is set to
`64 * context_number(object)`, following the heuristic proposed in
Mächler and Bühlmann (2004).

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

## Extended contexts

As explained in detail in the
[`loglikelihood.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/loglikelihood.md)
documentation and in the dedicated
[`vignette("likelihood", package = "mixvlmc")`](https://fabrice-rossi.github.io/mixvlmc/dev/articles/likelihood.md),
the first values of a time series do not in general have a proper
context for a VLMC of non zero depth. In order to simulate something
meaningful for those values when `init` is not provided, we rely on the
notion of extended context defined in the documents mentioned above.
This follows the same logic as using
[`loglikelihood.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/loglikelihood.md)
with the parameter `initial="extended"`. All VLMC functions that need to
manipulate initial values with no proper context use the same approach.

## References

Mächler, M. and Bühlmann, P. (2004) "Variable Length Markov Chains:
Methodology, Computing, and Software" Journal of Computational and
Graphical Statistics, 13 (2), 435-455,
[doi:10.1198/1061860043524](https://doi.org/10.1198/1061860043524)

## See also

[`stats::simulate()`](https://rdrr.io/r/stats/simulate.html) for details
and examples on the random number generator setting

## Examples

``` r
pc <- powerconsumption[powerconsumption$week == 5, ]
rdts <- cut(pc$active_power,
  breaks = c(0, quantile(pc$active_power,
    probs = c(0.25, 0.5, 0.75, 1)
  ))
)
model <- vlmc(rdts, min_size = 5)
new_rdts <- simulate(model, 500, seed = 0)
head(new_rdts, 10)
#>  [1] (2.13,7.54] (2.13,7.54] (2.13,7.54] (2.13,7.54] (2.13,7.54] (2.13,7.54]
#>  [7] (2.13,7.54] (2.13,7.54] (2.13,7.54] (2.13,7.54]
#> Levels: (0,0.458] (0.458,1.34] (1.34,2.13] (2.13,7.54]
## the simulation starts with the init sequence
new_rdts_2 <- simulate(model, 500, seed = 0, init = rdts[1:5])
head(new_rdts_2, 10)
#>  [1] (0.458,1.34] (0.458,1.34] (0.458,1.34] (0.458,1.34] (0.458,1.34]
#>  [6] (1.34,2.13]  (1.34,2.13]  (1.34,2.13]  (1.34,2.13]  (2.13,7.54] 
#> Levels: (0,0.458] (0.458,1.34] (1.34,2.13] (2.13,7.54]
new_rdts_3 <- simulate(model, 500, seed = 0, burnin = 500)
## automatic burn in period
new_rdts_4 <- simulate(model, 500, seed = 0, burnin = "auto")
```
