# Next state prediction in a discrete time series for a VLMC

This function computes one step ahead predictions for a discrete time
series based on a VLMC.

## Usage

``` r
# S3 method for class 'vlmc'
predict(object, newdata, type = c("raw", "probs"), final_pred = TRUE, ...)

# S3 method for class 'vlmc_cpp'
predict(object, newdata, type = c("raw", "probs"), final_pred = TRUE, ...)
```

## Arguments

- object:

  a fitted VLMC.

- newdata:

  an object that can be interpreted as a discrete time series (for
  instance a `dts` object), with values in the state space of the model.

- type:

  character indicating the type of prediction required. The default
  `"raw"` returns actual predictions in the form of a new time series.
  The alternative `"probs"` returns a matrix of prediction probabilities
  (see details).

- final_pred:

  if `TRUE` (default value), the predictions include a final prediction
  step, made by computing the context of the full time series. When
  `FALSE` this final prediction is not included.

- ...:

  additional arguments (not used).

## Value

A vector of predictions if `type="raw"` or a matrix of state
probabilities if `type="probs"`. The number of predictions (or of rows
of the matrix) is the length of `newdata`, plus one if `final_pred` is
`TRUE`.

## Details

Given a time series `X`, at time step `t`, a context is computed using
observations from `X[1]` to `X[t-1]` (see the dedicated section). The
prediction is then the most probable state for `X[t]` given this
context. Ties are broken according to the natural order in the state
space, favouring "small" values. The time series of predictions is
returned by the function when `type="raw"` (default case).

When `type="probs"`, each `X[t]` is associated with the conditional
probabilities of its possible values given its context. Those
probabilities are returned as a matrix of probabilities with column
names given by the state names.

## Extended contexts

As explained in detail in the
[`loglikelihood.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/loglikelihood.md)
documentation and in the dedicated
[`vignette("likelihood", package = "mixvlmc")`](https://fabrice-rossi.github.io/mixvlmc/articles/likelihood.md),
the first values of a time series do not in general have a proper
context for a VLMC of non zero depth. In order to predict something
meaningful for those values, we rely on the notion of extended context
defined in the documents mentioned above. This follows the same logic as
using
[`loglikelihood.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/loglikelihood.md)
with the parameter `initial="extended"`. All VLMC functions that need to
manipulate initial values with no proper context use the same approach.

## Examples

``` r
pc <- powerconsumption[powerconsumption$week == 5, ]
rdts <- cut(pc$active_power,
  breaks = c(0, quantile(pc$active_power,
    probs = c(0.25, 0.5, 0.75, 1)
  ))
)
model <- vlmc(rdts, min_size = 5)
predict(model, rdts[1:5])
#> [1] (0,0.458]    (0.458,1.34] (0.458,1.34] (0.458,1.34] (0.458,1.34]
#> [6] (0.458,1.34]
#> Levels: (0,0.458] (0.458,1.34] (1.34,2.13] (2.13,7.54]
predict(model, rdts[1:5], "probs")
#>      (0,0.458] (0.458,1.34] (1.34,2.13] (2.13,7.54]
#> [1,] 0.2500000    0.2500000   0.2500000  0.25000000
#> [2,] 0.1984127    0.6666667   0.1071429  0.02777778
#> [3,] 0.1369048    0.7500000   0.0952381  0.01785714
#> [4,] 0.1369048    0.7500000   0.0952381  0.01785714
#> [5,] 0.1369048    0.7500000   0.0952381  0.01785714
#> [6,] 0.1369048    0.7500000   0.0952381  0.01785714
## without the final prediction
predict(model, rdts[1:5], final_pred = FALSE)
#> [1] (0,0.458]    (0.458,1.34] (0.458,1.34] (0.458,1.34] (0.458,1.34]
#> Levels: (0,0.458] (0.458,1.34] (1.34,2.13] (2.13,7.54]
## C++ backend
pc <- powerconsumption[powerconsumption$week == 5, ]
rdts <- cut(pc$active_power,
  breaks = c(0, quantile(pc$active_power,
    probs = c(0.25, 0.5, 0.75, 1)
  ))
)
model <- vlmc(rdts, min_size = 5, backend = "C++")
predict(model, rdts[1:5])
#> [1] (0,0.458]    (0.458,1.34] (0.458,1.34] (0.458,1.34] (0.458,1.34]
#> [6] (0.458,1.34]
#> Levels: (0,0.458] (0.458,1.34] (1.34,2.13] (2.13,7.54]
predict(model, rdts[1:5], "probs")
#>      (0,0.458] (0.458,1.34] (1.34,2.13] (2.13,7.54]
#> [1,] 0.2500000    0.2500000   0.2500000  0.25000000
#> [2,] 0.1984127    0.6666667   0.1071429  0.02777778
#> [3,] 0.1369048    0.7500000   0.0952381  0.01785714
#> [4,] 0.1369048    0.7500000   0.0952381  0.01785714
#> [5,] 0.1369048    0.7500000   0.0952381  0.01785714
#> [6,] 0.1369048    0.7500000   0.0952381  0.01785714
```
