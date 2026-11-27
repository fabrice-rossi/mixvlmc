# Next state prediction in a discrete time series for a VLMC with covariates

This function computes one-step-ahead predictions for a discrete time
series based on a VLMC with covariates.

## Usage

``` r
# S3 method for class 'covlmc'
predict(
  object,
  newdata,
  newcov,
  type = c("raw", "probs"),
  final_pred = TRUE,
  ...
)
```

## Arguments

- object:

  a fitted VLMC with covariates (a `covlmc` object). Fully trimmed
  models are not supported (see
  [`trim.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/trim.covlmc.md)).

- newdata:

  a discrete time series with the same state space as the one used to
  fit `object`.

- newcov:

  a data frame with the new values for the covariates, with one row per
  observation in `newdata`.

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

  additional arguments.

## Value

A vector of predictions if `type="raw"` or a matrix of state
probabilities if `type="probs"`. The number of predictions (or of rows
of the matrix) is the length of `newdata`, plus one if
`final_pred=TRUE`.

## Details

Given a time series `X`, at time step `t`, a context is computed using
observations from `X[1]` to `X[t-1]` (see the dedicated section). The
prediction is then the most probable state for `X[t]` given the logistic
model of the context and the corresponding values of the covariates. The
time series of predictions is returned by the function when `type="raw"`
(default case).

When `type="probs"`, the function returns the probabilities of each
state for `X[t]` as estimated by the logistic models. Those
probabilities are returned as a matrix of probabilities with column
names given by the state names.

## Extended contexts

As explained in detail in the
[`loglikelihood.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/loglikelihood.covlmc.md)
documentation and in the dedicated
[`vignette("likelihood", package = "mixvlmc")`](https://fabrice-rossi.github.io/mixvlmc/dev/articles/likelihood.md),
the first initial values of a time series do not in general have a
proper context for a COVLMC with a non-zero order. In order to predict
something meaningful for those values, we rely on the notion of extended
context defined in the documents mentioned above. This follows the same
logic as using
[`loglikelihood.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/loglikelihood.covlmc.md)
with the parameter `initial="extended"`. All COVLMC functions that need
to manipulate initial values with no proper context use the same
approach.

## Examples

``` r
pc <- powerconsumption[powerconsumption$week == 10, ]
rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.2, 0.7, 1))))
rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
m_cov <- covlmc(rdts, rdts_cov, min_size = 5, alpha = 0.5)
rdts_probs <- predict(m_cov, rdts[1:144], rdts_cov[1:144, , drop = FALSE], type = "probs")
head(rdts_probs)
#>       (0,0.371] (0.371,1.46] (1.46,8.03]
#> [1,] 0.20039683    0.4990079  0.30059524
#> [2,] 0.09426230    0.8278689  0.07786885
#> [3,] 0.06943958    0.8656575  0.06490293
#> [4,] 0.06943958    0.8656575  0.06490293
#> [5,] 0.06943958    0.8656575  0.06490293
#> [6,] 0.06943958    0.8656575  0.06490293
rdts_preds <- predict(m_cov, rdts[1:144], rdts_cov[1:144, , drop = FALSE],
  type = "raw", final_pred = FALSE
)
## comparison between the predictions and the actual values
table(rdts_preds, rdts[1:144])
#>               
#> rdts_preds     (0,0.371] (0.371,1.46] (1.46,8.03]
#>   (0,0.371]            0            0           0
#>   (0.371,1.46]         0          126           3
#>   (1.46,8.03]          0            3          12
```
