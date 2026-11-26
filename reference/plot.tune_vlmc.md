# Plot the results of automatic (CO)VLMC complexity selection

This function plots the results of
[`tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_vlmc.md)
or
[`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_covlmc.md).

## Usage

``` r
# S3 method for class 'tune_vlmc'
plot(
  x,
  value = c("criterion", "likelihood"),
  cutoff = c("quantile", "native"),
  ...
)

# S3 method for class 'tune_covlmc'
plot(
  x,
  value = c("criterion", "likelihood"),
  cutoff = c("quantile", "native"),
  ...
)
```

## Arguments

- x:

  a `tune_vlmc` or a `tune_covlmc` object.

- value:

  the value to plot: either `"criterion"` (default) for the information
  criterion used for model selection or `"likelihood"` for the log
  likelihood.

- cutoff:

  the scale used for the cut off criterion: either `"quantile"`
  (default) or `"native"` (only for `tune_vlmc` objects).

- ...:

  additional parameters passed to
  [`base::plot()`](https://rdrr.io/r/base/plot.html).

## Value

the `x` object, invisibly.

## Details

The standard plot shows the evolution of the criterion used to select
the model ([`AIC()`](https://rdrr.io/r/stats/AIC.html) or
[`BIC()`](https://rdrr.io/r/stats/AIC.html)) as a function of the cut
off criterion expressed in the quantile scale (the quantile scale is
used by default to offer a common default behaviour between
[`vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/vlmc.md)
and
[`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc.md)).
Parameters can be used to display instead the
[`loglikelihood()`](https://fabrice-rossi.github.io/mixvlmc/reference/loglikelihood.md)
of the model (by setting `value="likelihood"`) and to use the native
scale for the cut off when available (by setting `cutoff="native"`,
which is not supported for
[`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_covlmc.md)
results).

## Customisation

The function sets several defaults before calling
[`base::plot()`](https://rdrr.io/r/base/plot.html), namely:

- `type`: "l" by default to use a line representation;

- `xlab`: "Cut off (quantile scale)" by default, adapted to the actual
  scale;

- `ylab`: the name of the criterion or "Log likelihood".

These parameters can be overridden by specifying other values when
calling the function. All parameters specified in addition to `x`,
`value` and `cutoff` are passed to
[`base::plot()`](https://rdrr.io/r/base/plot.html).

## Examples

``` r
pc <- powerconsumption[powerconsumption$week == 5, ]
rdts <- cut(pc$active_power,
  breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1)))
)
tune_result <- tune_vlmc(rdts)
## default plot
plot(tune_result)

## native scale for the cut off
plot(tune_result, cutoff = "native")

## likelihood
plot(tune_result, value = "likelihood")

## parameters overriding
plot(tune_result,
  value = "likelihood",
  xlab = "Cut off", type = "b"
)

pc <- powerconsumption[powerconsumption$week %in% 10:12, ]
rdts <- cut(pc$active_power,
  breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1)))
)
rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
rdts_best_model_tune <- tune_covlmc(rdts, rdts_cov, criterion = "AIC")
plot(rdts_best_model_tune)

plot(rdts_best_model_tune, value = "likelihood")

```
