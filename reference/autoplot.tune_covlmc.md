# Create a complete ggplot for the results of automatic COVLMC complexity selection

This function prepares a plot of the results of
[`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_covlmc.md)
using ggplot2. The result can be passed to
[`print()`](https://rdrr.io/r/base/print.html) to display the plot.

## Usage

``` r
# S3 method for class 'tune_covlmc'
autoplot(object, ...)
```

## Arguments

- object:

  a `tune_covlmc` object

- ...:

  additional parameters (not used currently)

## Value

a ggplot object

## Details

The graphical representation proposed by this function is complete,
while the one produced by
[`plot.tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/plot.tune_vlmc.md)
is minimalist. We use here the faceting capabilities of ggplot2 to
combine on a single graphical representation the evolution of multiple
characteristics of the COVLMC during the pruning process (depth, number
of contexts, log-likelihood, covariate depth and information criteria),
while
[`plot.tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/plot.tune_vlmc.md)
shows only the selection criterion or the log-likelihood. Each facet of
the resulting plot shows a quantity as a function of the cut off
expressed in quantile scale (the only scale available for COVLMC).

## Examples

``` r
pc <- powerconsumption[powerconsumption$week %in% 10:12, ]
rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
rdts_best_model_tune <- tune_covlmc(rdts, rdts_cov, criterion = "AIC")
covlmc_plot <- ggplot2::autoplot(rdts_best_model_tune)
print(covlmc_plot)
```
