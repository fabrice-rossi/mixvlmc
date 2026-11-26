# Create a complete ggplot for the results of automatic VLMC complexity selection

This function prepares a plot of the results of
[`tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_vlmc.md)
using ggplot2. The result can be passed to
[`print()`](https://rdrr.io/r/base/print.html) to display the plot.

## Usage

``` r
# S3 method for class 'tune_vlmc'
autoplot(object, cutoff = c("quantile", "native"), ...)
```

## Arguments

- object:

  a `tune_vlmc` object

- cutoff:

  the scale used for the cut off criterion on the x axis, either
  `"quantile"` (default) or `"native"`

- ...:

  additional parameters (not used currently)

## Value

a ggplot object

## Details

The graphical representation proposed by this function is complete,
while the one produced by
[`plot.tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/plot.tune_vlmc.md)
is minimalist. We use here the faceting capabilities of ggplot2 to
combine on a single graphical representation the evolution of multiple
characteristics of the VLMC during the pruning process (depth, number of
contexts, log-likelihood and information criteria), while
[`plot.tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/plot.tune_vlmc.md)
shows only the selection criterion or the log-likelihood. Each facet of
the resulting plot shows a quantity as a function of the cut off
expressed in quantile or native scale.

## Examples

``` r
pc <- powerconsumption[powerconsumption$week %in% 10:11, ]
rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
rdts_best_model_tune <- tune_vlmc(rdts, criterion = "BIC")
vlmc_plot <- ggplot2::autoplot(rdts_best_model_tune)
print(vlmc_plot)

## simple post customisation
print(vlmc_plot + ggplot2::geom_point())

## cut off in native scale
print(ggplot2::autoplot(rdts_best_model_tune, cutoff = "native"))
```
