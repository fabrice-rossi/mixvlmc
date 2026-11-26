# Trim a COVLMC

This function returns a trimmed COVLMC from which cached data have been
removed.

## Usage

``` r
# S3 method for class 'covlmc'
trim(ct, keep_model = FALSE, ...)
```

## Arguments

- ct:

  a fitted VLMC with covariates (a `covlmc` object).

- keep_model:

  specifies whether to keep the internal models (or not). Defaults to
  `FALSE`.

- ...:

  additional arguments for the trim function.

## Value

a trimmed `covlmc` object.

## Details

Called with `keep_model` set to `FALSE` (default case), the trimming is
maximal and reduces further usability of the model. In particular
[`loglikelihood.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/loglikelihood.covlmc.md)
cannot be used for new data,
[`contexts.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.covlmc.md)
and
[`model()`](https://fabrice-rossi.github.io/mixvlmc/reference/model.md)
do not support full model extraction,
[`predict.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/predict.covlmc.md),
[`simulate.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/simulate.covlmc.md)
and
[`prune.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/prune.covlmc.md)
cannot be used at all, and
[`metrics.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/metrics.covlmc.md)
does not compute the AUC.

Called with `keep_model` set to `TRUE`, the trimming process is less
complete. In particular internal models are simplified using
[`butcher::butcher()`](https://butcher.tidymodels.org/reference/butcher.html)
and some additional minor reductions. This saves less memory but enables
the use of
[`loglikelihood.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/loglikelihood.covlmc.md)
for new data, of
[`predict.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/predict.covlmc.md)
and of
[`simulate.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/simulate.covlmc.md),
as well as full model extraction.
[`prune.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/prune.covlmc.md)
cannot be used on the trimmed model and
[`metrics.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/metrics.covlmc.md)
does not compute the AUC.

## See also

[`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_covlmc.md)

## Examples

``` r
pc <- powerconsumption[powerconsumption$week %in% 5:7, ]
rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
m_cov <- covlmc(rdts, rdts_cov, min_size = 10, keep_data = TRUE)
print(object.size(m_cov), units = "Mb")
#> 12 Mb
t_m_cov_model <- trim(m_cov, keep_model = TRUE)
print(object.size(t_m_cov_model), units = "Mb")
#> 8.4 Mb
t_m_cov <- trim(m_cov)
print(object.size(t_m_cov), units = "Mb")
#> 0.1 Mb
## the fully trimmed model can still be used e.g. to compute the likelihood
## on the original data
logLik(t_m_cov)
#> 'log Lik.' -778.9822 (df=16)
```
