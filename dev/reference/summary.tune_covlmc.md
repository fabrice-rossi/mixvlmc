# Summary of the model selection of a VLMC with covariates

This function summarises the results of
[`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/tune_covlmc.md).
The associated [`print()`](https://rdrr.io/r/base/print.html) method
displays the selected model and the characteristics of all the models
considered during the selection.

## Usage

``` r
# S3 method for class 'tune_covlmc'
summary(object, ...)

# S3 method for class 'summary.tune_covlmc'
print(x, ...)
```

## Arguments

- object:

  the results of
  [`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/tune_covlmc.md)
  (a `tune_covlmc` object).

- ...:

  additional arguments (not used).

- x:

  a summary of the results of
  [`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/tune_covlmc.md),
  as returned by [`summary()`](https://rdrr.io/r/base/summary.html).

## Value

[`summary()`](https://rdrr.io/r/base/summary.html) returns an object of
class `summary.tune_covlmc`: the results of
[`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/tune_covlmc.md)
without the saved models (see the `save` parameter of
[`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/tune_covlmc.md)).
In particular, its `results` component is the data frame that describes
the models considered during the selection.

[`print()`](https://rdrr.io/r/base/print.html) displays the summary and
returns `x` invisibly.

## See also

[`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/tune_covlmc.md)

## Examples

``` r
pc <- powerconsumption[powerconsumption$week == 10, ]
rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
rdts_tune <- tune_covlmc(rdts, rdts_cov, min_size = 5)
summary(rdts_tune)
#> VLMC with covariates tune results
#> 
#> Best VLMC with covariates selected by BIC (520.4432) with likelihood function "truncated" (-249.854)
#> VLMC with covariates context tree on (0,0.842], (0.842,8.03] 
#>  cutoff in quantile scale: 0.03526
#>  Number of contexts: 2 
#>  Maximum context length: 1 
#> 
#> Pruning results
#>          alpha depth nb_contexts loglikelihood cov_depth       AIC       BIC
#>   5.000000e-01     4           7     -238.1423         4  508.2846  586.8726
#>   4.146005e-01     4           7     -238.4751         4  506.9502  580.6264
#>   3.645332e-01     3           5     -240.3916         2  498.7831  542.9889
#>   1.727500e-01     2           4     -242.7754         2  501.5509  540.8448
#>   1.157590e-01     2           4     -244.0123         2  502.0246  536.4069
#>   3.875799e-02     2           3     -246.2856         2  504.5712  534.0417
#>   3.526427e-02     1           2     -249.8540         1  505.7080  520.4432
#>   3.902563e-03     1           2     -253.9287         0  511.8574  521.6809
#>  4.612249e-195     0           1     -695.9198         0 1393.8395 1398.7513
```
