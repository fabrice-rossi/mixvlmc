# Summary of the model selection of a VLMC

This function summarises the results of
[`tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/tune_vlmc.md).
The associated [`print()`](https://rdrr.io/r/base/print.html) method
displays the selected model and the characteristics of all the models
considered during the selection.

## Usage

``` r
# S3 method for class 'tune_vlmc'
summary(object, ...)

# S3 method for class 'summary.tune_vlmc'
print(x, ...)
```

## Arguments

- object:

  the results of
  [`tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/tune_vlmc.md)
  (a `tune_vlmc` object).

- ...:

  additional arguments (not used).

- x:

  a summary of the results of
  [`tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/tune_vlmc.md),
  as returned by [`summary()`](https://rdrr.io/r/base/summary.html).

## Value

[`summary()`](https://rdrr.io/r/base/summary.html) returns an object of
class `summary.tune_vlmc`: the results of
[`tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/tune_vlmc.md)
without the saved models (see the `save` parameter of
[`tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/tune_vlmc.md)).
In particular, its `results` component is the data frame that describes
the models considered during the selection.

[`print()`](https://rdrr.io/r/base/print.html) displays the summary and
returns `x` invisibly.

## See also

[`tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/tune_vlmc.md)

## Examples

``` r
rdts <- sample(as.factor(c("A", "B", "C")), 100, replace = TRUE)
tune_result <- tune_vlmc(rdts)
summary(tune_result)
#> VLMC tune results
#> 
#> Best VLMC selected by BIC (214.1828) with likelihood function "truncated" (-102.5375)
#> VLMC context tree on A, B, C [C++]
#>  cutoff: 2.367 (quantile: 0.09373)
#>  Number of contexts: 1 
#>  Maximum context length: 0 
#> 
#> Pruning results
#>    cutoff      alpha depth nb_contexts loglikelihood      AIC      BIC
#>  1.151293 0.31622777     5          22     -80.85383 249.7077 362.0783
#>  1.285201 0.27659500     5          21     -82.09727 248.1945 355.4574
#>  1.357022 0.25742636     5          21     -82.95564 249.9113 357.1741
#>  1.406763 0.24493497     4          13     -90.24834 232.4967 298.8975
#>  1.435386 0.23802358     4          12     -91.67587 231.3517 292.6448
#>  1.464964 0.23108637     4          10     -93.39686 226.7937 277.8713
#>  1.807543 0.16405675     4           7     -96.09010 220.1802 255.9345
#>  2.252993 0.10508426     2           3     -99.74347 211.4869 226.8102
#>  2.367360 0.09372783     0           1    -102.53755 209.0751 214.1828
```
