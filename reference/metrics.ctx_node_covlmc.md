# Predictive quality metrics for a node of a COVLMC context tree

This function computes and returns predictive quality metrics for a node
(`ctx_node_covlmc`) extracted from a VLMC with covariates.

## Usage

``` r
# S3 method for class 'ctx_node_covlmc'
metrics(model, ...)
```

## Arguments

- model:

  A `ctx_node_covlmc` object as returned by
  [`find_sequence.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/find_sequence.covlmc.md)
  or
  [`contexts.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.covlmc.md)

- ...:

  additional parameters for predictive metrics computation.

## Value

an object of class `metrics.covlmc` with the following components:

- `accuracy`: the accuracy of the predictions

- `conf_mat`: the confusion matrix of the predictions, with predicted
  values in rows and true values in columns

- `auc`: the AUC of the predictive model

## Details

Compared to
[`metrics.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/metrics.covlmc.md),
this function focuses on a single context and assesses the quality of
its predictions, disregarding observations that have other contexts.
Apart from this limited scope, the function operates as
[`metrics.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/metrics.covlmc.md).
The node must be a context (see
[`is_context()`](https://fabrice-rossi.github.io/mixvlmc/reference/is_context.md)).

## References

David J. Hand and Robert J. Till (2001). "A Simple Generalisation of the
Area Under the ROC Curve for Multiple Class Classification Problems."
*Machine Learning* 45(2), p. 171–186. DOI:
[doi:10.1023/A:1010920819831](https://doi.org/10.1023/A%3A1010920819831)
.

## See also

[`metrics.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/metrics.vlmc.md),
[`metrics.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/metrics.covlmc.md),
[`metrics.ctx_node()`](https://fabrice-rossi.github.io/mixvlmc/reference/metrics.ctx_node.md),
[`contexts.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.vlmc.md),
[`predict.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/predict.vlmc.md).

## Examples

``` r
pc <- powerconsumption[powerconsumption$week == 5, ]
breaks <- c(
  0,
  median(powerconsumption$active_power, na.rm = TRUE),
  max(powerconsumption$active_power, na.rm = TRUE)
)
labels <- c(0, 1)
rdts <- cut(pc$active_power, breaks = breaks, labels = labels)
rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
m_cov <- covlmc(rdts, rdts_cov, min_size = 5)
m_ctxs <- contexts(m_cov)
## get the predictive metrics for each context
lapply(m_ctxs, metrics)
#> [[1]]
#> Context [T]: 0 
#>  followed by 0 (318), 1 (31)
#>  Confusion matrix: 
#>     0   1  
#>   0 318 31 
#>   1 0   0  
#>  Accuracy: 0.9112 
#>  AUC: 0.6111 
#> 
#> [[2]]
#> Context [T]: 0, 1 
#>  followed by 0 (4), 1 (27)
#>  Confusion matrix: 
#>     0 1  
#>   0 0 0  
#>   1 4 27 
#>  Accuracy: 0.871 
#>  AUC: 0.5 
#> 
#> [[3]]
#> Context [T]: 0, 1, 1 
#>  followed by 0 (5), 1 (22)
#>  Confusion matrix: 
#>     0 1  
#>   0 0 0  
#>   1 5 22 
#>  Accuracy: 0.8148 
#>  AUC: 0.5 
#> 
#> [[4]]
#> Context [T]: 1, 1, 1 
#>  followed by 0 (23), 1 (575)
#>  Confusion matrix: 
#>     0  1   
#>   0 0  0   
#>   1 23 575 
#>  Accuracy: 0.9615 
#>  AUC: 0.5 
#> 
```
