# Predictive quality metrics for a node of a context tree

This function computes and returns predictive quality metrics for a node
(`ctx_node`) extracted from a context tree.

## Usage

``` r
# S3 method for class 'ctx_node'
metrics(model, ...)
```

## Arguments

- model:

  a `ctx_node` object as returned by
  [`find_sequence()`](https://fabrice-rossi.github.io/mixvlmc/reference/find_sequence.md).

- ...:

  additional parameters for predictive metrics computation.

## Value

The returned value is guaranteed to have at least three components:

- `accuracy`: the accuracy of the predictions

- `conf_mat`: the confusion matrix of the predictions, with predicted
  values in rows and true values in columns

- `auc`: the AUC of the predictive model

## Details

Compared to
[`metrics.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/metrics.vlmc.md),
this function focuses on a single context and assesses the quality of
its predictions, disregarding observations that have other contexts.
Apart from this limited scope, the function operates as
[`metrics.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/metrics.vlmc.md).
The node must be a context (see
[`is_context()`](https://fabrice-rossi.github.io/mixvlmc/reference/is_context.md)).

As the transition probabilities are the same for all the occurrences of
a context, the predictions of a context are constant and the AUC is not
informative: it is always `NA`.

## References

David J. Hand and Robert J. Till (2001). "A Simple Generalisation of the
Area Under the ROC Curve for Multiple Class Classification Problems."
*Machine Learning* 45(2), p. 171–186. DOI:
[doi:10.1023/A:1010920819831](https://doi.org/10.1023/A%3A1010920819831)
.

## See also

[`metrics.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/metrics.vlmc.md),
[`metrics.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/metrics.covlmc.md),
`metrics.ctx_node()`,
[`contexts.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.vlmc.md),
[`predict.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/predict.vlmc.md).

## Examples

``` r
pc <- powerconsumption[powerconsumption$week == 5, ]
rdts <- cut(pc$active_power,
  breaks = c(0, quantile(pc$active_power,
    probs = c(0.25, 0.5, 0.75, 1)
  ))
)
model <- vlmc(rdts)
model_ctxs <- contexts(model)
metrics(model_ctxs[[4]])
#> Context [T]: (1.34,2.13] 
#>  followed by (0,0.458] (1), (0.458,1.34] (31), (1.34,2.13] (183), (2.13,7.54] (37)
#>  Confusion matrix: 
#>                (0,0.458] (0.458,1.34] (1.34,2.13] (2.13,7.54] 
#>   (0,0.458]    0         0            0           0           
#>   (0.458,1.34] 0         0            0           0           
#>   (1.34,2.13]  1         23           162         26          
#>   (2.13,7.54]  0         0            0           0           
#>  Accuracy: 0.7642 
#>  AUC: NA 
```
