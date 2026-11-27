# Predictive quality metrics for VLMC with covariates

This function computes and returns predictive quality metrics for
context based models such as VLMC and VLMC with covariates.

## Usage

``` r
# S3 method for class 'covlmc'
metrics(model, ...)

# S3 method for class 'metrics.covlmc'
print(x, ...)
```

## Arguments

- model:

  a fitted VLMC with covariates (a `covlmc` object).

- ...:

  additional parameters for predictive metrics computation.

- x:

  a `metrics.covlmc` object, result of a call to `metrics.covlmc()`.

## Value

An object of class `metrics.covlmc` with the following components:

- `accuracy`: the accuracy of the predictions

- `conf_mat`: the confusion matrix of the predictions, with predicted
  values in rows and true values in columns

- `auc`: the AUC of the predictive model

The object has a print method that recalls basic information about the
model together with the values of the components above.

## Details

A context based model computes transition probabilities for its
contexts. Using a maximum transition probability decision rule, this can
be used to predict the state that is the most likely to follow the
current one, given the context (see
[`predict.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/predict.vlmc.md)).
The quality of these predictions on the time series used to fit the
model is evaluated using standard metrics including:

- accuracy

- the full confusion matrix

- the area under the ROC curve (AUC), considering the context based
  model as a (conditional) probability estimator. We use Hand and
  Till (2001) multiclass AUC in case of a state space with more than 2
  states

## Methods (by generic)

- `print(metrics.covlmc)`: Prints the predictive metrics of the VLMC
  with covariates.

## Trimmed models

The predictions of a trimmed model (see
[`trim.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/trim.covlmc.md))
on the time series used to fit it are not available any more. The
accuracy and the confusion matrix are obtained from the confusion
matrices of the contexts computed when the model was fitted (and from
the one of the first observations computed when the model was trimmed),
but the AUC cannot be computed: it is `NA`, with a warning.

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

## References

David J. Hand and Robert J. Till (2001). "A Simple Generalisation of the
Area Under the ROC Curve for Multiple Class Classification Problems."
*Machine Learning* 45(2), p. 171–186. DOI:
[doi:10.1023/A:1010920819831](https://doi.org/10.1023/A%3A1010920819831)
.

## See also

[`metrics.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/metrics.vlmc.md),
`metrics.covlmc()`,
[`metrics.ctx_node()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/metrics.ctx_node.md),
[`contexts.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.vlmc.md),
[`predict.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/predict.vlmc.md).

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
metrics(m_cov)
#> VLMC with covariates context tree on 0, 1 
#>  cutoff in quantile scale: 0.05
#>  Number of contexts: 4 
#>  Maximum context length: 3 
#>  Confusion matrix: 
#>     0   1   
#>   0 318 31  
#>   1 32  627 
#>  Accuracy: 0.9375 
#>  AUC: 0.9442 
```
