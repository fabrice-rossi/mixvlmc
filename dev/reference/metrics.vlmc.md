# Predictive quality metrics for VLMC

This function computes and returns predictive quality metrics for
context based models such as VLMC and VLMC with covariates.

## Usage

``` r
# S3 method for class 'vlmc'
metrics(model, ...)

# S3 method for class 'metrics.vlmc'
print(x, ...)
```

## Arguments

- model:

  a fitted VLMC model.

- ...:

  additional arguments (not used).

- x:

  a `metrics.vlmc` object, result of a call to `metrics.vlmc()`.

## Value

An object of class `metrics.vlmc` with (at least) the following
components:

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

- `print(metrics.vlmc)`: Prints the predictive metrics of the VLMC
  model.

## Extended contexts

As explained in detail in the
[`loglikelihood.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/loglikelihood.md)
documentation and in the dedicated
[`vignette("likelihood", package = "mixvlmc")`](https://fabrice-rossi.github.io/mixvlmc/dev/articles/likelihood.md),
the first values of a time series do not in general have a proper
context for a VLMC of non zero depth. In order to predict something
meaningful for those values, we rely on the notion of extended context
defined in the documents mentioned above. This follows the same logic as
using
[`loglikelihood.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/loglikelihood.md)
with the parameter `initial="extended"`. All VLMC functions that need to
manipulate initial values with no proper context use the same approach.

## References

David J. Hand and Robert J. Till (2001). "A Simple Generalisation of the
Area Under the ROC Curve for Multiple Class Classification Problems."
*Machine Learning* 45(2), p. 171–186. DOI:
[doi:10.1023/A:1010920819831](https://doi.org/10.1023/A%3A1010920819831)
.

## See also

`metrics.vlmc()`,
[`metrics.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/metrics.covlmc.md),
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
model <- vlmc(rdts)
metrics(model)
#> VLMC context tree on 0, 1 [C++]
#>  cutoff: 1.921 (quantile: 0.05)
#>  Number of contexts: 69 
#>  Maximum context length: 63 
#>  Confusion matrix: 
#>     0   1   
#>   0 318 30  
#>   1 32  628 
#>  Accuracy: 0.9385 
#>  AUC: 0.9518 
```
