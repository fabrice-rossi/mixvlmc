# Predicting with (CO)VLMC

``` r

library(mixvlmc)
library(ggplot2)
```

## Introduction

Once a (CO)VLMC has been estimated from a sequence, it can be used to
make one step or multiple steps ahead predictions on sequences using the
same state space. The predictions can be expressed as probabilities on
the state space for the future value of the time series or directly as
the most likely value considering the past of the time series.

### VLMC case

Mixvlmc implements one-step-ahead prediction. This is done in a
straightforward way, apart from the initial values. Indeed, given a time
series \\(x_i)\_{1\leq i\leq n}\\ and a VLMC of order \\d\\, the context
of \\x_j\\ for \\j\>d\\ can be determined from the \\d\\ past values \\
x\_{j-d}, x\_{j-d+1}, \ldots, x\_{j-1}. \\ In the VLMC case, this
context is associated with a conditional probability distribution for
\\X_j\\. This distribution can be reported directly as the “prediction”
for \\x_j\\, or a direct prediction can be made using \\ \arg\max\_{x\in
S}\mathbb{P}(X_j=x\|X\_{j-1}=x\_{j-1},\ldots,X\_{j-d}=x\_{j-d}), \\
where \\S\\ is the state space of the VLMC. Notice that this can be used
to predict \\x\_{n+1}\\ which is unknown, making actual predictions.

### COVLMC case

The case of COVLMC is almost identical. The only difference comes from
the influence of the covariates. One-step-ahead predictions can be made
only if the time series of covariates is provided.

Once the context of \\x_j\\ has been computed as in the case of a VLMC,
we obtain from the COVLMC the conditional distribution \\
\mathbb{P}(X_j\|X\_{j-1}=x\_{j-1},\ldots,X\_{j-d}=x\_{j-d},
Y\_{j-1}=y\_{j-1}, \ldots, Y\_{j-h}=y\_{j-h}), \\ where \\h\\ is the
order of covariate dependency for this context.

The distribution can be reported or a prediction can be made using the
mode of the distribution, exactly as in the VLMC case.

### Initial values

The only difficulty comes in both cases from the initial values \\x_1\\
to \\x_d\\ for which no proper context can be determined. This issue
appears in numerous situations when using (CO)VLMC models, in particular
for likelihood calculation (see
[`vignette("likelihood")`](https://fabrice-rossi.github.io/mixvlmc/dev/articles/likelihood.md))
and for sampling (see
[`vignette("sampling")`](https://fabrice-rossi.github.io/mixvlmc/dev/articles/sampling.md)).

We use the notion of *extended context* described in detail in the
likelihood vignette. The key idea is to include in the (CO)VLMC
additional contexts (and thus conditional distributions) to model the
first few observations.

## In practice

### VLMC

Let us consider for example the sun spot time series, `sunspot.year`,
seen as a binary time series, with high activity associated with a
number of sun spots larger than the median number.

``` r

sun_activity <- as.factor(ifelse(sunspot.year >= median(sunspot.year), "high", "low"))
```

We fit automatically an optimal VLMC as follows:

``` r

sun_model_tune <- tune_vlmc(sun_activity)
sun_model_tune
#> VLMC context tree on high, low [C++]
#>  cutoff: 2.306 (quantile: 0.03175)
#>  Number of contexts: 9 
#>  Maximum context length: 5 
#>  Selected by BIC (236.262) with likelihood function "truncated" (-92.85552)
```

Predictions are obtained as follows:

``` r

sun_model <- as_vlmc(sun_model_tune)
sun_model_predictions <- predict(sun_model, sun_activity)
```

Notice that unlike many implementations of
[`stats::predict()`](https://rdrr.io/r/stats/predict.html), we do not
support predictions without new data, but the original time series can
of course be used as the “new data”.

Predictions are relatively good, as shown by the confusion matrix:

``` r

table(sun_model_predictions[-length(sun_model_predictions)], sun_activity)
#>       sun_activity
#>        high low
#>   high  119  27
#>   low    27 116
```

Notice that we remove the last prediction to be able to perform the
comparison. We could have asked for it to be excluded directly by
passing `final_pred=FALSE` to the
[`predict.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/predict.vlmc.md)
function.

The BIC used by default in
[`tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/tune_vlmc.md)
may be too conservative for prediction or sampling oriented use of the
final model. The AIC can select a more complex model, but in this case
it selects the same model as the BIC:

``` r

sun_model_tune_aic <- tune_vlmc(sun_activity, criterion = "AIC")
sun_model_tune_aic
#> VLMC context tree on high, low [C++]
#>  cutoff: 2.306 (quantile: 0.03175)
#>  Number of contexts: 9 
#>  Maximum context length: 5 
#>  Selected by AIC (203.711) with likelihood function "truncated" (-92.85552)
```

It has therefore the same confusion matrix on its estimation data set:

``` r

table(
  predict(as_vlmc(sun_model_tune_aic), sun_activity, final_pred = FALSE),
  sun_activity
)
#>       sun_activity
#>        high low
#>   high  119  27
#>   low    27 116
```

Despite the use of penalised likelihood, we may experience some form of
overfitting and a split sample approach could be used to study it. For
instance, one can use AIC to build a VLMC on the first half of the sun
spots time series and evaluate the quality of the predictions on the
second half, as follows:

``` r

first_half <- 1:(length(sun_activity) %/% 2)
sun_model_tune_aic_half <- tune_vlmc(sun_activity[first_half], criterion = "AIC")
sun_model <- as_vlmc(sun_model_tune_aic_half)
table(
  predict(sun_model, sun_activity[-first_half], final_pred = FALSE),
  sun_activity[-first_half]
)
#>       
#>        high low
#>   high   67  13
#>   low    14  51
```

The predictions remain of a rather good quality.

### COVLMC

To illustrate COVLMC predictions, we use the French CAC index provided
in `EuStockMarkets`:

``` r

CAC_raw <- as.data.frame(EuStockMarkets)$CAC
```

We turn it into a discrete time series with three values:

- Stay if the value of the index on day t+1 is between 99.5% and 100.5%
  of the value on day t
- Up if the value increased by at least 0.5%
- Down if the value decreased by at least 0.5%

``` r

CAC_rel_evol <- diff(CAC_raw) / CAC_raw[-length(CAC_raw)]
CAC_dts <- factor(
  ifelse(CAC_rel_evol >= 0.005, "Up",
    ifelse(CAC_rel_evol <= -0.005, "Down", "Stay")
  ),
  levels = c("Down", "Stay", "Up")
)
```

We use the other indexes of `EuStockMarkets` as covariates. As
previously, we select a model with the AIC criterion:

``` r

CAC_covariates <- as.data.frame(EuStockMarkets)[c("DAX", "SMI", "FTSE")][-1, ]
CAC_covlmc <- tune_covlmc(CAC_dts, CAC_covariates, criterion = "AIC")
CAC_comodel <- as_covlmc(CAC_covlmc)
```

Finally, we obtain predictions using
[`predict.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/predict.covlmc.md):

``` r

CAC_pred <- predict(CAC_comodel, CAC_dts, CAC_covariates, final_pred = FALSE)
```

In this case, the predictions are of rather poor quality:

``` r

table(CAC_pred, CAC_dts)
#>         CAC_dts
#> CAC_pred Down Stay  Up
#>     Down   40   46  36
#>     Stay  453  625 467
#>     Up     36   65  91
```

This can be easily explained by the large ambiguity of the conditional
distributions as can be observed by focusing on them rather than on
predictions:

``` r

CAC_probs <- predict(CAC_comodel, CAC_dts, CAC_covariates, final_pred = FALSE, type = "probs")
CAC_probs[1:10, ]
#>            Down      Stay        Up
#>  [1,] 0.2845616 0.3959118 0.3195266
#>  [2,] 0.2878788 0.4073084 0.3048128
#>  [3,] 0.2878788 0.4073084 0.3048128
#>  [4,] 0.2878788 0.4073084 0.3048128
#>  [5,] 0.2878788 0.4073084 0.3048128
#>  [6,] 0.2878788 0.4073084 0.3048128
#>  [7,] 0.2878788 0.4073084 0.3048128
#>  [8,] 0.2878788 0.4073084 0.3048128
#>  [9,] 0.4248040 0.3124036 0.2627924
#> [10,] 0.3747649 0.4891093 0.1361259
```

The Shannon entropies of those conditional distributions are generally
relatively close to the maximum in this setting, i.e. 1.09861, as shown
on this graphical illustration

``` r

entropies <- data.frame(entropy = apply(CAC_probs, 1, \(x) -sum(x * log(x))))
ggplot(entropies, aes(x = entropy)) +
  geom_density() +
  geom_rug(alpha = 0.1) +
  geom_vline(xintercept = -log(1 / 3), col = 2)
```

![Density plot, with a rug, of the Shannon entropies of the conditional
distributions given by the COVLMC on the CAC series, with a red vertical
line at the maximal entropy log(3). Almost all entropies are
concentrated just below this
maximum.](prediction_files/figure-html/unnamed-chunk-15-1.png) This
illustrates the fact that (CO)VLMC models are not predictive models but
rather generative models. They may perform well on predictive tasks if
the conditional distributions are peaked enough, which corresponds in a
way to simple predictive cases.

For example, the case of the sun spots studied above for the VLMC has
better predictive performances because of the presence of strongly
peaked distributions (some of them are even deterministic), as shown
below:

``` r

sun_probs <- predict(as_vlmc(sun_model_tune_aic), sun_activity,
  final_pred = FALSE,
  type = "probs"
)
sun_entropies <- data.frame(entropy = apply(
  sun_probs, 1,
  \(x) -sum(x * log(x), na.rm = TRUE)
))
ggplot(sun_entropies, aes(x = entropy)) +
  geom_histogram(bins = 50) +
  geom_rug(alpha = 0.5) +
  geom_vline(xintercept = -log(1 / 2), col = 2)
```

![Histogram, with a rug, of the Shannon entropies of the conditional
distributions given by the VLMC on the sun spots series, with a red
vertical line at the maximal entropy log(2). Many entropies are zero or
small, revealing strongly peaked and sometimes deterministic
distributions, and all stay below the
maximum.](prediction_files/figure-html/unnamed-chunk-16-1.png)

Thus, while (CO)VLMC based predictions can be interesting, one will
generally obtain more insights about the original time series using
simulations, as detailed in
[`vignette("sampling")`](https://fabrice-rossi.github.io/mixvlmc/dev/articles/sampling.md).

## Metrics

In addition to
[`predict.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/predict.vlmc.md)
and
[`predict.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/predict.covlmc.md),
mixvlmc provides a
[`metrics()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/metrics.md)
function which computes and reports a collection of classification
oriented quality metrics computed on the time series (and covariates)
used to estimate the (CO)VLMC model. Those metrics could be computed
easily using the [`predict()`](https://rdrr.io/r/stats/predict.html)
functions, but
[`metrics()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/metrics.md)
is faster and does not need the original data.
[`metrics()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/metrics.md)
can also be applied to a single context (as returned by
[`contexts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.md)
or
[`find_sequence()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/find_sequence.md))
to assess the quality of the predictions made in this context only; it
reports an error on a node that is not a context.

### VLMC

For instance for the sun spots AIC model, we obtain

``` r

sun_metrics <- metrics(as_vlmc(sun_model_tune_aic))
sun_metrics
#> VLMC context tree on high, low [C++]
#>  cutoff: 2.306 (quantile: 0.03175)
#>  Number of contexts: 9 
#>  Maximum context length: 5 
#>  Confusion matrix: 
#>        high low 
#>   high 119  27  
#>   low  27   116 
#>  Accuracy: 0.8131 
#>  AUC: 0.9146
```

The ROC curve is computed by
[`metrics()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/metrics.md)
(for state spaces of size 2) and can be plotted simply using:

``` r

plot(sun_metrics$roc)
```

![ROC curve of the VLMC predictions on the sun spots series, plotting
sensitivity against specificity. The curve rises well above the
diagonal, showing a good discrimination between the two
states.](prediction_files/figure-html/unnamed-chunk-18-1.png)

### COVLMC

Similarly, the CAC model gives:

``` r

CAC_metrics <- metrics(CAC_comodel)
CAC_metrics
#> VLMC with covariates context tree on Down, Stay, Up 
#>  cutoff in quantile scale: 0.01046
#>  Number of contexts: 5 
#>  Maximum context length: 2 
#>  Confusion matrix: 
#>        Down Stay Up  
#>   Down 40   46   36  
#>   Stay 453  625  467 
#>   Up   36   65   91  
#>  Accuracy: 0.4067 
#>  AUC: 0.6451
```

When the state space contains three or more states, we report the
generalised AUC proposed by Hand and Till in [A Simple Generalisation of
the Area Under the ROC Curve for Multiple Class Classification
Problems](https://doi.org/10.1023/A:1010920819831).
