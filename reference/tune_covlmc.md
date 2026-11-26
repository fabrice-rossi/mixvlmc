# Fit an optimal Variable Length Markov Chain with Covariates (COVLMC)

This function fits a Variable Length Markov Chain with Covariates
(COVLMC) to a discrete time series coupled with a time series of
covariates by optimising an information criterion (BIC or AIC).

## Usage

``` r
tune_covlmc(
  x,
  covariate,
  criterion = c("BIC", "AIC"),
  initial = c("truncated", "specific", "extended"),
  alpha_init = NULL,
  min_size = 5,
  max_depth = 100,
  verbose = 0,
  save = c("best", "initial", "all"),
  trimming = c("full", "partial", "none"),
  best_trimming = c("none", "partial", "full"),
  control = covlmc_control()
)
```

## Arguments

- x:

  an object that can be interpreted as a discrete time series: a
  numeric, character, factor or logical vector, or a `dts` object (see
  [`dts()`](https://fabrice-rossi.github.io/mixvlmc/reference/dts.md)).
  Its state space must contain at least two states.

- covariate:

  a data frame of covariates.

- criterion:

  criterion used to select the best model. Either `"BIC"` (default) or
  `"AIC"` (see details).

- initial:

  specifies the likelihood function, more precisely the way the first
  few observations for which contexts cannot be calculated are
  integrated in the likelihood. Defaults to `"truncated"`. See
  [`loglikelihood()`](https://fabrice-rossi.github.io/mixvlmc/reference/loglikelihood.md)
  for details.

- alpha_init:

  if non `NULL`, used as the initial cut off parameter (in quantile
  scale, in (0,1\]) to build the initial COVLMC. The default `NULL`
  corresponds to 0.5 (see details).

- min_size:

  number \>= 1 (default: 5). Tune the minimum number of observations for
  a context in the growing phase of the context tree (see
  [`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc.md)
  for details).

- max_depth:

  integer \>= 0 (default: 100). Longest context considered in growing
  phase of the initial context tree (see details).

- verbose:

  integer \>= 0 (default: 0). Verbosity level of the pruning process.
  The progress is reported with
  [`message()`](https://rdrr.io/r/base/message.html), which can be
  silenced with
  [`suppressMessages()`](https://rdrr.io/r/base/message.html).

- save:

  specifies which models are saved during the pruning process. The
  default value `"best"` asks the function to keep only the best model
  according to the `criterion`. When `save="initial"` the function keeps
  *in addition* the initial (complex) model which is then pruned during
  the selection process. When `save="all"`, the function returns all the
  models considered during the selection process. See details for memory
  occupation.

- trimming:

  specifies the type of trimming used when saving the intermediate
  models: `"full"` (default), `"partial"` or `"none"` (see the Memory
  occupation section).

- best_trimming:

  specifies the type of trimming used when saving the best model and the
  initial one: `"none"` (default), `"partial"` or `"full"` (see the
  Memory occupation section).

- control:

  a list with control parameters used to fit the models, see
  [`covlmc_control()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc_control.md).

## Value

an object of class `tune_covlmc`, a list with the following components:

- `best_model`: the optimal COVLMC

- `criterion`: the criterion used to select the optimal COVLMC

- `initial`: the likelihood function used to select the optimal COVLMC

- `results`: a data frame with details about the pruning process, with
  one row per model and the columns `alpha`, `depth`, `nb_contexts`,
  `loglikelihood`, `cov_depth`, `AIC` and `BIC`

- `saved_models`: a list of intermediate COVLMCs if `save="initial"` or
  `save="all"`. It contains an `initial` component with the large COVLMC
  obtained first and, when `save="all"`, an `all` component with a list
  of all the *other* COVLMCs obtained by pruning the initial one.

## Details

This function automates the process of fitting a large COVLMC to a
discrete time series with
[`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc.md)
and of pruning the tree (with
[`cutoff()`](https://fabrice-rossi.github.io/mixvlmc/reference/cutoff.md)
and
[`prune()`](https://fabrice-rossi.github.io/mixvlmc/reference/prune.md))
to get an optimal model with respect to an information criterion. To
avoid missing long-term dependencies, the function uses the `max_depth`
parameter as an initial guess but then relies on an automatic increase
of the value to make sure the initial context tree is only limited by
the `min_size` parameter. The initial value of the `alpha` parameter of
[`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc.md)
is also set to a conservative value (0.5) to avoid prior simplification
of the context tree. This can be overridden by setting the `alpha_init`
parameter to a more adapted value.

Once the initial COVLMC is obtained, the
[`cutoff()`](https://fabrice-rossi.github.io/mixvlmc/reference/cutoff.md)
and
[`prune()`](https://fabrice-rossi.github.io/mixvlmc/reference/prune.md)
functions are used to build all the COVLMC models that could be
generated using smaller values of the alpha parameter. The best model is
selected from this collection, including the initial complex tree, as
the one that minimises the chosen information criterion.

## Memory occupation

`covlmc` objects tend to be large and saving all the models during the
search for the optimal model can lead to an unreasonable use of memory.
To avoid this problem, intermediate models are kept only in trimmed form
using
[`trim.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/trim.covlmc.md)
with `keep_model=FALSE`. Both the initial model and the best one are
saved untrimmed. This default behaviour corresponds to
`trimming="full"`. Setting `trimming="partial"` asks the function to use
`keep_model=TRUE` in
[`trim.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/trim.covlmc.md)
for intermediate models. Finally, `trimming="none"` turns off trimming,
which is discouraged except for small data sets.

In parallel processing contexts (e.g. using
[foreach::%dopar%](https://rdrr.io/pkg/foreach/man/foreach.html)), the
results are serialised to be sent back to the main process: as the
logistic models keep the data used to fit them (including in the
environments attached to their formulas), this can be costly. It is
highly recommended to trim all saved models, including the best one and
the initial one. This can be done via the `best_trimming` parameter
whose possible values are identical to the ones of `trimming`.

## See also

[`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc.md),
[`cutoff()`](https://fabrice-rossi.github.io/mixvlmc/reference/cutoff.md)
and
[`prune()`](https://fabrice-rossi.github.io/mixvlmc/reference/prune.md)

## Examples

``` r
pc <- powerconsumption[powerconsumption$week %in% 6:7, ]
rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
rdts_best_model_tune <- tune_covlmc(rdts, rdts_cov)
rdts_best_model_tune
#> VLMC with covariates context tree on (0,1.26], (1.26,6.48] 
#>  cutoff in quantile scale: 0.001838
#>  Number of contexts: 6 
#>  Maximum context length: 4 
#>  Selected by BIC (1103.29) with likelihood function "truncated" (-525.026)
draw(as_covlmc(rdts_best_model_tune))
#> ▪
#> ├─ (0,1.26]
#> │  ├─ (0,1.26] (0.931, 0.06897)
#> │  └─ (1.26,6.48] (0.8205, 0.1795)
#> └─ (1.26,6.48]
#>    ├─ (0,1.26] (0.1538, 0.8462)
#>    └─ (1.26,6.48]
#>       ├─ (0,1.26] (0.1667, 0.8333)
#>       └─ (1.26,6.48]
#>          ├─ (0,1.26] ([ 0.47 • 2.862 ])
#>          └─ (1.26,6.48] (0.05439, 0.9456)
## details of the pruning process
rdts_best_model_tune$results
#>           alpha depth nb_contexts loglikelihood cov_depth      AIC      BIC
#> 1  0.5000000000     7          13     -501.1294         6 1072.259 1268.447
#> 2  0.4926494187     7          13     -501.7740         6 1065.548 1239.315
#> 3  0.4311164180     7          13     -502.0840         6 1064.168 1232.330
#> 4  0.4216949807     7          13     -502.4067         6 1062.813 1225.370
#> 5  0.4035040022     7          13     -503.0527         6 1058.105 1203.846
#> 6  0.3183056495     7          13     -503.7435         6 1053.487 1182.411
#> 7  0.2347151079     7          13     -504.4496         6 1052.899 1176.218
#> 8  0.2234759135     7          13     -505.1905         6 1052.381 1170.094
#> 9  0.1663903547     6          10     -508.1799         6 1052.360 1153.257
#> 10 0.1575447785     6          10     -509.1787         6 1052.357 1147.649
#> 11 0.0018376451     4           6     -525.0260         1 1064.052 1103.290
#> 12 0.0006256455     4           6     -530.8751         0 1073.750 1107.382
#> 13 0.0003608438     4           5     -535.5783         0 1081.157 1109.184
#> 14 0.0000799928     1           2     -548.4443         0 1100.889 1112.099
## selection with the AIC, keeping all the (trimmed) intermediate models
rdts_aic_tune <- tune_covlmc(rdts, rdts_cov, criterion = "AIC", save = "all")
length(rdts_aic_tune$saved_models$all)
#> [1] 13
```
