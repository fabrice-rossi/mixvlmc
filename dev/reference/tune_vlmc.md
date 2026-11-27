# Fit an optimal Variable Length Markov Chain (VLMC)

This function fits a Variable Length Markov Chain (VLMC) to a discrete
time series by optimising an information criterion (BIC or AIC).

## Usage

``` r
tune_vlmc(
  x,
  criterion = c("BIC", "AIC"),
  initial = c("truncated", "specific", "extended"),
  alpha_init = NULL,
  cutoff_init = NULL,
  min_size = 2L,
  max_depth = 100L,
  backend = getOption("mixvlmc.backend", "C++"),
  verbose = 0,
  save = c("best", "initial", "all")
)
```

## Arguments

- x:

  an object that can be interpreted as a discrete time series: a
  numeric, character, factor or logical vector, or a `dts` object (see
  [`dts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/dts.md)).
  Its state space must contain at least two states.

- criterion:

  criterion used to select the best model. Either `"BIC"` (default) or
  `"AIC"` (see details).

- initial:

  specifies the likelihood function, more precisely the way the first
  few observations for which contexts cannot be calculated are
  integrated in the likelihood. Defaults to `"truncated"`. See
  [`loglikelihood()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/loglikelihood.md)
  for details.

- alpha_init:

  if non `NULL` used as the initial cut off parameter (in quantile
  scale) to build the initial VLMC.

- cutoff_init:

  if non `NULL` used as the initial cut off parameter (in native scale)
  to build the initial VLMC. Takes precedence over `alpha_init` if
  specified.

- min_size:

  integer \>= 1 (default: 2). Minimum number of observations for a
  context in the growing phase of the initial context tree.

- max_depth:

  integer \>= 0 (default: 100). Longest context considered in the
  growing phase of the initial context tree (see details).

- backend:

  "R" or "C++" (default: as specified by the "mixvlmc.backend" option).
  Specifies the implementation used to represent the context tree and to
  build it. See
  [`vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/vlmc.md)
  for details.

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
  models considered during the selection process.

## Value

an object of class `tune_vlmc`, a list with the following components:

- `best_model`: the optimal VLMC

- `criterion`: the criterion used to select the optimal VLMC

- `initial`: the likelihood function used to select the optimal VLMC

- `results`: a data frame with details about the pruning process, with
  one row per model considered (cut off in native and quantile scales,
  depth, number of contexts, log-likelihood, AIC and BIC)

- `saved_models`: a list of intermediate VLMCs if `save="initial"` or
  `save="all"`. It contains an `initial` component with the large VLMC
  obtained first and, if `save="all"`, an `all` component with a list of
  all the *other* VLMCs obtained by pruning the initial one.

- `cutoffs`: the cut offs in native scale of the models considered (the
  `cutoff` column of `results`): the one of the initial model followed
  by the ones used to prune it.

## Details

This function automates the process of fitting a large VLMC to a
discrete time series with
[`vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/vlmc.md)
and of pruning the tree (with
[`cutoff()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.md)
and
[`prune()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/prune.md))
to get an optimal model with respect to an information criterion. To
avoid missing long term dependencies, the function uses the `max_depth`
parameter as an initial guess but then relies on an automatic increase
of the value to make sure the initial context tree is only limited by
the `min_size` parameter. The initial value of the `cutoff` parameter of
[`vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/vlmc.md)
is also set to conservative values (depending on the criterion) to avoid
prior simplification of the context tree: in native scale, it is set to
`0.25 * log(n)` for the BIC (where `n` is the length of the time series)
and to 1 for the AIC. This default value can be overridden using the
`cutoff_init` or `alpha_init` parameter.

Once the initial VLMC is obtained, the
[`cutoff()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.md)
and
[`prune()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/prune.md)
functions are used to build all the VLMC models that could be generated
using larger values of the initial cut off parameter. The best model is
selected from this collection, including the initial complex tree, as
the one that minimises the chosen information criterion.

## See also

[`vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/vlmc.md),
[`cutoff()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.md)
and
[`prune()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/prune.md),
as well as
[`as_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/as_vlmc.md),
[`plot.tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/plot.tune_vlmc.md)
and
[`autoplot.tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/autoplot.tune_vlmc.md)
to use the results.

## Examples

``` r
pc <- powerconsumption[powerconsumption$week == 5, ]
rdts <- cut(pc$active_power,
  breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1)))
)
tune_result <- tune_vlmc(rdts)
tune_result
#> VLMC context tree on (0,0.458], (0.458,1.34], (1.34,2.13], (2.13,7.54] [C++]
#>  cutoff: 27.65 (quantile: 5.935e-12)
#>  Number of contexts: 4 
#>  Maximum context length: 1 
#>  Selected by BIC (1482.507) with likelihood function "truncated" (-699.8914)
draw(as_vlmc(tune_result))
#> ▪ (0.25, 0.25, 0.25, 0.25)
#> ├─ (0,0.458] (0.7968, 0.1912, 0.007968, 0.003984)
#> ├─ (0.458,1.34] (0.1984, 0.6667, 0.1071, 0.02778)
#> ├─ (1.34,2.13] (0.003968, 0.123, 0.7262, 0.1468)
#> └─ (2.13,7.54] (0.003968, 0.01587, 0.1587, 0.8214)
summary(tune_result)
#> VLMC tune results
#> 
#> Best VLMC selected by BIC (1482.507) with likelihood function "truncated" (-699.8914)
#> VLMC context tree on (0,0.458], (0.458,1.34], (1.34,2.13], (2.13,7.54] [C++]
#>  cutoff: 27.65 (quantile: 5.935e-12)
#>  Number of contexts: 4 
#>  Maximum context length: 1 
#> 
#> Pruning results
#>      cutoff        alpha depth nb_contexts loglikelihood      AIC      BIC
#>    1.728931 3.262686e-01    22          87     -586.3797 1694.759 2972.004
#>    1.932266 2.764686e-01    22          86     -588.2830 1692.566 2955.129
#>    1.983807 2.649825e-01    22          85     -590.2446 1690.489 2938.372
#>    2.011203 2.590549e-01    22          84     -592.2508 1688.502 2921.703
#>    2.033853 2.542452e-01    22          83     -594.2670 1686.534 2905.055
#>    2.058090 2.491889e-01    22          82     -596.3187 1684.637 2888.477
#>    2.071983 2.463322e-01    22          81     -598.3832 1682.766 2871.925
#>    2.087211 2.432352e-01    15          66     -602.5104 1601.021 2569.965
#>    2.097326 2.411979e-01    15          65     -604.6055 1599.211 2553.474
#>    2.147880 2.312476e-01    15          60     -608.0159 1576.032 2456.890
#>    2.218395 2.179983e-01    15          54     -612.3185 1548.637 2341.409
#>    2.269873 2.087723e-01    15          53     -614.5583 1547.117 2325.208
#>    2.307683 2.022278e-01    15          52     -616.8587 1545.717 2309.128
#>    2.326159 1.990996e-01     6          41     -621.4559 1488.912 2090.831
#>    2.364202 1.927997e-01     6          40     -623.7932 1487.586 2074.825
#>    2.486624 1.737665e-01     6          39     -626.1846 1486.369 2058.927
#>    2.606593 1.568358e-01     6          38     -628.7703 1485.541 2043.417
#>    2.635573 1.529856e-01     6          38     -631.3979 1490.796 2048.673
#>    2.660562 1.497375e-01     6          34     -637.5188 1479.038 1978.190
#>    2.679814 1.472796e-01     6          32     -641.5846 1475.169 1944.960
#>    2.696868 1.451344e-01     6          29     -646.2379 1466.476 1892.224
#>    2.734667 1.404843e-01     6          24     -650.7119 1445.424 1797.767
#>    2.778982 1.352125e-01     5          19     -655.1716 1424.343 1703.282
#>    2.902569 1.214853e-01     5          17     -660.3244 1422.649 1672.225
#>    3.018283 1.098447e-01     5          16     -663.3327 1422.665 1657.561
#>    3.470850 7.377936e-02     5          15     -666.3610 1422.722 1642.936
#>    4.241063 3.703083e-02     5          14     -670.3391 1424.678 1630.212
#>    4.852954 2.123878e-02     5          12     -676.2076 1424.415 1600.587
#>    5.265140 1.455689e-02     5          11     -681.4165 1428.833 1590.324
#>    5.469069 1.206514e-02     5           9     -686.8010 1427.602 1559.731
#>    6.113073 6.647295e-03     2           5     -693.2422 1416.484 1489.889
#>   27.648398 5.934644e-12     1           4     -699.8914 1423.783 1482.507
#>  131.023478 1.621825e-56     1           4     -796.4529 1616.906 1675.630
#>  176.210494 4.461210e-76     1           3     -945.7765 1909.553 1953.596
#>  210.406262 6.866482e-91     1           2    -1153.9830 2319.966 2349.328
#>  215.400128 4.709728e-93     0           1    -1366.8862 2739.772 2754.453
## selection with the AIC
tune_result_aic <- tune_vlmc(rdts, criterion = "AIC")
draw(as_vlmc(tune_result_aic))
#> ▪ (0.25, 0.25, 0.25, 0.25)
#> ├─ (0,0.458] (0.7968, 0.1912, 0.007968, 0.003984)
#> ├─ (0.458,1.34] (0.1984, 0.6667, 0.1071, 0.02778)
#> │  └─ (0,0.458] (0.375, 0.4167, 0.1458, 0.0625)
#> ├─ (1.34,2.13] (0.003968, 0.123, 0.7262, 0.1468)
#> └─ (2.13,7.54] (0.003968, 0.01587, 0.1587, 0.8214)
```
