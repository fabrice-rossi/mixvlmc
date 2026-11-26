# Fit a Variable Length Markov Chain with Covariates (COVLMC)

This function fits a Variable Length Markov Chain with covariates
(COVLMC) to a discrete time series coupled with a time series of
covariates.

## Usage

``` r
covlmc(
  x,
  covariate,
  alpha = 0.05,
  min_size = 5L,
  max_depth = 100L,
  keep_data = TRUE,
  control = covlmc_control(...),
  ...
)
```

## Arguments

- x:

  an object that can be interpreted as a discrete time series: a
  numeric, character, factor or logical vector, or a `dts` object (see
  [`dts()`](https://fabrice-rossi.github.io/mixvlmc/reference/dts.md)).
  Its state space must contain at least two states.

- covariate:

  a data frame of covariates, with one row per observation of `x`, at
  least one column and no missing values.

- alpha:

  number in (0,1\] (default: 0.05) cut off value in the pruning phase
  (in quantile scale).

- min_size:

  number \>= 1 (default: 5). Tune the minimum number of observations for
  a context in the growing phase of the context tree (see below for
  details).

- max_depth:

  integer \>= 0 (default: 100). Longest context considered in the
  growing phase of the context tree.

- keep_data:

  logical (defaults to `TRUE`). If `TRUE`, the original data are stored
  in the resulting object to enable post-pruning (see
  [`prune.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/prune.covlmc.md)).

- control:

  a list with control parameters, see
  [`covlmc_control()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc_control.md).

- ...:

  arguments passed to
  [`covlmc_control()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc_control.md).

## Value

a fitted VLMC with covariates, an object of class `covlmc`.

## Details

The model is built using the algorithm described in Zanin Zambom et al.
As for the
[`vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/vlmc.md)
approach, the algorithm builds first a context tree (see
[`ctx_tree()`](https://fabrice-rossi.github.io/mixvlmc/reference/ctx_tree.md)).
The `min_size` parameter is used to compute the actual number of
observations per context in the growing phase of the tree. It is
computed as `min_size*(1+p*d)*(s-1)` where `d` is the length of the
context (a.k.a. the depth in the tree), `s` is the number of states and
`p` is the number of parameters associated with one time step of the
covariates (one per numerical or logical covariate, and the number of
levels minus one per factor). This corresponds to ensuring `min_size`
observations per parameter of the logistic regression during the
estimation phase.

Then logistic models are fitted in the leaves of the tree: the goal of
each logistic model is to estimate the conditional distribution of the
next state of the time series given the context (the recent past of the
time series) and delayed versions of the covariates. A pruning strategy
is used to simplify the models (mainly to reduce the time window
associated with the covariates) and the tree itself.

Parameters specified by `control` are used to fine-tune the behaviour of
the algorithm.

## Logistic models

By default, `covlmc` uses two different computing *engines* for logistic
models:

- when the time series has only two states, `covlmc` uses
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html) with the binomial
  family ([`stats::binomial()`](https://rdrr.io/r/stats/family.html));

- when the time series has at least three states, `covlmc` uses
  [`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html) with the
  multinomial family
  ([`VGAM::multinomial()`](https://rdrr.io/pkg/VGAM/man/multinomial.html)).

It is possible to replace
[`stats::glm()`](https://rdrr.io/r/stats/glm.html) and
[`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html) with
[`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html) by
setting the global option `mixvlmc.predictive` to `"multinom"` (the
default value is `"glm"`). The engines estimate the same models, but
their results are not always identical:

- [`stats::glm()`](https://rdrr.io/r/stats/glm.html) and
  [`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html) use
  iteratively reweighted least squares while
  [`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html) uses
  a quasi-Newton optimisation. When the data of a context are (almost)
  separable, for instance when a value of a covariate determines the
  next state, the maximum likelihood coefficients are infinite and the
  engines stop at different (large) values. The likelihoods remain very
  close, but the coefficients can be quite different and a likelihood
  ratio test whose p-value is close to `alpha` can lead to different
  decisions, and thus to different models.

- [`stats::glm()`](https://rdrr.io/r/stats/glm.html) and
  [`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html) report a
  warning when they do not converge in `mixvlmc.maxit` iterations (see
  [mixvlmc-package](https://fabrice-rossi.github.io/mixvlmc/reference/mixvlmc-package.md)),
  while [`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html)
  does not.

- the states that never follow a context in the data are ignored by the
  model of this context, which gives them a zero probability and has
  fewer coefficients. Similarly, levels of factor covariates absent from
  the data of a context are ignored:
  [`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html) reports no
  coefficient for them while
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html) and
  [`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html)
  report a zero coefficient (for instance in the text format of
  [`draw.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.covlmc.md);
  the LaTeX format shows zero coefficients for all the engines).

The engine used to estimate a COVLMC is recorded in the model: it is
also used to estimate the new logistic models needed by
[`prune.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/prune.covlmc.md),
even if the option has been changed in between (with a warning).

The number of parameters of a COVLMC, used by
[`stats::AIC()`](https://rdrr.io/r/stats/AIC.html) and
[`stats::BIC()`](https://rdrr.io/r/stats/AIC.html), counts the
coefficients of the covariates actually estimated by the engine (as a
consequence of the last point of the list above) and all the intercepts:
it does not depend on the engine (see the "Number of parameters" section
of
[`loglikelihood.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/loglikelihood.covlmc.md)).

## References

- Bühlmann, P. and Wyner, A. J. (1999), "Variable length Markov chains."
  Ann. Statist. 27 (2) 480-513
  [doi:10.1214/aos/1018031204](https://doi.org/10.1214/aos/1018031204)

- Zanin Zambom, A., Kim, S. and Lopes Garcia, N. (2022), "Variable
  length Markov chain with exogenous covariates." J. Time Ser. Anal.,
  43 (2) 312-328
  [doi:10.1111/jtsa.12615](https://doi.org/10.1111/jtsa.12615)

## See also

[`cutoff.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/cutoff.covlmc.md)
and
[`prune.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/prune.covlmc.md)
for post-pruning,
[`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_covlmc.md)
for automatic model selection.

## Examples

``` r
pc <- powerconsumption[powerconsumption$week == 5, ]
rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power,
  probs = c(1 / 3, 2 / 3, 1)
)))
rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
m_cov <- covlmc(rdts, rdts_cov, min_size = 15)
draw(m_cov)
#> ▪
#> ├─ (0,0.556] ([ -2.885 • 1.237
#> │               -4.185 • -15.4 ])
#> ├─ (0.556,1.78] (0.1012, 0.7827, 0.1161)
#> └─ (1.78,7.54] (0.002976, 0.122, 0.875)
withr::with_options(
  list(mixvlmc.predictive = "multinom"),
  m_cov_nnet <- covlmc(rdts, rdts_cov, min_size = 15)
)
draw(m_cov_nnet)
#> ▪
#> ├─ (0,0.556] ([ -2.885 • 1.237 
#> │               -4.185 • -7.944 ])
#> ├─ (0.556,1.78] (0.1012, 0.7827, 0.1161)
#> └─ (1.78,7.54] (0.002976, 0.122, 0.875)
```
