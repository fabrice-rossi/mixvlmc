# Computing engines and control of VLMC with covariates

``` r

library(mixvlmc)
```

A VLMC with covariates (COVLMC, see
[`vignette("covlmc")`](https://fabrice-rossi.github.io/mixvlmc/articles/covlmc.md))
associates a logistic model to each of its contexts. Those models are
fitted by external packages, called *computing engines* in `mixvlmc`.
This article describes the available engines, their differences and the
parameters that control the estimation of the logistic models. It also
explains how to reduce the memory occupied by COVLMC objects, which
store the fitted models.

## Computing engines

### Choosing an engine

The engine is selected with the `mixvlmc.predictive` option (see
`?mixvlmc-package` for the list of the package options). Two values are
supported:

- `"glm"` (default value):
  [`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc.md)
  uses [`stats::glm()`](https://rdrr.io/r/stats/glm.html) with the
  binomial family when the time series has two states, and
  [`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html) with the
  multinomial family when it has three states or more;
- `"multinom"`:
  [`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc.md)
  uses [`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html)
  in all cases.

The engine can be changed for the rest of the session with
[`options()`](https://rdrr.io/r/base/options.html):

``` r

old_options <- options(mixvlmc.predictive = "multinom")
## covlmc() and tune_covlmc() use now nnet::multinom()
options(old_options) ## back to the previous engine
```

To use an engine only for some computations, it is simpler to rely on
[`withr::with_options()`](https://withr.r-lib.org/reference/with_options.html)
(or on
[`withr::local_options()`](https://withr.r-lib.org/reference/with_options.html)
inside a function), as done in the rest of this article.

The engine used by
[`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc.md)
is recorded in the model. When
[`prune()`](https://fabrice-rossi.github.io/mixvlmc/reference/prune.md)
fits new models for a COVLMC
([`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_covlmc.md)
relies on
[`prune()`](https://fabrice-rossi.github.io/mixvlmc/reference/prune.md)),
it uses the recorded engine, with a warning if the option has been
changed in between.

### A running example

We use the household electricity consumption data set
(`powerconsumption`, see also
[`vignette("covlmc")`](https://fabrice-rossi.github.io/mixvlmc/articles/covlmc.md)),
restricted to its fifth week and discretised in three states. The
covariate indicates whether the measurement was made during the day.

``` r

pc_week_5 <- powerconsumption[powerconsumption$week == 5, ]
elec_dts <- cut(pc_week_5$active_power,
  breaks = c(0, 0.4, 2, 8),
  labels = c("low", "typical", "high")
)
elec_cov <- data.frame(day = pc_week_5$hour >= 7 & pc_week_5$hour <= 17)
```

We select a model with the AIC with the default engine (here
[`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html) as the time
series has three states):

``` r

elec_glm <- tune_covlmc(elec_dts, elec_cov, criterion = "AIC")
elec_glm
#> VLMC with covariates context tree on low, typical, high 
#>  cutoff in quantile scale: 0.1715
#>  Number of contexts: 5 
#>  Maximum context length: 2 
#>  Selected by AIC (1000.052) with likelihood function "truncated" (-487.0262)
```

and with
[`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html):

``` r

elec_multinom <- withr::with_options(
  list(mixvlmc.predictive = "multinom"),
  tune_covlmc(elec_dts, elec_cov, criterion = "AIC")
)
elec_multinom
#> VLMC with covariates context tree on low, typical, high 
#>  cutoff in quantile scale: 0.1715
#>  Number of contexts: 5 
#>  Maximum context length: 2 
#>  Selected by AIC (1000.053) with likelihood function "truncated" (-487.0264)
```

The two engines lead to the same context tree, with very close
likelihoods and AIC. The models are shown below with their coefficients,
first for [`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html):

``` r

model_glm <- as_covlmc(elec_glm)
draw(model_glm, model = "full")
#> ▪
#> ├─ low ([ (I)    • day_1TRUE
#> │         -1.558 • 1.006     ])
#> ├─ typical
#> │  ├─ low ([ (I)   
#> │  │         0.3365
#> │  │         -1.609 ])
#> │  ├─ typical ([ (I)   
#> │  │             2.937 
#> │  │             0.3747 ])
#> │  └─ high ([ (I)  
#> │             2.773
#> │             1.705 ])
#> └─ high ([ (I)   • day_1TRUE
#>            17.41 • -14.23   
#>            19.38 • -14.88    ])
```

and then for
[`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html):

``` r

model_multinom <- as_covlmc(elec_multinom)
draw(model_multinom, model = "full")
#> ▪
#> ├─ low ([ (I)    • day_1TRUE
#> │         -1.558 • 1.006     ])
#> ├─ typical
#> │  ├─ low ([ (I)   
#> │  │         0.3365
#> │  │         -1.609 ])
#> │  ├─ typical ([ (I)   
#> │  │             2.937 
#> │  │             0.3747 ])
#> │  └─ high ([ (I)  
#> │             2.773
#> │             1.705 ])
#> └─ high ([ (I)   • day_1TRUE
#>            11.56 • -8.385   
#>            13.53 • -9.029    ])
```

Most of the coefficients are identical (up to the displayed precision),
but the ones of the `high` context are very different.

### Estimation algorithms and separable data

The engines maximise the same likelihood, but with different algorithms:
[`stats::glm()`](https://rdrr.io/r/stats/glm.html) and
[`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html) use iteratively
reweighted least squares, while
[`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html) uses a
quasi-Newton optimisation method (BFGS). When the maximum likelihood
estimator exists, the algorithms reach it (up to numerical precision)
and the coefficients agree, as in most of the contexts above.

This is not the case when the data of a context are (almost) separable,
for instance when a value of a covariate determines the next state. Let
us look at the states that follow the `high` state, depending on the
value of the covariate:

``` r

elec_states <- as.character(elec_dts)
after_high <- which(head(elec_states, -1) == "high")
high_table <- table(
  day = elec_cov$day[after_high],
  next_state = elec_states[after_high + 1]
)
high_table
#>        next_state
#> day     high low typical
#>   FALSE  150   0      21
#>   TRUE    90   1      24
```

The `high` state is never followed by the `low` one at night and very
rarely during the day (in 1 of the 115 transitions from `high`). With
`low` as the reference state, the logistic model of the `high` context
describes the logarithm of the ratio between the probability of
`typical` (respectively `high`) and the one of `low` as the sum of an
intercept and of a coefficient for `day_1TRUE`, the value of the
covariate at the time of the last `high` state. As the probability of
`low` at night is zero in the data, the maximum likelihood intercepts
are infinite (and the coefficients of `day_1TRUE` are minus infinity, as
`low` has a non-zero probability during the day).

The algorithms therefore make the intercepts grow until their stopping
criterion is met: as the likelihood barely changes once the probability
of `low` at night is small enough, the algorithms stop at different
(large) values. We can compare the probabilities estimated by the two
models of the `high` context:

``` r

high_probs <- function(fitted) {
  coefs <- unname(as.matrix(model(find_sequence(fitted, "high"))))
  ## logits with respect to the reference state (low)
  eta <- rbind(night = c(0, coefs[, 1]), day = c(0, coefs[, 1] + coefs[, 2]))
  probs <- exp(eta) / rowSums(exp(eta))
  colnames(probs) <- states(fitted)
  probs
}
high_probs(model_glm)
#>                low   typical      high
#> night 3.366147e-09 0.1228070 0.8771930
#> day   8.695652e-03 0.2086957 0.7826087
high_probs(model_multinom)
#>                low   typical      high
#> night 1.168289e-06 0.1228000 0.8771989
#> day   8.695001e-03 0.2086925 0.7826125
```

The probabilities agree except for the probability of `low` at night,
which is tiny in both cases but differs by several orders of magnitude.
This has almost no effect on the likelihood:

``` r

logLik(model_glm)
#> 'log Lik.' -487.0262 (df=13)
logLik(model_multinom)
#> 'log Lik.' -487.0264 (df=13)
```

In general, such differences have no practical consequences. However the
coefficients of separable contexts should not be interpreted (only their
signs are meaningful), and as
[`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc.md)
relies on likelihood ratio tests to simplify the models, a test whose
p-value is close to the cut off `alpha` can lead to different decisions
with the two engines, and thus to different models.

### Convergence and the `mixvlmc.maxit` option

The maximal number of iterations of the engines is given by the
`mixvlmc.maxit` option (100 by default). When it is reached,
[`stats::glm()`](https://rdrr.io/r/stats/glm.html) and
[`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html) report a
warning (a message is reported instead when this concerns a model that
is discarded because it is rank deficient), while
[`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html) does
not.

We illustrate this with a binary version of the electricity consumption
time series, for which the `"glm"` engine uses
[`stats::glm()`](https://rdrr.io/r/stats/glm.html). The following
function counts the warnings emitted by an expression rather than
displaying them:

``` r

count_warnings <- function(expr) {
  nb_warnings <- 0
  value <- withCallingHandlers(expr, warning = function(w) {
    nb_warnings <<- nb_warnings + 1
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = nb_warnings)
}
elec_bin <- cut(pc_week_5$active_power,
  breaks = c(0, 2, 8),
  labels = c("normal", "high")
)
bin_glm <- covlmc(elec_bin, elec_cov, alpha = 0.1)
bin_glm_5 <- withr::with_options(
  list(mixvlmc.maxit = 5),
  count_warnings(covlmc(elec_bin, elec_cov, alpha = 0.1))
)
bin_glm_5$warnings
#> [1] 8
```

With at most 5 iterations,
[`stats::glm()`](https://rdrr.io/r/stats/glm.html) reports 8 convergence
warnings, but the resulting model has the same likelihood as the one
obtained with the default number of iterations:

``` r

logLik(bin_glm)
#> 'log Lik.' -283.846 (df=5)
logLik(bin_glm_5$value)
#> 'log Lik.' -283.846 (df=5)
```

Indeed most of the warnings concern models fitted on (almost) separable
data, for which there is no finite solution to converge to: stopping the
iterations earlier has almost no effect on their likelihood. The
situation is different with
[`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html):

``` r

bin_multinom <- withr::with_options(
  list(mixvlmc.predictive = "multinom"),
  covlmc(elec_bin, elec_cov, alpha = 0.1)
)
bin_multinom_5 <- withr::with_options(
  list(mixvlmc.predictive = "multinom", mixvlmc.maxit = 5),
  count_warnings(covlmc(elec_bin, elec_cov, alpha = 0.1))
)
bin_multinom_5$warnings
#> [1] 0
```

No warning is reported, but the model obtained with at most 5 iterations
is not the one obtained with the default value:

``` r

logLik(bin_multinom)
#> 'log Lik.' -283.846 (df=5)
logLik(bin_multinom_5$value)
#> 'log Lik.' -286.9373 (df=4)
context_number(bin_multinom)
#> [1] 4
context_number(bin_multinom_5$value)
#> [1] 3
```

With three states or more,
[`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html) also stops
after `mixvlmc.maxit` iterations and reports convergence warnings, as
shown below with the three-state time series:

``` r

elec_vglm <- covlmc(elec_dts, elec_cov, alpha = 0.1)
elec_vglm_5 <- withr::with_options(
  list(mixvlmc.maxit = 5),
  count_warnings(covlmc(elec_dts, elec_cov, alpha = 0.1))
)
elec_vglm_5$warnings
#> [1] 11
logLik(elec_vglm)
#> 'log Lik.' -487.0262 (df=13)
logLik(elec_vglm_5$value)
#> 'log Lik.' -487.2099 (df=13)
```

Here the context tree is the same (with 5 contexts), but the likelihood
is slightly lower with at most 5 iterations, as some of the logistic
models have not converged.

The default value of `mixvlmc.maxit` should be sufficient in most cases.
It can be increased if
[`stats::glm()`](https://rdrr.io/r/stats/glm.html) or
[`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html) report
convergence warnings on contexts that are not separable. With
[`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html), the
absence of warning does not guarantee convergence: fitting the model
again with a larger `mixvlmc.maxit` and comparing the results is the
simplest check.

### States and levels absent from a context

The data associated with a context do not necessarily contain all the
states. In the example above, the `low` state is never followed by the
`high` one, as shown by the detailed frequencies of the contexts:

``` r

contexts(model_glm, frequency = "detailed")
#>        context freq low typical high
#> 1          low  167 128      39    0
#> 2 low, typical   39  15      21    3
#> 3 typical,....  469  22     415   32
#> 4 high, ty....   45   2      32   11
#> 5         high  286   1      45  240
```

The states that never follow a context are ignored by the model of this
context, which gives them a zero probability and has fewer coefficients:
the model of the `low` context has only one row of coefficients (it
describes the probability of `typical` relative to `low`) while the
model of the `high` context has two. This holds for all the engines.

A practical consequence is that the likelihood of new data in which
`low` is followed by `high` is zero. This is the case of the sixth week
of the data set:

``` r

pc_week_6 <- powerconsumption[powerconsumption$week == 6, ]
elec_dts_6 <- cut(pc_week_6$active_power,
  breaks = c(0, 0.4, 2, 8),
  labels = c("low", "typical", "high")
)
elec_cov_6 <- data.frame(day = pc_week_6$hour >= 7 & pc_week_6$hour <= 17)
loglikelihood(model_glm, newdata = elec_dts_6, newcov = elec_cov_6)
#> 'log Lik.' -Inf (df= 13, nb obs.= 1006, initial="truncated")
```

Similarly, levels of factor covariates that are absent from the data of
a context are ignored by its model. The engines report this differently:
[`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html) gives no
coefficient for those levels, while
[`stats::glm()`](https://rdrr.io/r/stats/glm.html) and
[`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html) give
them a zero coefficient. We illustrate this with an artificial time
series with three states whose dynamics depend on a factor covariate `z`
with three levels. The level `w` can only be observed after state `a`:

``` r

set.seed(2)
nb_obs <- 600
probs <- list(u = c(0.6, 0.2, 0.2), v = c(0.2, 0.6, 0.2), w = c(0.2, 0.2, 0.6))
sim_x <- character(nb_obs)
sim_z <- character(nb_obs)
sim_x[1] <- "a"
sim_z[1] <- "w"
for (t in 2:nb_obs) {
  ## the next state depends on the previous value of the covariate
  sim_x[t] <- sample(c("a", "b", "c"), 1, prob = probs[[sim_z[t - 1]]])
  ## w is only observed after a
  if (sim_x[t] == "a") {
    sim_z[t] <- sample(c("u", "v", "w"), 1)
  } else {
    sim_z[t] <- sample(c("u", "v"), 1)
  }
}
sim_cov <- data.frame(z = factor(sim_z))
table(sim_x, sim_z)
#>      sim_z
#> sim_x   u   v   w
#>     a  81  78  78
#>     b 101 123   0
#>     c  71  68   0
```

In the contexts `b` and `c`, the level `w` is absent from the data.
[`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html) reports no
coefficient for it:

``` r

sim_glm <- covlmc(sim_x, sim_cov)
draw(sim_glm, model = "full")
#> ▪
#> ├─ a ([ (I)     • z_1v  z_1w  
#> │       -0.9676 • 1.59  0.5368
#> │       -1.427  • 1.044 2.238  ])
#> ├─ b ([ (I)     • z_1v  
#> │       -0.9163 • 1.67  
#> │       -1.261  • 0.8864 ])
#> └─ c ([ (I)    • z_1v 
#>         -1.006 • 2.392
#>         -1.006 • 1.173 ])
```

while [`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html)
reports a zero coefficient:

``` r

sim_multinom <- withr::with_options(
  list(mixvlmc.predictive = "multinom"),
  covlmc(sim_x, sim_cov)
)
draw(sim_multinom, model = "full")
#> ▪
#> ├─ a ([ (I)     • z_1v  z_1w  
#> │       -0.9676 • 1.59  0.5368
#> │       -1.427  • 1.044 2.238  ])
#> ├─ b ([ (I)     • z_1v   z_1w
#> │       -0.9163 • 1.67   0   
#> │       -1.261  • 0.8864 0    ])
#> └─ c ([ (I)    • z_1v  z_1w
#>         -1.006 • 2.392 0   
#>         -1.006 • 1.173 0    ])
```

In both cases, the coefficient of `w` in the contexts `b` and `c` is not
estimated: the models are identical and so are their likelihoods.

``` r

logLik(sim_glm)
#> 'log Lik.' -574.9331 (df=14)
logLik(sim_multinom)
#> 'log Lik.' -574.9331 (df=14)
```

The LaTeX representation produced by
[`draw()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.md)
(see the article [Drawing context trees and
models](https://fabrice-rossi.github.io/mixvlmc/articles/drawing.md))
represents those coefficients by zeros whatever the engine, for instance
with [`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html):

``` r

draw(sim_glm, "latex")
#> \begin{forest}
#> [\textbf{$\epsilon$}
#> [\multicolumn{3}{c}{\textbf{a}}\\\hline
#> {\small (I)}&{\small -0.9676}&{\small -1.427}\\
#> {\small $\text{z[v]}_{t-1}$}&{\small 1.59}&{\small 1.044}\\
#> {\small $\text{z[w]}_{t-1}$}&{\small 0.5368}&{\small 2.238}\\
#> , align=ccc]
#> [\multicolumn{3}{c}{\textbf{b}}\\\hline
#> {\small (I)}&{\small -0.9163}&{\small -1.261}\\
#> {\small $\text{z[v]}_{t-1}$}&{\small 1.67}&{\small 0.8864}\\
#> {\small $\text{z[w]}_{t-1}$}&{\small 0}&{\small 0}\\
#> , align=ccc]
#> [\multicolumn{3}{c}{\textbf{c}}\\\hline
#> {\small (I)}&{\small -1.006}&{\small -1.006}\\
#> {\small $\text{z[v]}_{t-1}$}&{\small 2.392}&{\small 1.173}\\
#> {\small $\text{z[w]}_{t-1}$}&{\small 0}&{\small 0}\\
#> , align=ccc]]
#> \end{forest}
```

### Number of parameters

The number of parameters of a COVLMC, used by
[`AIC()`](https://rdrr.io/r/stats/AIC.html) and
[`BIC()`](https://rdrr.io/r/stats/AIC.html), is the `df` attribute of
its log likelihood (see the “Number of parameters” section of
[`?loglikelihood.covlmc`](https://fabrice-rossi.github.io/mixvlmc/reference/loglikelihood.covlmc.md)).
It does not depend on the engine: the coefficients that are not
estimated, such as the ones of the states or levels absent from the data
of a context, are not counted, regardless of the way the engine reports
them.

More precisely, in a time series with \\s\\ states, the model of a
context followed by \\k\\ different states in the data has \\s-1\\
intercepts and \\(k-1)p\\ coefficients for the covariates, where \\p\\
is the number of parameters associated with the covariates: for each
time step of the memory of the model, one per numerical or logical
covariate and the number of levels minus one per factor covariate
(counting only the levels present in the data of the context). The
\\s-1\\ intercepts are counted even if some states never follow the
context, as for the transition probabilities of a context of a VLMC: a
context followed by a single state has therefore \\s-1\\ parameters (see
the next section). This ensures that each context has a cost in the
information criteria, as in the theory of context tree selection. The
following code recomputes the number of parameters of each context of
the electricity consumption example from its detailed frequencies and
from the memory of its model:

``` r

elec_ctx <- contexts(model_glm, frequency = "detailed", hsize = TRUE)
s <- length(states(model_glm))
k <- rowSums(elec_ctx[, as.character(states(model_glm))] > 0)
p <- elec_ctx$hsize
elec_ctx$parameters <- (s - 1) + (k - 1) * p
elec_ctx$nominal <- (s - 1) * (1 + p)
elec_ctx[, c("context", "hsize", "parameters", "nominal")]
#>        context hsize parameters nominal
#> 1          low     1          3       4
#> 2 low, typical     0          2       2
#> 3 typical,....     0          2       2
#> 4 high, ty....     0          2       2
#> 5         high     1          4       4
sum(elec_ctx$parameters)
#> [1] 13
attr(logLik(model_glm), "df")
#> [1] 13
```

For instance, the `low` context (first row) is followed by \\k = 2\\
different states and its model has a memory of 1 time step of the
logical covariate, thus \\p = 1\\: its model has \\s-1 = 2\\ intercepts
and \\(k-1)p = 1\\ coefficient for the covariate, that is 3 parameters.

The likelihood ratio tests used by
[`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc.md)
to simplify the models use instead the *nominal* number of parameters,
\\(s-1)(1+p)\\, where \\p\\ counts all the levels of the factor
covariates, as in the original algorithm. The two numbers differ only
for the contexts whose model uses the covariates and that are not
followed by all the states (such as the `low` context) or, with factor
covariates, whose data do not contain all the levels. In this example,
the sum of the nominal numbers of parameters of the contexts would be 14
instead of 13.

## Degenerate contexts

When a context is always followed by the same state in the data, the
maximum likelihood estimate of the probability of this state is one, and
the logistic models are degenerate.
[`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc.md)
does not call the engine in this case but uses a *constant model* that
does not depend on the covariates. Its probabilities are regularised:
they are computed after adding `pseudo_obs` fake observations of each of
the states to the observed ones. If the context is followed \\n\\ times
by the state \\x\\ and the state space contains \\s\\ states, the
probability of \\x\\ is \\
\frac{n+\text{pseudo\\obs}}{n+s\times\text{pseudo\\obs}}, \\ while the
other states have a probability of
\\\frac{\text{pseudo\\obs}}{n+s\times\text{pseudo\\obs}}\\. This
corresponds to a Bayesian posterior mean estimation with a Dirichlet
prior. The `pseudo_obs` parameter is set with
[`covlmc_control()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc_control.md)
and defaults to 1.

We illustrate this with an artificial binary time series in which state
1 is always followed by state 0, while the probability of switching from
0 to 1 increases with a numerical covariate:

``` r

set.seed(1)
nb_obs <- 500
deg_cov <- data.frame(y = runif(nb_obs))
deg_x <- integer(nb_obs)
for (t in 2:nb_obs) {
  if (deg_x[t - 1] == 1) {
    deg_x[t] <- 0L
  } else {
    deg_x[t] <- rbinom(1, 1, 0.2 + 0.6 * deg_cov$y[t - 1])
  }
}
deg_model <- covlmc(deg_x, deg_cov)
draw(deg_model, model = "full")
#> ▪
#> ├─ 0 ([ (I)    • y_1  
#> │       -1.371 • 2.232 ])
#> └─ 1 ([ (I)  
#>         -5.03 ])
```

The context 1 has a constant model, with only an intercept. As the time
series has two states, this intercept is the logit of the probability of
state 1. In the data, the context is followed 152 times by state 0:

``` r

deg_ctx <- contexts(deg_model, frequency = "detailed", model = "coef")
deg_ctx
#>   context freq   0   1         coef
#> 1       0  347 195 152 -1.37084....
#> 2       1  152 152   0 -5.03043....
```

and the probability of state 1 after state 1 is therefore `1 / (n + 2)`
(with `pseudo_obs = 1`):

``` r

n_1 <- deg_ctx[["0"]][2]
plogis(deg_ctx$coef[[2]][1])
#> [1] 0.006493506
1 / (n_1 + 2)
#> [1] 0.006493506
```

The `pseudo_obs` parameter can be given to
[`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc.md)
(and to
[`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_covlmc.md))
via a `control` list created by
[`covlmc_control()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc_control.md).
[`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc.md)
also accepts it directly as an argument, which is passed to
[`covlmc_control()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc_control.md).
Larger values lead to less extreme probabilities:

``` r

pseudo_obs_values <- c(0.1, 1, 10)
deg_models <- lapply(pseudo_obs_values, function(po) {
  covlmc(deg_x, deg_cov, control = covlmc_control(pseudo_obs = po))
})
data.frame(
  pseudo_obs = pseudo_obs_values,
  prob_1_after_1 = sapply(deg_models, function(m) {
    plogis(contexts(m, model = "coef")$coef[[2]][1])
  }),
  expected = pseudo_obs_values / (n_1 + 2 * pseudo_obs_values),
  loglikelihood = sapply(deg_models, function(m) as.numeric(logLik(m)))
)
#>   pseudo_obs prob_1_after_1     expected loglikelihood
#> 1        0.1   0.0006570302 0.0006570302     -222.8753
#> 2        1.0   0.0064935065 0.0064935065     -223.7656
#> 3       10.0   0.0581395349 0.0581395349     -231.8799
```

The regularised probabilities are used to compute the likelihood, which
decreases when `pseudo_obs` increases as the probability of the observed
transition decreases. Whatever the value of `pseudo_obs`, the model of a
context followed by a single state has \\s-1\\ parameters, its
intercepts (see the previous section), so `pseudo_obs` has no effect on
the number of parameters:

``` r

attr(logLik(deg_models[[1]]), "df")
#> [1] 3
attr(logLik(deg_models[[3]]), "df")
#> [1] 3
```

Notice that the regularisation applies only to contexts followed by a
single state: in the other contexts, states that never follow the
context have a zero probability, as explained above.

## Memory occupation and trimming

### What a COVLMC stores

A `covlmc` object contains the fitted engine objects (one per context,
plus the ones used to compute the likelihood of the first observations,
see
[`vignette("likelihood")`](https://fabrice-rossi.github.io/mixvlmc/articles/likelihood.md))
as well as the data used to fit them. When `keep_data = TRUE` (the
default value),
[`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc.md)
also stores the original time series and covariates, and caches some
models, in order to enable post-pruning with
[`prune()`](https://fabrice-rossi.github.io/mixvlmc/reference/prune.md)
(and thus
[`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_covlmc.md)).
The engine objects themselves store quantities whose size grows with the
number of observations (such as fitted values and residuals) and their
formulas reference the environment in which they were created.

As a consequence, COVLMC objects can be large. Two measures of size are
given below: [`object.size()`](https://rdrr.io/r/utils/object.size.html)
ignores environments, while the size of the serialised object includes
them and corresponds to what is written by
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html) (without compression)
or sent to the workers in parallel processing. Neither is exactly the
memory used by the object in the R session.

``` r

obj_sizes <- function(...) {
  objects <- list(...)
  data.frame(
    object_size_kb = sapply(objects, function(o) {
      round(as.numeric(object.size(o)) / 1024)
    }),
    serialised_kb = sapply(objects, function(o) {
      round(length(serialize(o, NULL)) / 1024)
    })
  )
}
obj_sizes(
  "VGAM::vglm()" = model_glm,
  "nnet::multinom()" = model_multinom
)
#>                  object_size_kb serialised_kb
#> VGAM::vglm()               5264          2697
#> nnet::multinom()           1132           683
```

The two measures do not agree, as they do not account for the components
of the objects in the same way, but both show that the models fitted by
[`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html) are much larger
than the ones fitted by
[`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html).

### Trimming a model

The
[`trim()`](https://fabrice-rossi.github.io/mixvlmc/reference/trim.md)
function (see
[`trim.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/trim.covlmc.md))
removes the data and the cached models from a COVLMC. With
`keep_model = TRUE`, the engine objects are kept but simplified (with
[`butcher::butcher()`](https://butcher.tidymodels.org/reference/butcher.html)
for [`stats::glm()`](https://rdrr.io/r/stats/glm.html) and
[`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html)) and
the environments referenced by their formulas are discarded. With
`keep_model = FALSE` (default), the engine objects are removed and only
their coefficients are kept.

``` r

obj_sizes(
  "untrimmed" = model_glm,
  "trim(keep_model = TRUE)" = trim(model_glm, keep_model = TRUE),
  "trim()" = trim(model_glm)
)
#>                         object_size_kb serialised_kb
#> untrimmed                         5264          2697
#> trim(keep_model = TRUE)           3893           702
#> trim()                              36             7
```

The fully trimmed model is very small, but it supports fewer operations.
The following table summarises what can be done with a model trimmed
with `keep_model = TRUE` or `keep_model = FALSE` (see the documentation
of the corresponding functions):

| Operation | `TRUE` | `FALSE` |
|:---|:--:|:--:|
| [`logLik()`](https://rdrr.io/r/stats/logLik.html), [`AIC()`](https://rdrr.io/r/stats/AIC.html), [`BIC()`](https://rdrr.io/r/stats/AIC.html) | yes | yes |
| [`draw()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.md), `contexts(model = "coef")` | yes | yes |
| `contexts(metrics = TRUE)` | yes | yes |
| [`metrics()`](https://fabrice-rossi.github.io/mixvlmc/reference/metrics.md) of the model (except the AUC) | yes | yes |
| `contexts(model = "full")` | yes | no |
| `model(type = "full")` | yes | no |
| [`loglikelihood()`](https://fabrice-rossi.github.io/mixvlmc/reference/loglikelihood.md) on new data | yes | no |
| [`predict()`](https://rdrr.io/r/stats/predict.html), [`simulate()`](https://rdrr.io/r/stats/simulate.html) | yes | no |
| [`positions()`](https://fabrice-rossi.github.io/mixvlmc/reference/positions.md), `contexts(positions = TRUE)` | no | no |
| [`prune()`](https://fabrice-rossi.github.io/mixvlmc/reference/prune.md) | no | no |

The metrics of a trimmed model are computed from the ones of its
contexts (and of the first observations, computed by
[`trim()`](https://fabrice-rossi.github.io/mixvlmc/reference/trim.md)):
the accuracy and the confusion matrix are available, but the AUC is
missing (with a warning). The likelihood of the original data is also
preserved by trimming, while the likelihood of new data needs the engine
objects:

``` r

elec_trimmed <- trim(model_glm)
logLik(elec_trimmed)
#> 'log Lik.' -487.0262 (df=13)
loglikelihood(trim(model_glm, keep_model = TRUE),
  newdata = elec_dts_6, newcov = elec_cov_6
)
#> 'log Lik.' -Inf (df= 13, nb obs.= 1006, initial="truncated")
loglikelihood(elec_trimmed, newdata = elec_dts_6, newcov = elec_cov_6)
#> Error in `loglikelihood.covlmc()`:
#> ! loglikelihood calculation for new data is not supported by fully trimmed covlmc
```

As
[`prune()`](https://fabrice-rossi.github.io/mixvlmc/reference/prune.md)
is not available for trimmed models, the trimming should be done once
the model selection is completed.

### Trimming in `tune_covlmc()`

[`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_covlmc.md)
builds a sequence of models by pruning an initial large model and can
save some of them. The `trimming` argument specifies the trimming of the
intermediate models (`"full"` by default, which corresponds to
[`trim()`](https://fabrice-rossi.github.io/mixvlmc/reference/trim.md),
`"partial"`, which corresponds to `trim(keep_model = TRUE)`, or
`"none"`), while `best_trimming` applies to the best model and to the
initial one, which are not trimmed by default (see
[`vignette("model-selection")`](https://fabrice-rossi.github.io/mixvlmc/articles/model-selection.md)
and the “Memory occupation” section of
[`?tune_covlmc`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_covlmc.md)).
Trimming all the models, including the best one, is recommended when
memory is an issue, in particular when
[`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_covlmc.md)
is called in parallel processing contexts:

``` r

elec_glm_trimmed <- tune_covlmc(elec_dts, elec_cov,
  criterion = "AIC",
  best_trimming = "full"
)
obj_sizes(
  "default" = elec_glm,
  "best_trimming = \"full\"" = elec_glm_trimmed
)
#>                        object_size_kb serialised_kb
#> default                          5267          2697
#> best_trimming = "full"             39             8
```

The trimmed result still gives access to the selected model and to the
results of the selection process, for instance:

``` r

elec_glm_trimmed
#> VLMC with covariates context tree on low, typical, high 
#>  cutoff in quantile scale: 0.1715
#>  Number of contexts: 5 
#>  Maximum context length: 2 
#>  Selected by AIC (1000.052) with likelihood function "truncated" (-487.0262)
```
