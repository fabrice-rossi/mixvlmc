# Variable Length Markov Chains with Covariates (COVLMC)

``` r

library(mixvlmc)
library(ggplot2)
```

## Limitations of Variable Length Markov Chains

Variable Length Markov Chains (VLMC) are very useful to capture complex
structures in discrete time series as they can mix short memory with
long memory in a contextual way, leading to sparse models.

However, they cannot capture the influence of exogenous variables on the
behaviour of a time series. As a consequence, a VLMC fitted to sequences
influenced by covariates could fail to capture interesting patterns.

### A typical example

Let us focus again on the electrical usage data set used in the package
introduction, using the following two weeks of data:

``` r

pc_week_15_16 <- powerconsumption[powerconsumption$week %in% c(15, 16), ]
elec <- pc_week_15_16$active_power
ggplot(pc_week_15_16, aes(x = date_time, y = active_power)) +
  geom_line() +
  xlab("Date") +
  ylab("Active power (kW)")
```

![Line plot of the active power in kW over two weeks of household
electricity consumption. Daily cycles are clearly visible, with low
plateaus at night and long periods of stable moderate use punctuated by
sharp peaks during the
day.](covlmc_files/figure-html/active_power_week_15_16-1.png)

We build again a discrete time series using the following thresholds:

- low active power at night (typically below 0.4 kW);
- standard use between 0.4 and 2 kW;
- peak use above 2 kW.

``` r

elec_dts <- cut(elec, breaks = c(0, 0.4, 2, 8), labels = c("low", "typical", "high"))
```

Fitting a VLMC to this sequence gives the following model (using BIC for
model selection):

``` r

elec_vlmc_tune <- tune_vlmc(elec_dts)
best_elec_vlmc <- as_vlmc(elec_vlmc_tune)
draw(best_elec_vlmc)
#> ▪ (0.3269, 0.4841, 0.189)
#> ├─ low (0.8088, 0.1821, 0.009105)
#> ├─ typical (0.1292, 0.8041, 0.06667)
#> │  ├─ low (0.3667, 0.5833, 0.05)
#> │  └─ typical (0.1022, 0.8365, 0.0613)
#> │     └─ low (0.4058, 0.5507, 0.04348)
#> └─ high (0, 0.1864, 0.8136)
```

Considering the apparent regularity of the time series, one may wonder
whether this model really captures the dynamics of electricity
consumption. In particular the longest memory of 3 time steps, i.e., 30
minutes, may seem a bit short compared to the somewhat long periods of
stability.

## Theoretical aspects

VLMC with covariates have been introduced in [Variable length Markov
chain with exogenous covariates](https://doi.org/10.1111/jtsa.12615).
The core idea is to enable the conditional probabilities of the next
state given the context to depend on exogenous covariates.

### Variable memory

A COVLMC models a pair of sequences. The main sequence is denoted
\\\mathbf{X}=X_1, X_2, \ldots, X_n, \ldots\\. The random variables take
values in a finite state space \\S\\ exactly as in a standard VLMC. We
have *in addition* another sequence of random variables
\\\mathbf{Y}=Y_1, Y_2, \ldots, Y_n, \ldots\\ which take values in
\\\mathbb{R}^p\\. This latter assumption \\Y_l\in \mathbb{R}^p\\ can be
relaxed to more general spaces.

A COVLMC puts restrictions on the conditional probabilities of \\X_n\\
given the past of *both* sequences. To simplify the presentation, we
denote \\X^k_m\\ the sequence of random variables \\ X_k^m=(X_k,
X\_{k-1}, \ldots, X_m), \\ and we use similar notations for the values
taken by the variables \\x^k_m\\, as well as for \\Y^k_m\\ and
\\y^k_m\\. The notation accommodates both the temporal order if \\k\<m\\
and the reverse temporal order if \\k\>m\\.

A pair of sequences \\\mathbf{X}\\ and \\\mathbf{Y}\\ is a COVLMC if
there is a maximal order \\l\_{\max}\\ and a function \\l\\ from
\\S^{l\_{\max}}\\ to \\\\0,\ldots,l\_{\max}\\\\ such that for all
\\n\>l\_{\max}\\ \\ \begin{multline} \mathbb{P}(X_n=x_n\mid
X^{n-1}\_1=x^{n-1}\_1, Y^{n-1}\_1=y^{n-1}\_1)=\\
\mathbb{P}\left(X_n=x_n\mid
X^{n-1}\_{n-l\left(x^{n-1}\_{n-l\_{\max}}\right)}=x^{n-1}\_{n-l\left(x^{n-1}\_{n-l\_{\max}}\right)},
Y^{n-1}\_{n-l\left(x^{n-1}\_{n-l\_{\max}}\right)}=y^{n-1}\_{n-l\left(x^{n-1}\_{n-l\_{\max}}\right)}\right).
\end{multline} \\ As in VLMC, the \\\mathbf{X}\\ process has a finite
and variable memory, but this memory applies to both \\\mathbf{X}\\ and
\\\mathbf{Y}\\. Notice that the memory order depends only on
\\\mathbf{X}\\ and that no assumptions are made on the temporal
behaviour of \\\mathbf{Y}\\. Thus a COVLMC on \\\mathbf{X}\\ and
\\\mathbf{Y}\\ is not a VLMC on the pair \\(\mathbf{X}, \mathbf{Y})\\
(which can make sense if \\\mathbf{Y}\\ is discrete).

As for VLMC, the memory length function generates a *context* function
\\c\\ which keeps in the past the part needed to obtain the conditional
distribution: \\c\\ is a function from \\S^{l\_{\max}}\\ to
\\\bigcup\_{k=0}^{l\_{\max}}S^k\\ given by \\
c(x^{n-1}\_{n-l\_{\max}})=x^{n-1}\_{n-l\left(x^{n-1}\_{n-l\_{\max}}\right)}
\\ The image by \\c\\ of \\S^{l\_{\max}}\\ is the set of contexts of the
COVLMC which is entirely specified by \\l\\ and one conditional
distribution per context.

As the notations are somewhat opaque at first, we can illustrate the
definition with a simple example. We consider a binary sequence (values
in \\S=\\0, 1\\\\) and a single numerical covariate
\\Y_t\in\mathbb{R}\\. As in the theoretical example in
[`vignette("variable-length-markov-chains")`](https://fabrice-rossi.github.io/mixvlmc/articles/variable-length-markov-chains.md)
we assume that \\l\_{\max}=3\\ and that \\l\\ is given by \\
\begin{align\*} l(a, b, 0)&=1&\forall a, \forall b,\\ l(c, 1,
1)&=2&\forall c,\\ l(0, 0, 1)&=3,&\\ l(1, 0, 1)&=3.&\\ \end{align\*} \\
In practice the COVLMC is therefore fully described by specifying the
following probabilities: \\ \begin{align\*} \mathbb{P}(X_t=1\mid&
X\_{t-1}=0, Y\_{t-1}=y\_{t-1})\\ \mathbb{P}(X_t=1\mid& X\_{t-1}=1,
X\_{t-2}=1, Y\_{t-1}=y\_{t-1}, Y\_{t-2}=y\_{t-2})\\
\mathbb{P}(X_t=1\mid& X\_{t-1}=1, X\_{t-2}=0, X\_{t-3}=0,
Y\_{t-1}=y\_{t-1}, Y\_{t-2}=y\_{t-2}, Y\_{t-3}=y\_{t-3})\\
\mathbb{P}(X_t=1\mid& X\_{t-1}=1, X\_{t-2}=0, X\_{t-3}=1,
Y\_{t-1}=y\_{t-1}, Y\_{t-2}=y\_{t-2}, Y\_{t-3}=y\_{t-3}) \end{align\*}
\\

### Conditional distributions

The main difficulty induced by COVLMC compared to VLMC is the
specification of the conditional distributions. Indeed the conditional
distributions depend on \\\mathbf{Y}\\ and cannot simply be given by a
table. For instance, in the example above, we need to specify, among
others, \\\mathbb{P}(X_t=1\mid X\_{t-1}=0, Y\_{t-1}=y\_{t-1})\\, for all
values of \\y\_{t-1}\in \mathbb{R}\\ (in \\\mathbb{R}^p\\ in the general
case).

A natural choice in this particular example is to use a logistic model,
i.e. to assume \\ \mathbb{P}(X_t=1\mid X\_{t-1}=0,
Y\_{t-1}=y\_{t-1})=g(\alpha^0+y\_{t-1}\beta_1^0), \\ where
\\g(t)=\frac{1}{1+\exp(-t)}\\ is the logistic function. The superscripts
on \\\alpha^0\\ and \\\beta^0_1\\ refer to the context, here \\0\\,
while the subscript on \\\beta^0_1\\ refers to the time delay (here
\\1\\). By extension, we have for example \\ \begin{multline\*}
\mathbb{P}(X_t=1\mid X\_{t-1}=1, X\_{t-2}=1, Y\_{t-1}=y\_{t-1},
Y\_{t-2}=y\_{t-2})=\\
g\left(\alpha^{1,1}+y\_{t-1}\beta^{1,1}\_1+y\_{t-2}\beta^{1,1}\_2\right).
\end{multline\*} \\

More generally, the probability distribution associated with a context
could be given by any function that maps the values of the covariates to
a distribution on \\S\\, the state space. Following the original
[paper](https://doi.org/10.1111/jtsa.12615), `mixvlmc` uses multinomial
logistic regression as implemented by
[`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html) or
[`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html), or a
logistic regression provided by
[`stats::glm()`](https://rdrr.io/r/stats/glm.html) for state spaces with
only 2 states. This has several advantages over a more general solution:

1.  during the estimation phase, one probability distribution is
    estimated for each relevant context: this could induce a large
    computational burden for more complex models than multinomial
    logistic ones;
2.  having a simple model makes it possible to fit contexts with a
    limited number of occurrences which is important to allow searching
    for long term dependencies;
3.  logistic models with different memory orders are easy to compare
    using a likelihood-ratio test.

The last point is used in `mixvlmc` (as in the original paper) to
simplify local models with respect to the covariates. For a context of
length \\l\\, the probability distribution is assumed to depend on the
\\l\\ past values of \\\mathbf{Y}\\ but in practice we allow a
dependency on only \\k\<l\\ past values. For instance, we could have \\
\mathbb{P}(X_t=1\mid X\_{t-1}=1, X\_{t-2}=1, Y\_{t-1}=y\_{t-1},
Y\_{t-2}=y\_{t-2})=g\left(\alpha^{1,1}+y\_{t-1}\beta^{1,1}\_1\right). \\

### Beta-Context algorithm

Estimating a COVLMC model from two time series is more complex than
estimating a VLMC model. `mixvlmc` implements the Beta-Context algorithm
proposed in the original [paper](https://doi.org/10.1111/jtsa.12615). It
is inspired by the context algorithm used for VLMC (and proposed in
[Variable length Markov
chains](https://dx.doi.org/10.1214/aos/1018031204)). It can be
summarised as follows:

1.  the first step consists in building a context tree (see
    [`vignette("context-trees")`](https://fabrice-rossi.github.io/mixvlmc/articles/context-trees.md))
    from the \\\mathbf{X}\\ discrete sequence, almost exactly as for a
    VLMC: the only difference is that to be kept in the tree a context
    must appear a number of times that depends on its length, on the
    dimension of the covariates and on the number of states. This
    guarantees a minimal number of observations for the (maximum
    likelihood) estimation of the context-dependent multinomial logistic
    regression.
2.  the second step is an estimation one: a multinomial logistic
    regression model is estimated for each context, using a number of
    past values of \\\mathbf{Y}\\ equal to the length of the context.
3.  the rest of the algorithm consists in pruning the context tree and
    the logistic models:
    1.  leaf contexts are first assessed in terms of model
        simplification. The likelihood-ratio test is used to decide
        whether the oldest value of \\\mathbf{Y}\\ is relevant or not.
        Essentially we compare e.g.
        \\g\left(\alpha^{1,1}+y\_{t-1}\beta^{1,1}\_1\right)\\ to
        \\g\left(\alpha^{1,1}+y\_{t-1}\beta^{1,1}\_1+y\_{t-2}\beta^{1,1}\_2\right)\\
        as an estimator of \\\mathbb{P}(X_t=1\mid X\_{t-1}=1,
        X\_{t-2}=1, Y\_{t-1}=y\_{t-1}, Y\_{t-2}=y\_{t-2})\\;
    2.  based on the results of those tests, it may be possible to
        completely remove a context from the tree, see the
        [paper](https://doi.org/10.1111/jtsa.12615) for details.

This last pruning phase is carried out repeatedly as context removals
turn internal contexts into leaves that can then be further simplified.

## COVLMC in practice

### Estimation

COVLMC estimation is provided by the
[`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc.md)
function. We build first a simple example data set as follows:

``` r

set.seed(0)
nb_obs <- 200
covariates <- data.frame(y = runif(nb_obs))
x <- 0
for (k in 2:nb_obs) {
  ## we induce a simple dependency to the covariate
  ## and an order 1 memory
  if (covariates$y[k - 1] < 0.5) {
    if (x[k - 1] == 0) {
      x[k] <- sample(c(0, 1), 1, prob = c(0.7, 0.3))
    } else {
      x[k] <- sample(c(0, 1), 1, prob = c(0.3, 0.7))
    }
  } else {
    if (x[k - 1] == 0) {
      x[k] <- sample(c(0, 1), 1, prob = c(0.1, 0.9))
    } else {
      x[k] <- sample(c(0, 1), 1, prob = c(0.5, 0.5))
    }
  }
}
```

Then we estimate a COVLMC as follows:

``` r

model <- covlmc(x, covariates)
model
#> VLMC with covariates context tree on 0, 1 
#>  cutoff in quantile scale: 0.05
#>  Number of contexts: 2 
#>  Maximum context length: 1
```

The estimation process is controlled by three parameters:

- `max_depth`: the largest order/memory considered for the COVLMC
  (defaults to 100). This parameter is essentially a computational
  burden control parameter. It does not play a major role in COVLMC
  estimation because of the constraints imposed by `min_size`. The
  default value is very conservative;
- `min_size`: this parameter controls the minimal number of occurrences
  needed for a context to be included in the initial context tree. It
  gives the number of occurrences *per parameter* of the logistic model,
  which depends on the length of the context, the dimension of the
  covariates and the number of states;
- `alpha`: the parameter of the pruning process of the Beta-Context
  algorithm. Pruning decisions are all based on likelihood ratio tests
  and `alpha` is the common level of all those tests.

The default parameters work well on the previous example, as shown by
the obtained model:

``` r

draw(model, model = "full")
#> ▪
#> ├─ 0 ([ (I)    • y_1  
#> │       -2.951 • 6.236 ])
#> └─ 1 ([ (I)   • y_1   
#>         1.719 • -1.881 ])
```

The model has 2 contexts, 0 and 1, as expected. The logistic models are
described by their parameters. For context 0, the intercept is negative
and the coefficient of \\y\_{t-1}\\ is positive: the probability of
switching to 1 is small when \\y\_{t-1}\\ is small and increases with
\\y\_{t-1}\\. For context 1, the situation is reversed (the probability
of staying in 1 decreases with \\y\_{t-1}\\) and the effect of
\\y\_{t-1}\\ is smaller. This is consistent with the way the series were
constructed.

Notice however that obtaining an interesting model with the default
parameters should not be seen as a general property and proper model
choice must be implemented.

### Model choice

As COVLMC estimation fits a potentially large number of logistic models
to the data, the use of a penalised likelihood approach is recommended
to set its parameters and avoid overfitting.

However, model choice is more complex in the case of the COVLMC than for
VLMC. In particular, the pair
[`cutoff()`](https://fabrice-rossi.github.io/mixvlmc/reference/cutoff.md)/[`prune()`](https://fabrice-rossi.github.io/mixvlmc/reference/prune.md)
does not work as well for COVLMC as for VLMC (see
[`vignette("variable-length-markov-chains")`](https://fabrice-rossi.github.io/mixvlmc/articles/variable-length-markov-chains.md)).
Indeed the pruning process of the Beta-Context algorithm is such that
its effects cannot be predicted as easily as the ones of the Context
algorithm. In practice, computing the largest `alpha` (test level) that
is guaranteed to make a minimal but actual pruning of a given COVLMC is
easy, but subsequent cut off values could be misleading. To avoid any
problem it is therefore recommended to rely on the
[`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_covlmc.md)
function that has been designed to explore the full “pruning space”
associated with a given data set.

Used on the artificial example above, it gives:

``` r

model_tune <- tune_covlmc(x, covariates)
model_tune
#> VLMC with covariates context tree on 0, 1 
#>  cutoff in quantile scale: 0.1236
#>  Number of contexts: 2 
#>  Maximum context length: 1 
#>  Selected by BIC (245.1046) with likelihood function "truncated" (-111.9859)
```

and

``` r

draw(as_covlmc(model_tune), model = "full")
#> ▪
#> ├─ 0 ([ (I)    • y_1  
#> │       -2.951 • 6.236 ])
#> └─ 1 ([ (I)   • y_1   
#>         1.719 • -1.881 ])
```

The resulting model is the same as the one obtained before but it was
properly obtained by minimising the BIC.

As for
[`tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_vlmc.md),
the `max_depth` parameter is automatically increased to avoid using it
inadvertently as a regularisation parameter. However, while `min_size`
is generally considered as not having a major influence on the model
selection for VLMC, this is not the case for COVLMC. There is currently
no support in `mixvlmc` for an automatic choice of `min_size` and one
should therefore test several values and compare the obtained BIC/AIC.
In the artificial case, we can try for instance `min_size=2` as follows:

``` r

model_tune_2 <- tune_covlmc(x, covariates, min_size = 2)
model_tune_2
#> VLMC with covariates context tree on 0, 1 
#>  cutoff in quantile scale: 0.01195
#>  Number of contexts: 2 
#>  Maximum context length: 1 
#>  Selected by BIC (242.3184) with likelihood function "truncated" (-110.6132)
```

and `min_size=10` as follows:

``` r

model_tune_10 <- tune_covlmc(x, covariates, min_size = 10)
model_tune_10
#> VLMC with covariates context tree on 0, 1 
#>  cutoff in quantile scale: 0.1236
#>  Number of contexts: 2 
#>  Maximum context length: 1 
#>  Selected by BIC (245.6423) with likelihood function "truncated" (-112.2446)
```

Here we see no particular effect of the parameter on the selected model
because of the simplicity of the problem.

Let us now come back to the electricity consumption example described
above. We introduce a very basic day/night covariate as follows:

``` r

elec_cov <- data.frame(day = (pc_week_15_16$hour >= 7 & pc_week_15_16$hour <= 18))
```

and fit a COVLMC with

``` r

elec_tune <- tune_covlmc(elec_dts, elec_cov)
elec_tune
#> VLMC with covariates context tree on low, typical, high 
#>  cutoff in quantile scale: 0.007218
#>  Number of contexts: 7 
#>  Maximum context length: 3 
#>  Selected by BIC (2251.198) with likelihood function "truncated" (-1064.74)
```

The model selection process can be represented graphically as follows:

``` r

ggplot(elec_tune$results, aes(x = alpha, y = BIC)) +
  geom_line() +
  geom_point()
```

![Line plot with points of the BIC against the cut off alpha on the
quantile scale for the models considered during the COVLMC model
selection. The BIC reaches its minimum at a small value of alpha and
increases for larger
values.](covlmc_files/figure-html/unnamed-chunk-13-1.png) or
automatically using
e.g. [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
as follows:

``` r

print(autoplot(elec_tune))
```

![Six panels plotting the AIC, the BIC, the number of contexts, the
covariate depth, the depth and the log likelihood against the cut off on
the quantile scale for the COVLMC model selection. The BIC is minimal at
a small cut off and the AIC at a larger one, while the number of
contexts and the depths quickly reach their maximal
values.](covlmc_files/figure-html/unnamed-chunk-14-1.png)

The final model is:

``` r

draw(as_covlmc(elec_tune), model = "full", with_state = TRUE)
#> ▪
#> ├─ low ([ (low)   ~ (I)   
#> │         typical ~ -1.491
#> │         high    ~ -4.487 ])
#> ├─ typical
#> │  ├─ low ([ (low)   ~ (I)   
#> │  │         typical ~ 0.4643
#> │  │         high    ~ -1.992 ])
#> │  ├─ typical
#> │  │  ├─ low ([ (low)   ~ (I)   
#> │  │  │         typical ~ 0.3054
#> │  │  │         high    ~ -2.234 ])
#> │  │  ├─ typical ([ (low)   ~ (I)     • day_1TRUE
#> │  │  │             typical ~ 1.839   • 0.975    
#> │  │  │             high    ~ -0.1823 • -0.1462   ])
#> │  │  └─ high ([ (low)   ~ (I)   
#> │  │             typical ~ 2.773 
#> │  │             high    ~ 0.8473 ])
#> │  └─ high ([ (low)   ~ (I)  
#> │             typical ~ 3.367
#> │             high    ~ 1.705 ])
#> └─ high ([ (typical) ~ (I)  
#>            high      ~ 1.474 ])
```

It shows some interesting patterns:

- one of the logistic models is degenerate: in the `high` context, no
  transition to the `low` state can happen. This was already observed
  with the VLMC;
- the only dependency with respect to the covariate is in the context
  typical, typical, typical. In this case, the probability of staying in
  the typical state is higher during the day (about 91%) than at night
  (about 77%). In practice, this means that the model is able to
  generate longer sequences that stay in the typical state during the
  day than at night.

Notice finally that for this real world example, the `min_size`
parameter has some influence on the results. Setting it to a smaller
value does not change the final model, as shown below (the reported BIC
and log likelihood differ slightly because the truncated likelihood used
for model selection discards a number of initial observations that
depends on the initial model, see
[`vignette("likelihood")`](https://fabrice-rossi.github.io/mixvlmc/articles/likelihood.md)):

``` r

elec_tune_3 <- tune_covlmc(elec_dts, elec_cov, min_size = 3)
elec_tune_3
#> VLMC with covariates context tree on low, typical, high 
#>  cutoff in quantile scale: 0.007218
#>  Number of contexts: 7 
#>  Maximum context length: 3 
#>  Selected by BIC (2247) with likelihood function "truncated" (-1062.645)
```

However, increasing the parameter to, e.g., 10 generates a simpler but
weaker model (with a higher BIC) as shown here:

``` r

elec_tune_10 <- tune_covlmc(elec_dts, elec_cov, min_size = 10)
elec_tune_10
#> VLMC with covariates context tree on low, typical, high 
#>  cutoff in quantile scale: 0.03454
#>  Number of contexts: 5 
#>  Maximum context length: 2 
#>  Selected by BIC (2268.113) with likelihood function "truncated" (-1088.409)
```

### Diagnostics

The package provides numerous ways to analyse a COVLMC. Basic functions
include

- [`states()`](https://fabrice-rossi.github.io/mixvlmc/reference/states.md)
  returns the state space of the model;
- [`depth()`](https://fabrice-rossi.github.io/mixvlmc/reference/depth.md)
  returns the length of the longest context in the model;
- [`covariate_depth()`](https://fabrice-rossi.github.io/mixvlmc/reference/covariate_depth.md)
  returns the longest memory used by the model with respect to
  covariates;
- [`context_number()`](https://fabrice-rossi.github.io/mixvlmc/reference/context_number.md)
  returns the number of contexts in the model.

For instance, the model selected above has the following
characteristics:

``` r

elec_model <- as_covlmc(elec_tune)
states(elec_model)
#> [1] low     typical high   
#> Levels: low typical high
depth(elec_model)
#> [1] 3
covariate_depth(elec_model)
#> [1] 1
context_number(elec_model)
#> [1] 7
```

COVLMC objects support classical statistical functions such as:

``` r

logLik(elec_model)
#> 'log Lik.' -1064.74 (df=16)
AIC(elec_model)
#> [1] 2161.48
BIC(elec_model)
#> [1] 2251.198
```

### Contexts

The model can be explored in detail by drawing its context tree (see
[`vignette("context-trees")`](https://fabrice-rossi.github.io/mixvlmc/articles/context-trees.md)
for details) as follows:

``` r

draw(elec_model, p_value = TRUE)
#> ▪ (merging (low and high): 8.121e-230)
#> ├─ low (‹0.03454› 0.8088, 0.1821, 0.009105)
#> ├─ typical (merging (low and high): 8.264e-09)
#> │  ├─ low (‹0.8447› 0.3667, 0.5833, 0.05)
#> │  ├─ typical (collapsing: 3.961e-10)
#> │  │  ├─ low (‹0.2323› 0.4058, 0.5507, 0.04348)
#> │  │  ├─ typical (‹6.269e-05› [ 1.839   • 0.975  
#> │  │  │                         -0.1823 • -0.1462 ])
#> │  │  └─ high (‹0.6393› 0.05172, 0.8276, 0.1207)
#> │  └─ high (‹0.1638› 0.02817, 0.8169, 0.1549)
#> └─ high (‹0.776› 0, 0.1864, 0.8136)
```

The
[`draw.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.covlmc.md)
function is more advanced than its VLMC counterpart. It provides more
detailed information, particularly regarding p-values associated with
pruning operations (shown when `p_value = TRUE`). The “merging” p-value
corresponds to replacing some of the contexts with a single joint model,
while the “collapsing” p-value is associated with pruning all the
sub-contexts of a context. P-values associated with specific models
(shown between angle brackets) correspond to reducing the memory of the
corresponding model, that is discarding the dependency of the
conditional probability on the oldest covariates.

To explore the contexts in a programmatic way, one should rely on the
[`contexts()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.md)
function. COVLMC contexts have additional characteristics compared to
VLMC and context trees. In particular, the
[`contexts()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.md)
function can report the model associated with each context, either by
its parameters only:

``` r

contexts(elec_model, model = "coef")
#>        context         coef
#> 1          low -1.49102....
#> 2 low, typical 0.464305....
#> 3 low, typ.... 0.305381....
#> 4 typical,.... 1.839226....
#> 5 high, ty.... 2.772588....
#> 6 high, ty.... 3.367295....
#> 7         high 1.473892....
```

or using the models themselves:

``` r

contexts(elec_model, model = "full")
#>        context                                            model
#> 1          low <S4 class 'vglm' [package "VGAM"] with 37 slots>
#> 2 low, typical <S4 class 'vglm' [package "VGAM"] with 37 slots>
#> 3 low, typ.... <S4 class 'vglm' [package "VGAM"] with 37 slots>
#> 4 typical,.... <S4 class 'vglm' [package "VGAM"] with 37 slots>
#> 5 high, ty.... <S4 class 'vglm' [package "VGAM"] with 37 slots>
#> 6 high, ty.... <S4 class 'vglm' [package "VGAM"] with 37 slots>
#> 7         high <S4 class 'vglm' [package "VGAM"] with 37 slots>
```

As for context trees, focused analysis of specific contexts can be done
by requesting a `ctx_node_covlmc` representation of the context of
interest, for instance via the
[`find_sequence.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/find_sequence.covlmc.md)
function, or with the default result type of
[`contexts()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.md)

``` r

ctxs <- contexts(elec_model)
ctxs
#> Contexts:
#>  low
#>  low, typical
#>  low, typical, typical
#>  typical, typical, typical
#>  high, typical, typical
#>  high, typical
#>  high
```

Individual contexts can be analysed using a collection of dedicated
functions, for instance

``` r

model(ctxs[[2]])
#>            [,1]
#> [1,]  0.4643056
#> [2,] -1.9924301
model(ctxs[[3]], type = "full")
#> 
#> Call:
#> VGAM::vglm(formula = target ~ 1, family = VGAM::multinomial(refLevel = 1), 
#>     data = mm, model = FALSE, x.arg = FALSE, y.arg = FALSE, maxit = getOption("mixvlmc.maxit"))
#> 
#> 
#> Coefficients:
#> (Intercept):1 (Intercept):2 
#>     0.3053816    -2.2335922 
#> 
#> Degrees of Freedom: 138 Total; 136 Residual
#> Residual deviance: 114.655 
#> Log-likelihood: -57.32751 
#> 
#> This is a multinomial logit model with 3 levels
```

See
[`contexts.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.covlmc.md)
for details.
