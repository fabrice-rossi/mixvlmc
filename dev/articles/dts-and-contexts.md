# Discrete time series and contexts of models

``` r

library(mixvlmc)
```

This vignette covers two complementary aspects of `mixvlmc`: the
representation of the discrete time series used to estimate models, and
the detailed exploration of the contexts of fitted models. It assumes
some familiarity with the notions introduced in
[`vignette("context-trees")`](https://fabrice-rossi.github.io/mixvlmc/dev/articles/context-trees.md)
(contexts, context trees,
[`contexts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.md)
with `frequency`, and the basic node functions
[`find_sequence()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/find_sequence.md),
[`is_context()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/is_context.md),
[`children()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/children.md),
[`counts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/counts.md)
and
[`positions()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/positions.md)),
in
[`vignette("variable-length-markov-chains")`](https://fabrice-rossi.github.io/mixvlmc/dev/articles/variable-length-markov-chains.md)
and in
[`vignette("covlmc")`](https://fabrice-rossi.github.io/mixvlmc/dev/articles/covlmc.md).

## Discrete time series

### Creating a discrete time series

Since version 0.3.0, `mixvlmc` represents discrete time series by
objects of class `dts`, created by the
[`dts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/dts.md)
function. A `dts` stores the time series together with its *state
space*, i.e. the set of values that the series can take. A `dts` can be
built from a numeric, character, factor or logical vector. When the
state space is not specified, it is computed from the vector in a way
that depends on its type. For numeric and character vectors, the state
space consists of the unique values of the series, in increasing order:

``` r

x_num <- dts(c(3, 1, 2, 2, 1, 3))
x_num
#> Discrete time series of length 6 with state space 1, 2, 3
#>  3 1 2 2 1 ...
states(x_num)
#> [1] 1 2 3
x_chr <- dts(c("b", "a", "c", "a", "b"))
states(x_chr)
#> [1] "a" "b" "c"
```

For a factor, the state space is given by the levels of the factor,
including levels that do not appear in the series, and in the order of
the levels:

``` r

x_fct <- dts(factor(c("low", "high", "low"),
  levels = c("low", "medium", "high")
))
x_fct
#> Discrete time series of length 3 with state space low, medium, high
#>  low high low
```

For a logical vector, the state space is always `c(FALSE, TRUE)`:

``` r

x_lgl <- dts(c(TRUE, TRUE, TRUE))
states(x_lgl)
#> [1] FALSE  TRUE
```

By default, only the first 5 values of the series are printed, but this
can be changed with the `n` parameter of
[`print()`](https://rdrr.io/r/base/print.html):

``` r

set.seed(0)
x_long <- dts(sample(c("A", "B", "C"), 50, replace = TRUE))
x_long
#> Discrete time series of length 50 with state space A, B, C
#>  B A C A B ...
print(x_long, n = 15)
#> Discrete time series of length 50 with state space A, B, C
#>  B A C A B A C C B B C C A A A ...
```

[`is_dts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/is_dts.md)
tests whether an object is a discrete time series,
[`states()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/states.md)
returns its state space and
[`dts_data()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/dts_data.md)
recovers the original series as a vector (of the same type as the state
space):

``` r

is_dts(x_chr)
#> [1] TRUE
is_dts(c("b", "a", "c"))
#> [1] FALSE
dts_data(x_chr)
#> [1] "b" "a" "c" "a" "b"
dts_data(x_fct)
#> [1] low  high low 
#> Levels: low medium high
```

The length of a `dts` is given by
[`length()`](https://rdrr.io/r/base/length.html) and a `dts` can be
subsetted with `[`. The result is a `dts` with the same state space,
even if some states do not appear in the subset:

``` r

length(x_long)
#> [1] 50
x_long[1:2]
#> Discrete time series of length 2 with state space A, B, C
#>  B A
```

Discrete time series cannot contain missing values:

``` r

try(dts(c("a", NA, "b")))
#> Error in validate_dts(x, vals) : 
#>   discrete time series cannot contain missing values
```

### Specifying the state space

The state space can be given explicitly with the `vals` parameter of
[`dts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/dts.md).
This has two uses. First, it sets the order of the states, which is the
order used by all the functions of the package (for instance to order
the columns of the results of
[`contexts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.md)
or [`predict()`](https://rdrr.io/r/stats/predict.html), or the
probabilities displayed by
[`draw()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/draw.md)).
Second, the state space can include states that do not appear in the
series:

``` r

x_abc <- dts(c("b", "a", "b"), vals = c("c", "b", "a"))
x_abc
#> Discrete time series of length 3 with state space c, b, a
#>  b a b
```

When `vals` is given,
[`dts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/dts.md)
checks that the series only contains values of the state space:

``` r

try(dts(c("a", "d"), vals = c("a", "b", "c")))
#> Error : x contains unknown states
```

### Why use a discrete time series object?

All the estimation functions of the package
([`ctx_tree()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/ctx_tree.md),
[`vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/vlmc.md),
[`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/covlmc.md),
[`tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/tune_vlmc.md)
and
[`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/tune_covlmc.md))
accept either a plain vector or a `dts`. In the former case, the vector
is converted with
[`dts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/dts.md)
using the default state space. Using a `dts` directly is useful mainly
when the state space of the model cannot be deduced reliably from the
series used to estimate it, a common situation when several series are
analysed with the same state space.

Let us consider the household electricity consumption data set
`powerconsumption`. As in
[`vignette("covlmc")`](https://fabrice-rossi.github.io/mixvlmc/dev/articles/covlmc.md),
the active power is discretised into three levels. The following
function extracts the discretised series of some weeks of the data set
(as a character vector):

``` r

power_levels <- c("low", "typical", "high")
power_weeks <- function(weeks) {
  pc <- powerconsumption[powerconsumption$week %in% weeks, ]
  as.character(cut(pc$active_power,
    breaks = c(0, 0.4, 2, 8),
    labels = power_levels
  ))
}
week_33 <- power_weeks(33)
week_35 <- power_weeks(35)
table(week_33)
#> week_33
#>     low typical 
#>     925      83
table(week_35)
#> week_35
#>    high     low typical 
#>      28     891      89
```

The consumption is very low during week 33, in August (probably because
of holidays), and the `high` level is never observed. A model estimated
on this week from the character vector has therefore only two states and
it cannot be used on week 35:

``` r

model_33_raw <- vlmc(week_33)
states(model_33_raw)
#> [1] "low"     "typical"
try(loglikelihood(model_33_raw, newdata = week_35))
#> Error : x contains unknown states
```

If we specify the state space with a `dts`, the model knows that the
`high` level exists:

``` r

week_33_dts <- dts(week_33, vals = power_levels)
model_33 <- vlmc(week_33_dts)
model_33
#> VLMC context tree on low, typical, high [C++]
#>  cutoff: 2.996 (quantile: 0.05)
#>  Number of contexts: 42 
#>  Maximum context length: 24
loglikelihood(model_33, newdata = week_35)
#> 'log Lik.' -Inf (df= 84, nb obs.= 984, initial="truncated")
```

The log likelihood is now computed, and its value, \\-\infty\\, tells us
that week 35 is impossible according to the model of week 33, as the
`high` level was never observed during that week. The predicted
probabilities include all the states of the state space, in the
specified order (the last row is the prediction of the state that
follows the series, see
[`predict.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/predict.vlmc.md)):

``` r

predict(model_33, newdata = week_35[1:5], type = "probs")
#>            low    typical high
#> [1,] 0.9176587 0.08234127    0
#> [2,] 0.9155844 0.08441558    0
#> [3,] 0.9088757 0.09112426    0
#> [4,] 0.9009126 0.09908735    0
#> [5,] 0.8942029 0.10579710    0
#> [6,] 0.8847403 0.11525974    0
```

Notice that the new data (`newdata` in
[`loglikelihood()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/loglikelihood.md)
and [`predict()`](https://rdrr.io/r/stats/predict.html), `init` in
[`simulate()`](https://rdrr.io/r/stats/simulate.html) or the sequence
given to
[`find_sequence()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/find_sequence.md))
can be given as a plain vector or as a `dts`. In both cases, it is
interpreted in the state space of the model: a `dts` with a different
state space is accepted provided its values belong to the state space of
the model. For instance, the following call computes the log likelihood
of week 34, in which the `high` level is also absent, with the model of
week 33:

``` r

week_34_dts <- dts(power_weeks(34))
states(week_34_dts)
#> [1] "low"     "typical"
loglikelihood(model_33, newdata = week_34_dts)
#> 'log Lik.' -201.5814 (df= 84, nb obs.= 984, initial="truncated")
```

When the series is a factor, its levels define the state space, so
discretising directly with [`cut()`](https://rdrr.io/r/base/cut.html)
(without the conversion to character used above) also gives a model with
three states:

``` r

states(vlmc(factor(week_33, levels = power_levels)))
#> [1] low     typical high   
#> Levels: low typical high
```

Finally, [`simulate()`](https://rdrr.io/r/stats/simulate.html) does not
return a `dts` but a vector of the same type as the state space of the
model (here a character vector), whose values are taken from this state
space. It can be converted into a `dts` with the same state space as
follows:

``` r

sim_33 <- simulate(model_33, 20, seed = 0)
sim_33
#>  [1] "low"     "low"     "low"     "low"     "typical" "low"     "low"    
#>  [8] "low"     "low"     "low"     "low"     "low"     "low"     "low"    
#> [15] "low"     "low"     "low"     "low"     "typical" "low"
dts(sim_33, vals = states(model_33))
#> Discrete time series of length 20 with state space low, typical, high
#>  low low low low typical ...
```

## Contexts of a VLMC

In the rest of this vignette, we use a VLMC estimated on week 5 of the
electricity consumption data set. We keep the positions of the contexts
in the time series with `keep_match = TRUE` (they are not kept by
default). The model is estimated with the default C++ backend (see
[`vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/vlmc.md)),
but all the functions described below work in the same way with the R
backend (`backend = "R"`).

``` r

week_5 <- dts(power_weeks(5), vals = power_levels)
model_5 <- vlmc(week_5, keep_match = TRUE)
model_5
#> VLMC context tree on low, typical, high [C++]
#>  cutoff: 2.996 (quantile: 0.05)
#>  Number of contexts: 26 
#>  Maximum context length: 9
draw(model_5)
#> ▪ (0.1667, 0.5496, 0.2837)
#> ├─ low (0.7665, 0.2335, 0)
#> │  └─ low (0.7638, 0.2362, 0)
#> │     └─ low (0.7917, 0.2083, 0)
#> │        └─ low (0.8133, 0.1867, 0)
#> │           ├─ low (0.9167, 0.08333, 0)
#> │           │  └─ low (0.9091, 0.09091, 0)
#> │           │     └─ low (0.9, 0.1, 0)
#> │           │        └─ low (0.8889, 0.1111, 0)
#> │           │           └─ typical (0.4, 0.6, 0)
#> │           └─ typical (0.4, 0.6, 0)
#> ├─ typical (0.0704, 0.8466, 0.08303)
#> │  ├─ low (0.3846, 0.5385, 0.07692)
#> │  ├─ typical (0.04691, 0.8849, 0.06823)
#> │  │  └─ typical (0.04337, 0.8892, 0.06747)
#> │  │     └─ low (0.2941, 0.6471, 0.05882)
#> │  └─ high (0.04444, 0.7111, 0.2444)
#> └─ high (0.003497, 0.1573, 0.8392)
#>    ├─ typical (0.02174, 0.2826, 0.6957)
#>    │  └─ typical (0.03125, 0.3125, 0.6562)
#>    │     └─ high (0.3333, 0.6667, 0)
#>    └─ high (0, 0.1333, 0.8667)
#>       └─ high (0, 0.1106, 0.8894)
#>          └─ high (0, 0.0973, 0.9027)
#>             └─ high (0, 0.1018, 0.8982)
#>                └─ high (0, 0.1, 0.9)
#>                   └─ high (0, 0.1037, 0.8963)
#>                      └─ typical (0, 0.3571, 0.6429)
```

### Two representations

[`contexts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.md)
gives two representations of the contexts of a model. By default, it
returns a list of `ctx_node` objects, one per context, which can be
explored with the node functions described in the next section:

``` r

ctxs_5 <- contexts(model_5)
ctxs_5
#> Contexts:
#>  typical, high, high, high, high, high, high, high
#>  high, high, high, high, high, high, high
#>  high, high, high, high, high, high
#>  high, high, high, high, high
#>  high, high, high, high
#>  high, high, high
#>  high, high
#>  high, typical, typical, high
#>  typical, typical, high
#>  typical, high
#>  high
#>  low, typical
#>  low, typical, typical, typical
#>  typical, typical, typical
#>  typical, typical
#>  high, typical
#>  typical, low, low, low, low
#>  typical, low, low, low, low, low, low, low, low
#>  low, low, low, low, low, low, low, low
#>  low, low, low, low, low, low, low
#>  low, low, low, low, low, low
#>  low, low, low, low, low
#>  low, low, low, low
#>  low, low, low
#>  low, low
#>  low
```

With `sequence = TRUE`, or as soon as additional information is
requested (see below), the function returns a data frame with one row
per context. Its first column, `context`, is a list: each element is a
vector of states (of the same type as `states(model_5)`) that represents
a context. As the default printing of such a column is truncated, we
define a small helper function that replaces each context by a character
string, only for display purposes:

``` r

flat_contexts <- function(ctx_df) {
  ctx_df$context <- vapply(ctx_df$context, paste, character(1), collapse = ", ")
  ctx_df
}
ctx_df_5 <- contexts(model_5, sequence = TRUE)
ctx_df_5$context[[1]]
#> [1] "typical" "high"    "high"    "high"    "high"    "high"    "high"   
#> [8] "high"
```

Contexts are written by default in temporal order (the most recent state
is on the right), while `reverse = TRUE` uses the reverse order of
Bühlmann and Wyner (the most recent state is on the left), which
corresponds to the paths from the root in the representation of
[`draw()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/draw.md):

``` r

head(flat_contexts(contexts(model_5, sequence = TRUE, reverse = TRUE)))
#>                                             context
#> 1 high, high, high, high, high, high, high, typical
#> 2          high, high, high, high, high, high, high
#> 3                high, high, high, high, high, high
#> 4                      high, high, high, high, high
#> 5                            high, high, high, high
#> 6                                  high, high, high
```

The parameter `reverse` has the same effect on the list representation
and can also be given to the
[`print()`](https://rdrr.io/r/base/print.html) function of a list of
contexts (`print(ctxs_5, reverse = TRUE)`).

The additional columns depend on the parameters of
[`contexts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.md)
and on the class of the model, as summarised below
([`contexts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.md)
accepts the same parameters for both backends of
[`ctx_tree()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/ctx_tree.md)
and
[`vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/vlmc.md),
but the order of the contexts may differ from one backend to the other).

| parameter   | `ctx_tree` | `vlmc` | `covlmc` | columns                               |
|-------------|:----------:|:------:|:--------:|---------------------------------------|
| `frequency` |    yes     |  yes   |   yes    | `freq`, one per state if `"detailed"` |
| `positions` |    yes     |  yes   |   yes    | `positions`                           |
| `local`     |     no     |  yes   |   yes    | (modifies the counts)                 |
| `cutoff`    |     no     |  yes   |    no    | `cutoff`                              |
| `metrics`   |     no     |  yes   |   yes    | `accuracy`, `auc`                     |
| `model`     |     no     |   no   |   yes    | `coef` or `model`                     |
| `hsize`     |     no     |   no   |   yes    | `hsize`                               |
| `merging`   |     no     |   no   |   yes    | `merged`                              |

### Counts

As for context trees, `frequency = "total"` adds a `freq` column with
the number of occurrences of each context in the time series (not
including a possible final occurrence not followed by any state), while
`frequency = "detailed"` adds in addition one column per state with the
number of times the context is followed by this state:

``` r

flat_contexts(contexts(model_5, frequency = "detailed"))
#>                                              context freq low typical high
#> 1  typical, high, high, high, high, high, high, high   14   0       5    9
#> 2           high, high, high, high, high, high, high  135   0      14  121
#> 3                 high, high, high, high, high, high  150   0      15  135
#> 4                       high, high, high, high, high  167   0      17  150
#> 5                             high, high, high, high  185   0      18  167
#> 6                                   high, high, high  208   0      23  185
#> 7                                         high, high  240   0      32  208
#> 8                       high, typical, typical, high    3   1       2    0
#> 9                             typical, typical, high   32   1      10   21
#> 10                                     typical, high   46   1      13   32
#> 11                                              high  286   1      45  240
#> 12                                      low, typical   39  15      21    3
#> 13                    low, typical, typical, typical   17   5      11    1
#> 14                         typical, typical, typical  415  18     369   28
#> 15                                  typical, typical  469  22     415   32
#> 16                                     high, typical   45   2      32   11
#> 17                       typical, low, low, low, low   15   6       9    0
#> 18   typical, low, low, low, low, low, low, low, low    5   2       3    0
#> 19            low, low, low, low, low, low, low, low   45  40       5    0
#> 20                 low, low, low, low, low, low, low   50  45       5    0
#> 21                      low, low, low, low, low, low   55  50       5    0
#> 22                           low, low, low, low, low   60  55       5    0
#> 23                                low, low, low, low   75  61      14    0
#> 24                                     low, low, low   96  76      20    0
#> 25                                          low, low  127  97      30    0
#> 26                                               low  167 128      39    0
```

These counts are the ones used to estimate the transition probabilities
of the model shown by
[`draw()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/draw.md):
for instance, the probabilities associated with the context `low` are
obtained by dividing the counts of the `low` row by its `freq` value.

In a VLMC, a context can be the suffix of longer contexts. For instance,
`high, high` is the last part of `high, high, high`. The counts above
include all the occurrences of `high, high`, including the ones that are
the last part of an occurrence of `high, high, high`. With
`local = TRUE`, the counts include only the occurrences of the context
that are not the last part of a longer context, that is the time steps
at which the context is the one actually used by the model to predict
the next state:

``` r

ctx_local_5 <- contexts(model_5, frequency = "detailed", local = TRUE)
flat_contexts(ctx_local_5)
#>                                              context freq low typical high
#> 1  typical, high, high, high, high, high, high, high   14   0       5    9
#> 2           high, high, high, high, high, high, high  121   0       9  112
#> 3                 high, high, high, high, high, high   15   0       1   14
#> 4                       high, high, high, high, high   17   0       2   15
#> 5                             high, high, high, high   18   0       1   17
#> 6                                   high, high, high   23   0       5   18
#> 7                                         high, high   32   0       9   23
#> 8                       high, typical, typical, high    3   1       2    0
#> 9                             typical, typical, high   29   0       8   21
#> 10                                     typical, high   14   0       3   11
#> 11                                              high    0   0       0    0
#> 12                                      low, typical   39  15      21    3
#> 13                    low, typical, typical, typical   17   5      11    1
#> 14                         typical, typical, typical  398  13     358   27
#> 15                                  typical, typical   54   4      46    4
#> 16                                     high, typical   45   2      32   11
#> 17                       typical, low, low, low, low   15   6       9    0
#> 18   typical, low, low, low, low, low, low, low, low    5   2       3    0
#> 19            low, low, low, low, low, low, low, low   40  38       2    0
#> 20                 low, low, low, low, low, low, low    5   5       0    0
#> 21                      low, low, low, low, low, low    5   5       0    0
#> 22                           low, low, low, low, low    5   5       0    0
#> 23                                low, low, low, low    0   0       0    0
#> 24                                     low, low, low   21  15       6    0
#> 25                                          low, low   31  21      10    0
#> 26                                               low   40  31       9    0
```

Each time step is associated with at most one context, so that the sum
of the local counts, 1006, is smaller than the length of the time
series, 1008 (the first time steps do not have a long enough past and
the last state is not followed by any state). Some contexts have zero
local counts. For instance, the context `high` exists only because the
model must define a next state distribution after the sequence
`low, high`, which never appears in the time series: the context `high`
is used only for this unobserved sequence, while all the actual
occurrences of `high` are covered by longer contexts.

### Cut off values

For a VLMC, `cutoff` adds a `cutoff` column that summarises, for each
context, the likelihood ratio test that compares the next state
distribution of the context with the one of its parent, the context
obtained by removing its oldest state. With `cutoff = "native"` the
value is the log likelihood ratio, while `cutoff = "quantile"` gives it
in the quantile scale (the p-value of the test). See
[`cutoff()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.md),
[`prune()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/prune.md),
[`vignette("variable-length-markov-chains")`](https://fabrice-rossi.github.io/mixvlmc/dev/articles/variable-length-markov-chains.md)
and
[`vignette("model-selection")`](https://fabrice-rossi.github.io/mixvlmc/dev/articles/model-selection.md)
for details on these two scales.

``` r

flat_contexts(contexts(model_5, cutoff = "quantile"))
#>                                              context       cutoff
#> 1  typical, high, high, high, high, high, high, high 4.109563e-02
#> 2           high, high, high, high, high, high, high 9.898744e-01
#> 3                 high, high, high, high, high, high 9.973425e-01
#> 4                       high, high, high, high, high 9.811938e-01
#> 5                             high, high, high, high 8.419314e-01
#> 6                                   high, high, high 6.121980e-01
#> 7                                         high, high 2.441270e-01
#> 8                       high, typical, typical, high 2.059937e-02
#> 9                             typical, typical, high 8.660173e-01
#> 10                                     typical, high 3.209832e-02
#> 11                                              high 1.249709e-87
#> 12                                      low, typical 1.459397e-07
#> 13                    low, typical, typical, typical 2.639095e-03
#> 14                         typical, typical, typical 9.391320e-01
#> 15                                  typical, typical 4.308507e-02
#> 16                                     high, typical 4.617865e-03
#> 17                       typical, low, low, low, low 1.929706e-03
#> 18   typical, low, low, low, low, low, low, low, low 3.136127e-02
#> 19            low, low, low, low, low, low, low, low 9.705409e-01
#> 20                 low, low, low, low, low, low, low 9.760067e-01
#> 21                      low, low, low, low, low, low 9.800842e-01
#> 22                           low, low, low, low, low 7.841197e-02
#> 23                                low, low, low, low 8.961669e-01
#> 24                                     low, low, low 8.082117e-01
#> 25                                          low, low 9.974477e-01
#> 26                                               low 4.762280e-71
```

The model was estimated with the default `alpha = 0.05` and a context
with a quantile value lower than `alpha` is significantly different from
its parent. Some contexts have larger values: they are kept in the model
because one of their descendants in the context tree is significant (for
instance a context `low, low, low, low, low, low, low, low` is kept
because its extension `typical, low, low, low, low, low, low, low, low`
is significant). Notice that those values are not those reported by
`cutoff(model_5)` (see the dedicated section in the documentation of
[`contexts.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.vlmc.md)).

### Predictive metrics

With `metrics = TRUE`,
[`contexts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.md)
adds predictive quality metrics for each context, computed on the time
steps at which the context is used by the model (i.e. the ones counted
by `local = TRUE`):

``` r

flat_contexts(contexts(model_5,
  frequency = "total", local = TRUE,
  metrics = TRUE
))
#>                                              context freq  accuracy auc
#> 1  typical, high, high, high, high, high, high, high   14 0.6428571  NA
#> 2           high, high, high, high, high, high, high  121 0.9256198  NA
#> 3                 high, high, high, high, high, high   15 0.9333333  NA
#> 4                       high, high, high, high, high   17 0.8823529  NA
#> 5                             high, high, high, high   18 0.9444444  NA
#> 6                                   high, high, high   23 0.7826087  NA
#> 7                                         high, high   32 0.7187500  NA
#> 8                       high, typical, typical, high    3 0.6666667  NA
#> 9                             typical, typical, high   29 0.7241379  NA
#> 10                                     typical, high   14 0.7857143  NA
#> 11                                              high    0       NaN  NA
#> 12                                      low, typical   39 0.5384615  NA
#> 13                    low, typical, typical, typical   17 0.6470588  NA
#> 14                         typical, typical, typical  398 0.8994975  NA
#> 15                                  typical, typical   54 0.8518519  NA
#> 16                                     high, typical   45 0.7111111  NA
#> 17                       typical, low, low, low, low   15 0.6000000  NA
#> 18   typical, low, low, low, low, low, low, low, low    5 0.6000000  NA
#> 19            low, low, low, low, low, low, low, low   40 0.9500000  NA
#> 20                 low, low, low, low, low, low, low    5 1.0000000  NA
#> 21                      low, low, low, low, low, low    5 1.0000000  NA
#> 22                           low, low, low, low, low    5 1.0000000  NA
#> 23                                low, low, low, low    0       NaN  NA
#> 24                                     low, low, low   21 0.7142857  NA
#> 25                                          low, low   31 0.6774194  NA
#> 26                                               low   40 0.7750000  NA
```

The `accuracy` column gives the proportion of those time steps at which
the most probable state according to the context is the observed next
state (it is not defined when the local count is zero). For a VLMC, the
predicted probabilities are identical for all the time steps of a
context, so that they cannot separate the next states: the `auc` column
is therefore always missing. It is informative for a VLMC with
covariates, see below. The metrics of the whole model are given by
`metrics(model_5)` (see
[`vignette("prediction")`](https://fabrice-rossi.github.io/mixvlmc/dev/articles/prediction.md)).

### Positions

With `positions = TRUE`, a `positions` column gives for each context the
list of its positions in the time series. A position of a context is the
index \\t\\ of its last state, so that \\x\_{t+1}\\ is the state that
follows the context. Positions are available only if they have been kept
during the estimation (`keep_match = TRUE` for
[`vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/vlmc.md),
`keep_position = TRUE`, the default, for
[`ctx_tree()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/ctx_tree.md),
always for
[`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/covlmc.md)).
As the positions are numerous, the column is better explored
programmatically:

``` r

ctx_pos_5 <- contexts(model_5, frequency = "total", positions = TRUE)
head(ctx_pos_5$positions[[1]])
#> [1]  51 140 222 274 308 335
```

Positions cover all the occurrences of a context, not only the local
ones. Unlike the counts, they include a possible final occurrence of the
context, at the end of the time series, which is not followed by any
state. Here the series ends with a sequence of `low` states, so the last
time step, 1008, is a position of several contexts, which have therefore
one more position than their count:

``` r

at_end <- sapply(ctx_pos_5$positions, function(pos) length(week_5) %in% pos)
ctx_pos_end <- flat_contexts(ctx_pos_5[at_end, c("context", "freq")])
ctx_pos_end$nb_positions <- lengths(ctx_pos_5$positions[at_end])
ctx_pos_end
#>                    context freq nb_positions
#> 22 low, low, low, low, low   60           61
#> 23      low, low, low, low   75           76
#> 24           low, low, low   96           97
#> 25                low, low  127          128
#> 26                     low  167          168
```

## Contexts of a VLMC with covariates

We use now the data of
[`vignette("covlmc")`](https://fabrice-rossi.github.io/mixvlmc/dev/articles/covlmc.md):
weeks 15 and 16 of the electricity consumption data set, with a
covariate that indicates whether the observation is made during the day
or during the night.

``` r

elec_dts <- dts(power_weeks(c(15, 16)), vals = power_levels)
pc_15_16 <- powerconsumption[powerconsumption$week %in% c(15, 16), ]
elec_cov <- data.frame(day = (pc_15_16$hour >= 7 & pc_15_16$hour <= 18))
elec_model <- covlmc(elec_dts, elec_cov)
elec_model
#> VLMC with covariates context tree on low, typical, high 
#>  cutoff in quantile scale: 0.05
#>  Number of contexts: 7 
#>  Maximum context length: 3
draw(elec_model)
#> ▪
#> ├─ low ([ -1.681 • 0.4984 
#> │         -4.277 • -0.8766 ])
#> ├─ typical
#> │  ├─ low (0.3667, 0.5833, 0.05)
#> │  ├─ typical
#> │  │  ├─ low (0.4058, 0.5507, 0.04348)
#> │  │  ├─ typical ([ 1.792   • -14.02 ⁞ -3.253 ⁞ 18.34
#> │  │  │             -0.2877 • 16.01  ⁞ -36.08 ⁞ 20.01 ])
#> │  │  └─ high (0.05172, 0.8276, 0.1207)
#> │  └─ high (0.02817, 0.8169, 0.1549)
#> └─ high (0, 0.1864, 0.8136)
```

[`contexts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.md)
supports for COVLMC the `frequency`, `local`, `positions` and `metrics`
parameters with the same meaning as for a VLMC. As the transition
probabilities of a COVLMC depend on the covariates, the `auc` column is
now informative: for a context whose model uses the covariates, it
measures how well the probabilities computed by its model separate the
possible next states. For a context whose model does not use the
covariates, the predictions are constant and the AUC is 0.5 (or missing
when some states never follow the context). The parameter `hsize` adds a
column that gives the number of past values of the covariates used by
the model of each context (0 for a context whose model does not use the
covariates).

``` r

flat_contexts(contexts(elec_model,
  frequency = "detailed", hsize = TRUE,
  metrics = TRUE
))
#>                     context freq low typical high hsize  accuracy       auc
#> 1                       low  659 533     120    6     1 0.8088012 0.5653481
#> 2              low, typical  120  44      70    6     0 0.5833333 0.5000000
#> 3     low, typical, typical   69  28      38    3     0 0.5507246 0.5000000
#> 4 typical, typical, typical  655  49     568   38     3 0.8702290 0.6125158
#> 5    high, typical, typical   58   3      48    7     0 0.8275862 0.5000000
#> 6             high, typical   71   2      58   11     0 0.8169014 0.5000000
#> 7                      high  381   0      71  310     0 0.8136483        NA
```

The models themselves can be included in the data frame, either as their
coefficients (`model = "coef"`) or as the R objects produced by the
estimation engine (`model = "full"`, see
[`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/covlmc.md)
and the article [Computing engines and control of VLMC with
covariates](https://fabrice-rossi.github.io/mixvlmc/articles/covlmc-engines.html)
for details about the engines). As for contexts, the corresponding
column is a list:

``` r

ctx_coef <- contexts(elec_model, model = "coef")
ctx_coef$coef[[1]]
#>           [,1]       [,2]
#> [1,] -1.681411  0.4984117
#> [2,] -4.276666 -0.8766255
```

Finally, when the state space has at least three states, some contexts
of a COVLMC can be *merged*: they share a single model. This happens for
instance with a stricter cut off:

``` r

elec_model_merged <- covlmc(elec_dts, elec_cov, alpha = 1e-7)
draw(elec_model_merged)
#> ▪
#> ├─ low (0.8088, 0.1821, 0.009105)
#> ├─ typical
#> │  ├─ typical
#> │  │  ├─ low (0.4058, 0.5507, 0.04348)
#> │  │  ├─ typical (0.07481, 0.8672, 0.05802)
#> │  │  └─ high (0.05172, 0.8276, 0.1207)
#> │  └─ low, high (0.2408, 0.6702, 0.08901)
#> └─ high (0, 0.1864, 0.8136)
```

In this representation (see the article [Drawing context trees and
models](https://fabrice-rossi.github.io/mixvlmc/articles/drawing.html)),
the leaf `low, high` denotes the two merged contexts `low, typical` and
`high, typical`.
[`contexts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.md)
reports merged contexts individually; `merging = TRUE` adds a `merged`
column that identifies them:

``` r

flat_contexts(contexts(elec_model_merged,
  frequency = "total", merging = TRUE,
  metrics = TRUE
))
#>                     context freq  accuracy auc merged
#> 1                       low  659 0.8088012 0.5  FALSE
#> 2     low, typical, typical   69 0.5507246 0.5  FALSE
#> 3 typical, typical, typical  655 0.8671756 0.5  FALSE
#> 4    high, typical, typical   58 0.8275862 0.5  FALSE
#> 5              low, typical  120 0.6701571 0.5   TRUE
#> 6             high, typical   71 0.6701571 0.5   TRUE
#> 7                      high  381 0.8136483  NA  FALSE
```

As merged contexts share their model, they share also their metrics,
computed on all the time steps of the merged contexts.

## Navigating the contexts of a fitted model

### Nodes of a VLMC

The function
[`find_sequence()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/find_sequence.md)
described in
[`vignette("context-trees")`](https://fabrice-rossi.github.io/mixvlmc/dev/articles/context-trees.md)
also applies to fitted models. It returns `NULL` when the sequence is
not represented in the context tree of the model:

``` r

find_sequence(model_5, c("low", "high"))
#> NULL
```

and a `ctx_node` object otherwise. Such a node can be a context or not:

``` r

node_typical <- find_sequence(model_5, "typical")
node_typical
#> Sequence [T]: typical 
#>  followed by low (39), typical (469), high (46)
is_context(node_typical)
#> [1] FALSE
node_th <- find_sequence(model_5, c("typical", "high"))
node_th
#> Context [T]: typical, high 
#>  followed by low (1), typical (13), high (32)
is_context(node_th)
#> [1] TRUE
```

The sequence represented by a node is obtained with
[`as_sequence()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/as_sequence.md).
Its order (temporal or reverse) is the one used to create the node (by
[`find_sequence()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/find_sequence.md)
or
[`contexts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.md)),
as reported by
[`is_reversed()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/is_reversed.md),
and can be switched with [`rev()`](https://rdrr.io/r/base/rev.html) or
overridden with the `reverse` parameter of
[`as_sequence()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/as_sequence.md):

``` r

as_sequence(node_th)
#> [1] "typical" "high"
is_reversed(node_th)
#> [1] FALSE
node_th_rev <- rev(node_th)
node_th_rev
#> Context [R]: high, typical 
#>  followed by low (1), typical (13), high (32)
as_sequence(node_th_rev)
#> [1] "high"    "typical"
as_sequence(node_th_rev, reverse = FALSE)
#> [1] "typical" "high"
```

The context tree can be traversed with
[`parent()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/parent.md),
which removes the oldest state of the sequence (it returns `NULL` for
the root of the tree, the empty sequence), and
[`children()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/children.md),
which returns a list with one element per state (`NULL` when the
corresponding extension of the sequence is not in the tree):

``` r

parent(node_th)
#> Context [T]: high 
#>  followed by low (1), typical (45), high (240)
parent(parent(node_th))
#> Sequence [T]:  
#>  followed by low (168), typical (554), high (286)
children(node_th)
#> $low
#> NULL
#> 
#> $typical
#> Context [T]: typical, typical, high 
#>  followed by low (1), typical (10), high (21)
#> 
#> $high
#> NULL
```

The
[`counts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/counts.md)
function supports the `local` parameter, as
[`contexts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.md):

``` r

counts(node_th)
#>   total low typical high
#> 1    46   1      13   32
counts(node_th, local = TRUE)
#>   total low typical high
#> 1    14   0       3   11
```

The positions of the node in the time series are given by
[`positions()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/positions.md).
As the state that follows a context at position \\t\\ is \\x\_{t+1}\\,
the counts can be recovered from the positions and the original series
(a final occurrence at the end of the series would give a missing value,
ignored by [`table()`](https://rdrr.io/r/base/table.html)):

``` r

pos_th <- positions(node_th)
head(pos_th)
#> [1]  45  76 113 120 124 134
table(factor(dts_data(week_5)[pos_th + 1], levels = power_levels))
#> 
#>     low typical    high 
#>       1      13      32
```

Finally,
[`cutoff()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.md)
and
[`metrics()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/metrics.md)
give, for a single context, the information reported by the
corresponding parameters of
[`contexts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.md).
Notice that
[`cutoff()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.md)
uses by default the quantile scale and modifies slightly the value to
ensure that the context is pruned when the value is used in
[`prune()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/prune.md)
(use `raw = TRUE` to get the exact value, see
[`cutoff.ctx_node()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.ctx_node.md)):

``` r

cutoff(node_th)
#> [1] 0.03209832
cutoff(node_th, scale = "native", raw = TRUE)
#> [1] 3.438952
metrics(node_th)
#> Context [T]: typical, high 
#>  followed by low (1), typical (13), high (32)
#>  Confusion matrix: 
#>           low typical high 
#>   low     0   0       0    
#>   typical 0   0       0    
#>   high    0   3       11   
#>  Accuracy: 0.7857 
#>  AUC: NA
```

[`metrics()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/metrics.md)
can only be applied to contexts:

``` r

try(metrics(node_typical))
#> Error in metrics.ctx_node(node_typical) : 
#>   the node is not a context, no metrics can be computed
```

### Nodes of a VLMC with covariates

For a COVLMC,
[`find_sequence()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/find_sequence.md)
and
[`contexts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.md)
return `ctx_node_covlmc` objects. In addition to the functions described
above,
[`model()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/model.md)
gives the model of a context, either as its coefficients (by default) or
as the full model, while
[`covariate_memory()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/covariate_memory.md)
gives the number of past values of the covariates used by this model (as
the `hsize` column of
[`contexts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.md)):

``` r

node_low <- find_sequence(elec_model, "low")
node_low
#> Context [T]: low 
#>  followed by low (533), typical (120), high (6)
model(node_low)
#>           [,1]       [,2]
#> [1,] -1.681411  0.4984117
#> [2,] -4.276666 -0.8766255
model(node_low, type = "full")
#> 
#> Call:
#> VGAM::vglm(formula = target ~ ., family = VGAM::multinomial(refLevel = 1), 
#>     data = mm, model = FALSE, x.arg = FALSE, y.arg = FALSE, maxit = getOption("mixvlmc.maxit"))
#> 
#> 
#> Coefficients:
#> (Intercept):1 (Intercept):2   day_1TRUE:1   day_1TRUE:2 
#>    -1.6814114    -4.2766661     0.4984117    -0.8766255 
#> 
#> Degrees of Freedom: 1318 Total; 1314 Residual
#> Residual deviance: 684.6391 
#> Log-likelihood: -342.3195 
#> 
#> This is a multinomial logit model with 3 levels
covariate_memory(node_low)
#> [1] 1
metrics(node_low)
#> Context [T]: low 
#>  followed by low (533), typical (120), high (6)
#>  Confusion matrix: 
#>           low typical high 
#>   low     533 120     6    
#>   typical 0   0       0    
#>   high    0   0       0    
#>  Accuracy: 0.8088 
#>  AUC: 0.5653
```

Both functions return `NULL` for a node that is not a context:

``` r

node_tt <- find_sequence(elec_model, c("typical", "typical"))
is_context(node_tt)
#> [1] FALSE
model(node_tt)
#> NULL
covariate_memory(node_tt)
#> NULL
```

[`children()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/children.md)
returns also `ctx_node_covlmc` objects.
[`cutoff()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.md)
gives the p-values of the likelihood ratio tests associated with a node,
in quantile scale only (see
[`cutoff.ctx_node_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.ctx_node_covlmc.md)):
for a context, the test used to reduce the memory of its model (when it
can lead to pruning) and, for a node with children, the tests used to
replace its children by the node or to merge some of them. The union of
these values over all the nodes is given by
[`cutoff()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.md)
applied to the model:

``` r

children(node_tt)
#> $low
#> Context [T]: low, typical, typical 
#>  followed by low (28), typical (38), high (3)
#> 
#> $typical
#> Context [T]: typical, typical, typical 
#>  followed by low (49), typical (568), high (38)
#> 
#> $high
#> Context [T]: high, typical, typical 
#>  followed by low (3), typical (48), high (7)
cutoff(node_low)
#> [1] 0.03453508
cutoff(node_tt)
#> [1] 0.0002688263
cutoff(elec_model)
#> [1] 3.453508e-02 7.217747e-03 2.688263e-04 2.670549e-07
```

[`is_merged()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/is_merged.md)
tells whether a context is merged with other contexts and
[`merged_with()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/merged_with.md)
returns them (as a list with one element per state, in the same way as
[`children()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/children.md):
the merged contexts differ only in their oldest state):

``` r

ctxs_merged <- contexts(elec_model_merged)
sapply(ctxs_merged, is_merged)
#> [1] FALSE FALSE FALSE FALSE  TRUE  TRUE FALSE
node_lt <- find_sequence(elec_model_merged, c("low", "typical"))
merged_with(node_lt)
#> $low
#> NULL
#> 
#> $typical
#> NULL
#> 
#> $high
#> Context [T]: high, typical 
#>  followed by low (2), typical (58), high (11)
identical(model(node_lt), model(merged_with(node_lt)$high))
#> [1] TRUE
merged_with(find_sequence(elec_model_merged, "low"))
#> NULL
```

## Worked examples

### Predictable contexts and when they occur

Using the node functions, we can easily compute summary information
about the contexts of a VLMC. The following function gives, for a
context, its length, the most probable next state with its probability
and the number of time steps at which the context is used by the model:

``` r

context_summary <- function(node) {
  freqs <- counts(node)
  probs <- unlist(freqs[, -1]) / freqs$total
  data.frame(
    context = paste(as_sequence(node), collapse = ", "),
    length = length(as_sequence(node)),
    next_state = names(probs)[which.max(probs)],
    probability = max(probs),
    usage = counts(node, frequency = "total", local = TRUE)
  )
}
summary_5 <- do.call(rbind, lapply(contexts(model_5), context_summary))
```

We look for the contexts whose next state is almost deterministic (with
a probability of at least 0.9) and that are actually used, ordered by
decreasing length:

``` r

predictable <- summary_5[summary_5$probability >= 0.9 & summary_5$usage > 0, ]
predictable[order(predictable$length, decreasing = TRUE), ]
#>                               context length next_state probability usage
#> 20  low, low, low, low, low, low, low      7        low   0.9000000     5
#> 3  high, high, high, high, high, high      6       high   0.9000000    15
#> 21       low, low, low, low, low, low      6        low   0.9090909     5
#> 22            low, low, low, low, low      5        low   0.9166667     5
#> 5              high, high, high, high      4       high   0.9027027    18
```

Long sequences of the `low` level are likely to be followed by another
`low` observation. The positions of the corresponding contexts can be
related to other information available in the data set, for instance the
hour of the day. Let us consider the longest context made only of `low`
states:

``` r

all_low <- function(node) all(as_sequence(node) == "low")
low_ctxs <- Filter(all_low, contexts(model_5))
low_lengths <- sapply(low_ctxs, function(node) length(as_sequence(node)))
longest_low <- low_ctxs[[which.max(low_lengths)]]
longest_low
#> Context [T]: low, low, low, low, low, low, low, low 
#>  followed by low (40), typical (5), high (0)
pc_5 <- powerconsumption[powerconsumption$week == 5, ]
table(hour = pc_5$hour[positions(longest_low)])
#> hour
#>  2  3  4  5  6  7  8 17 21 
#>  2  6  6 11 12  5  1  1  1
```

As expected, those long periods of low consumption happen mostly during
the night and the early morning.

### Contexts that use the covariates

In a COVLMC, the dependency of the transition probabilities on the
covariates is context specific. The contexts that actually use the
covariates are obtained as follows:

``` r

uses_covariate <- function(node) covariate_memory(node) > 0
uses_cov <- Filter(uses_covariate, contexts(elec_model))
uses_cov
#> [[1]]
#> Context [T]: low 
#>  followed by low (533), typical (120), high (6)
#> 
#> [[2]]
#> Context [T]: typical, typical, typical 
#>  followed by low (49), typical (568), high (38)
```

To understand the effect of the covariate, we can for instance compare
the empirical distribution of the next state during the day and during
the night, using the positions of each context (the value of the
covariate at the position of a context is the most recent one available
to its model):

``` r

next_state_by_day <- function(node) {
  pos <- positions(node)
  ## remove a possible final occurrence
  pos <- pos[pos < length(elec_dts)]
  prop.table(table(
    day = elec_cov$day[pos],
    next_state = factor(dts_data(elec_dts)[pos + 1], levels = power_levels)
  ), margin = 1)
}
next_state_by_day(uses_cov[[1]])
#>        next_state
#> day             low     typical        high
#>   FALSE 0.833333333 0.155092593 0.011574074
#>   TRUE  0.762114537 0.233480176 0.004405286
```

After a `low` observation, the next observation is more likely to stay
`low` at night than during the day. The comparison with a context whose
model does not use the covariate shows smaller differences:

``` r

next_state_by_day(find_sequence(elec_model, "high"))
#>        next_state
#> day           low   typical      high
#>   FALSE 0.0000000 0.1737089 0.8262911
#>   TRUE  0.0000000 0.2023810 0.7976190
```

Finally, the predictive metrics of the contexts that use the covariates
show that their AUC is larger than 0.5, the value obtained by a context
whose model does not use the covariates (see the `auc` column of
[`contexts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.md)
for `elec_model` above):

``` r

sapply(uses_cov, function(node) unlist(metrics(node)[c("accuracy", "auc")]))
#>               [,1]      [,2]
#> accuracy 0.8088012 0.8702290
#> auc      0.5653481 0.6125158
```
