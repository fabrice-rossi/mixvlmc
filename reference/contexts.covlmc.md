# Contexts of a VLMC with covariates

This function returns the different contexts present in a VLMC with
covariates, possibly with some associated data.

## Usage

``` r
# S3 method for class 'covlmc'
contexts(
  ct,
  sequence = FALSE,
  reverse = FALSE,
  frequency = NULL,
  positions = FALSE,
  local = FALSE,
  metrics = FALSE,
  model = NULL,
  hsize = FALSE,
  merging = FALSE,
  ...
)
```

## Arguments

- ct:

  a fitted VLMC with covariates (a `covlmc` object).

- sequence:

  if `TRUE` the function returns its results as a `data.frame`, if
  `FALSE` (default) as a list of `ctx_node` objects (see details).

- reverse:

  logical (defaults to `FALSE`). If `TRUE`, contexts are given in
  reverse temporal order. See the dedicated section.

- frequency:

  specifies the counts to be included in the result data.frame. The
  default value of `NULL` does not include anything. `"total"` gives the
  number of occurrences of each context in the original sequence.
  `"detailed"` includes in addition the breakdown of these occurrences
  into all the possible states.

- positions:

  logical (defaults to `FALSE`). Specifies whether the positions of each
  context in the time series used to build the context tree should be
  reported in a `positions` column of the result data frame. The
  availability of the positions depends on the way the context tree was
  built (see the `keep_position` parameter of
  [`ctx_tree()`](https://fabrice-rossi.github.io/mixvlmc/reference/ctx_tree.md)).
  See the dedicated section for the definition of a position.

- local:

  specifies how the counts reported by `frequency` are computed. When
  `local` is `FALSE` (default value) the counts include both counts that
  are specific to the context (if any) and counts from the descendants
  of the context in the tree. When `local` is `TRUE` the counts include
  only the number of times the context appears without being the last
  part of a longer context.

- metrics:

  if `TRUE`, adds predictive metrics for each context (see
  [`metrics()`](https://fabrice-rossi.github.io/mixvlmc/reference/metrics.md)
  for the definition of predictive metrics).

- model:

  specifies whether to include the model associated with each context.
  The default result with `model=NULL` does not include any model.
  Setting `model` to `"coef"` adds the coefficients of the models in a
  `coef` column, while `"full"` includes the models themselves (as R
  objects) in a `model` column (not supported by fully trimmed models,
  see
  [`trim.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/trim.covlmc.md)).

- hsize:

  if `TRUE`, adds a `hsize` column to the result data frame that gives
  for each context the size of the history of covariates used by the
  model (see
  [`covariate_memory()`](https://fabrice-rossi.github.io/mixvlmc/reference/covariate_memory.md)).

- merging:

  if `TRUE`, adds a `merged` column to the result data frame. For a
  normal context, the value of `merged` is `FALSE`. Contexts that share
  the same model have a `TRUE` `merged` value (see
  [`is_merged()`](https://fabrice-rossi.github.io/mixvlmc/reference/is_merged.md)).

- ...:

  additional arguments for the contexts function.

## Value

A list of class `contexts` containing the contexts represented in this
tree (as `ctx_node_covlmc`) or a data.frame.

## Details

The default behaviour of the function is to return a list of all the
contexts using `ctx_node_covlmc` objects (as returned by
[`find_sequence.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/find_sequence.covlmc.md)).
The properties of the contexts can then be explored using adapted
functions such as
[`counts()`](https://fabrice-rossi.github.io/mixvlmc/reference/counts.md),
[`covariate_memory()`](https://fabrice-rossi.github.io/mixvlmc/reference/covariate_memory.md),
[`cutoff.ctx_node()`](https://fabrice-rossi.github.io/mixvlmc/reference/cutoff.ctx_node.md),
[`metrics.ctx_node()`](https://fabrice-rossi.github.io/mixvlmc/reference/metrics.ctx_node.md),
[`model()`](https://fabrice-rossi.github.io/mixvlmc/reference/model.md),
[`merged_with()`](https://fabrice-rossi.github.io/mixvlmc/reference/merged_with.md)
and
[`positions()`](https://fabrice-rossi.github.io/mixvlmc/reference/positions.md).

When `sequence=TRUE` the method returns a data.frame whose first column,
named `context`, contains the contexts as vectors (i.e. the value
returned by
[`as_sequence()`](https://fabrice-rossi.github.io/mixvlmc/reference/as_sequence.md)
applied to a `ctx_node` object). Other columns contain context specific
values specified by the additional parameters. Setting any of those
parameters to a value that asks for reporting information will toggle
the result type of the function to `data.frame`.

See
[`contexts.ctx_tree()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.ctx_tree.md)
for details about the `frequency` parameter. When `model` is non `NULL`,
the resulting `data.frame` contains the models associated with each
context (either the full R model or its coefficients). Other columns are
added if the corresponding parameters are set to `TRUE`.

## Positions

A position of a context `ctx` in the time series `x` is an index value
`t` such that the context ends with `x[t]`. Thus `x[t+1]`, when it
exists, is the value that follows the context. For instance if
`x=c(0, 0, 1, 1)` and `ctx=c(0, 1)` (in standard state order), then the
position of `ctx` in `x` is 3.

Positions include all the occurrences of a context, including an
occurrence at the end of the time series (`t=length(x)`), which is not
followed by any value. Such an occurrence is not included in the
frequencies, which count the values that follow the context: the number
of positions of a context can therefore exceed its frequency by one.

## State order in a context

Notice that contexts are given by default in the temporal order and not
in the "reverse" order used by many VLMC research papers: older values
are on the left. For instance, the context `c(1, 0)` is reported if the
sequence 0, then 1 appeared in the time series used to build the context
tree. Set `reverse` to `TRUE` for the reverse convention which is
somewhat easier to relate to the way the context trees are represented
by [`draw()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.md)
(i.e. recent values at the top of the tree).

## See also

[`find_sequence()`](https://fabrice-rossi.github.io/mixvlmc/reference/find_sequence.md)
and
[`find_sequence.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/find_sequence.covlmc.md)
for direct access to a specific context, and
[`contexts.ctx_tree()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.ctx_tree.md),
[`contexts.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.vlmc.md)
and `contexts.covlmc()` for concrete implementations of
[`contexts()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.md).

## Examples

``` r
pc <- powerconsumption[powerconsumption$week == 5, ]
breaks <- c(0, median(pc$active_power), max(pc$active_power))
dts <- cut(pc$active_power, breaks = breaks)
dts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
m_cov <- covlmc(dts, dts_cov, min_size = 5)
## direct representation with ctx_node_covlmc objects
m_cov_ctxs <- contexts(m_cov)
m_cov_ctxs
#> Contexts:
#>  (0,1.34], (0,1.34], (0,1.34]
#>  (1.34,7.54], (0,1.34], (0,1.34]
#>  (1.34,7.54], (0,1.34]
#>  (1.34,7.54]
sapply(m_cov_ctxs, covariate_memory)
#> [1] 3 1 0 0
sapply(m_cov_ctxs, is_merged)
#> [1] FALSE FALSE FALSE FALSE
sapply(m_cov_ctxs, model)
#> [[1]]
#>     (Intercept) day_night_1TRUE day_night_2TRUE day_night_3TRUE 
#>       -2.813042        2.177964       -4.448526        2.417832 
#> 
#> [[2]]
#>     (Intercept) day_night_1TRUE 
#>       -19.56607        18.38741 
#> 
#> [[3]]
#> (Intercept) 
#>   -1.856298 
#> 
#> [[4]]
#> (Intercept) 
#>    2.535411 
#> 
## data.frame interface
contexts(m_cov, model = "coef")
#>        context         coef
#> 1 (0,1.34].... -2.81304....
#> 2 (1.34,7..... -19.5660....
#> 3 (1.34,7..... -1.85629....
#> 4  (1.34,7.54] 2.535411....
contexts(m_cov, model = "full", hsize = TRUE)
#>        context        model hsize
#> 1 (0,1.34].... c(`(Inte....     3
#> 2 (1.34,7..... c(`(Inte....     1
#> 3 (1.34,7..... c(`(Inte....     0
#> 4  (1.34,7.54] c(`(Inte....     0
contexts(m_cov, metrics = TRUE, merging = TRUE)
#>        context  accuracy       auc merged
#> 1 (0,1.34].... 0.9351852 0.5704120  FALSE
#> 2 (1.34,7..... 0.8750000 0.7678571  FALSE
#> 3 (1.34,7..... 0.8648649 0.5000000  FALSE
#> 4  (1.34,7.54] 0.9265873 0.5000000  FALSE
```
