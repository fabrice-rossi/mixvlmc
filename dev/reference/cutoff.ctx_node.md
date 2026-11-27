# Cut off value for pruning a node in the context tree of a VLMC

This function returns the cut off value associated with a specific node
in the context tree interpreted as a VLMC. The node is represented by a
`ctx_node` object as returned by
[`find_sequence()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/find_sequence.md)
or
[`contexts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.md).
For details, see
[`cutoff.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.vlmc.md).

## Usage

``` r
# S3 method for class 'ctx_node'
cutoff(model, scale = c("quantile", "native"), raw = FALSE, ...)
```

## Arguments

- model:

  a `ctx_node` object as returned by
  [`find_sequence()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/find_sequence.md)

- scale:

  specifies whether the results should be "native" log-likelihood ratio
  values or expressed in a "quantile" scale of a chi-squared
  distribution (defaults to "quantile").

- raw:

  specifies whether the returned values should be limit values computed
  in the model or modified values that guarantee pruning (defaults to
  `FALSE`, see details in
  [`cutoff.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.vlmc.md))

- ...:

  additional arguments for the `cutoff` function.

## Value

a cut off value

## See also

[`cutoff()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.md)

## Examples

``` r
pc <- powerconsumption[powerconsumption$week == 5, ]
rdts <- cut(pc$active_power,
  breaks = c(0, quantile(pc$active_power,
    probs = c(0.25, 0.5, 0.75, 1)
  ))
)
model <- vlmc(rdts)
model_ctxs <- contexts(model)
cutoff(model_ctxs[[1]])
#> [1] 0.01532964
cutoff(model_ctxs[[2]], scale = "native", raw = TRUE)
#> [1] 212.9032
```
