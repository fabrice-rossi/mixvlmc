# Cut off values for VLMC-like models

This generic function returns one or more cut off values that are
guaranteed to have an effect on the `model` passed to the function when
a simplification procedure is applied (in general a tree pruning
operation as provided by
[`prune()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/prune.md)).

## Usage

``` r
cutoff(model, ...)
```

## Arguments

- model:

  a model.

- ...:

  additional arguments for the implementations of `cutoff()`.

## Value

a cut off value or a vector of cut off values.

## Details

The exact definition of a cut off value depends on the model type and is
documented in the concrete implementations of the function (see e.g.
[`cutoff.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.vlmc.md)
and
[`cutoff.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.covlmc.md)).

## See also

[`prune()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/prune.md)

## Examples

``` r
pc <- powerconsumption[powerconsumption$week == 5, ]
rdts <- cut(pc$active_power,
  breaks = c(0, quantile(pc$active_power,
    probs = c(0.25, 0.5, 0.75, 1)
  ))
)
model <- vlmc(rdts)
draw(model)
#> ▪ (0.25, 0.25, 0.25, 0.25)
#> ├─ (0,0.458] (0.7968, 0.1912, 0.007968, 0.003984)
#> │  └─ (0,0.458] (0.809, 0.1809, 0.005025, 0.005025)
#> │     └─ (0,0.458] (0.8188, 0.1688, 0.00625, 0.00625)
#> │        └─ (0,0.458] (0.8462, 0.1385, 0.007692, 0.007692)
#> │           └─ (0.458,1.34] (0.55, 0.4, 0, 0.05)
#> ├─ (0.458,1.34] (0.1984, 0.6667, 0.1071, 0.02778)
#> │  ├─ (0,0.458] (0.375, 0.4167, 0.1458, 0.0625)
#> │  │  └─ (0,0.458] (0.3611, 0.4444, 0.1389, 0.05556)
#> │  │     └─ (0.458,1.34] (0, 0.8889, 0.1111, 0)
#> │  └─ (0.458,1.34] (0.1369, 0.75, 0.09524, 0.01786)
#> │     └─ (0,0.458] (0.4, 0.55, 0.05, 0)
#> ├─ (1.34,2.13] (0.003968, 0.123, 0.7262, 0.1468)
#> │  └─ (2.13,7.54] (0, 0.2, 0.525, 0.275)
#> └─ (2.13,7.54] (0.003968, 0.01587, 0.1587, 0.8214)
#>    └─ (0.458,1.34] (0, 0.1429, 0.5714, 0.2857)
model_cuts <- cutoff(model)
model_2 <- prune(model, model_cuts[2])
draw(model_2)
#> ▪ (0.25, 0.25, 0.25, 0.25)
#> ├─ (0,0.458] (0.7968, 0.1912, 0.007968, 0.003984)
#> │  └─ (0,0.458] (0.809, 0.1809, 0.005025, 0.005025)
#> │     └─ (0,0.458] (0.8188, 0.1688, 0.00625, 0.00625)
#> │        └─ (0,0.458] (0.8462, 0.1385, 0.007692, 0.007692)
#> │           └─ (0.458,1.34] (0.55, 0.4, 0, 0.05)
#> ├─ (0.458,1.34] (0.1984, 0.6667, 0.1071, 0.02778)
#> │  └─ (0,0.458] (0.375, 0.4167, 0.1458, 0.0625)
#> │     └─ (0,0.458] (0.3611, 0.4444, 0.1389, 0.05556)
#> │        └─ (0.458,1.34] (0, 0.8889, 0.1111, 0)
#> ├─ (1.34,2.13] (0.003968, 0.123, 0.7262, 0.1468)
#> └─ (2.13,7.54] (0.003968, 0.01587, 0.1587, 0.8214)
#>    └─ (0.458,1.34] (0, 0.1429, 0.5714, 0.2857)
```
