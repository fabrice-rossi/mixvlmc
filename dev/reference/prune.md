# Prune a Variable Length Markov Chain (VLMC)

This function prunes a VLMC.

## Usage

``` r
prune(vlmc, alpha = 0.05, cutoff = NULL, ...)

# S3 method for class 'vlmc'
prune(vlmc, alpha = 0.05, cutoff = NULL, ...)

# S3 method for class 'vlmc_cpp'
prune(vlmc, alpha = 0.05, cutoff = NULL, ...)
```

## Arguments

- vlmc:

  a fitted VLMC model.

- alpha:

  number in (0,1\] (default: 0.05) cut off value in quantile scale for
  pruning.

- cutoff:

  non negative number: cut off value in native (log likelihood ratio)
  scale for pruning. Defaults to the value obtained from `alpha`. Takes
  precedence over `alpha` if specified.

- ...:

  additional arguments (not used).

## Value

a pruned VLMC.

## Details

In general, pruning a VLMC is more efficient than constructing two VLMCs
(the base one and the pruned one). Up to numerical instabilities,
building a VLMC with `alpha = a` and then pruning it with `alpha = b`
(with `a > b`) should produce the same VLMC as building directly the
VLMC with `alpha = b` (in the native scale, the cut off used for pruning
must be larger than the one used to build the VLMC). Interesting cut off
values can be extracted from a VLMC using the
[`cutoff()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.md)
function.

As automated model selection is provided by
[`tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/tune_vlmc.md),
the direct use of `prune()` should be reserved to advanced exploration
of the set of trees that can be obtained from a complex one, e.g. to
implement model selection techniques that are not provided by
[`tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/tune_vlmc.md).

## See also

[`cutoff()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.md)
and
[`tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/tune_vlmc.md)

## Examples

``` r
pc <- powerconsumption[powerconsumption$week == 5, ]
rdts <- cut(pc$active_power,
  breaks = c(0, quantile(pc$active_power,
    probs = c(0.25, 0.5, 0.75, 1)
  ))
)
base_model <- vlmc(rdts, alpha = 0.1)
model_cuts <- cutoff(base_model)
pruned_model <- prune(base_model, model_cuts[3])
draw(pruned_model)
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
direct_simple <- vlmc(rdts, alpha = model_cuts[3])
draw(direct_simple)
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
# pruned_model and direct_simple should be identical
all.equal(pruned_model, direct_simple)
#> [1] TRUE
```
