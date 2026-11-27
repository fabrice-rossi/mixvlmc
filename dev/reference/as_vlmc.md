# Convert an object to a Variable Length Markov Chain (VLMC)

This generic function converts an object into a VLMC.

## Usage

``` r
as_vlmc(x, ...)

# S3 method for class 'ctx_tree'
as_vlmc(x, alpha, cutoff, ...)

# S3 method for class 'tune_vlmc'
as_vlmc(x, ...)
```

## Arguments

- x:

  an object to convert into a VLMC.

- ...:

  additional arguments for conversion functions.

- alpha:

  cut off parameter applied during the conversion, quantile scale (if
  specified).

- cutoff:

  cut off parameter applied during the conversion, native scale (if
  specified). Takes precedence over `alpha` if both are specified.

## Value

a VLMC.

## Details

When `x` is a context tree, the function converts it into a VLMC. If
`alpha` or `cutoff` is specified, it is used to reduce the complexity of
the tree as in a direct call to
[`vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/vlmc.md)
(see also
[`prune()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/prune.md)).
Otherwise, the tree is converted without pruning (the cut off of the
resulting VLMC is 0 in native scale, i.e. 1 in quantile scale).

When `x` is the result of
[`tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/tune_vlmc.md),
the function returns the best model selected by
[`tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/tune_vlmc.md).

## See also

[`ctx_tree()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/ctx_tree.md)

[`tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/tune_vlmc.md)

## Examples

``` r
## conversion from a context tree
rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 3)
draw(rdts_ctree)
#> ▪
#> ├─ 0
#> │  ├─ 0
#> │  │  └─ 1
#> │  └─ 1
#> │     ├─ 0
#> │     └─ 1
#> └─ 1
#>    ├─ 0
#>    │  ├─ 0
#>    │  └─ 1
#>    └─ 1
#>       ├─ 0
#>       └─ 1
rdts_vlmc <- as_vlmc(rdts_ctree)
class(rdts_vlmc)
#> [1] "vlmc_cpp"     "vlmc"         "ctx_tree_cpp" "ctx_tree"    
draw(rdts_vlmc)
#> ▪ (0.5, 0.5)
#> ├─ 0 (0.25, 0.75)
#> │  ├─ 0 (0, 1)
#> │  │  └─ 1 (0, 1)
#> │  └─ 1 (0.5, 0.5)
#> │     ├─ 0 (0, 1)
#> │     └─ 1 (1, 0)
#> └─ 1 (0.6, 0.4)
#>    ├─ 0 (0.6667, 0.3333)
#>    │  ├─ 0 (1, 0)
#>    │  └─ 1 (1, 0)
#>    └─ 1 (0.5, 0.5)
#>       ├─ 0 (0, 1)
#>       └─ 1 (1, 0)
## conversion with pruning
rdts_pruned_vlmc <- as_vlmc(rdts_ctree, alpha = 0.25)
draw(rdts_pruned_vlmc)
#> ▪ (0.5, 0.5)
#> ├─ 0 (0.25, 0.75)
#> │  └─ 1 (0.5, 0.5)
#> │     ├─ 0 (0, 1)
#> │     └─ 1 (1, 0)
#> └─ 1 (0.6, 0.4)
#>    └─ 1 (0.5, 0.5)
#>       ├─ 0 (0, 1)
#>       └─ 1 (1, 0)
## conversion from the result of tune_vlmc()
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
rdts_best_vlmc <- as_vlmc(tune_result)
draw(rdts_best_vlmc)
#> ▪ (0.25, 0.25, 0.25, 0.25)
#> ├─ (0,0.458] (0.7968, 0.1912, 0.007968, 0.003984)
#> ├─ (0.458,1.34] (0.1984, 0.6667, 0.1071, 0.02778)
#> ├─ (1.34,2.13] (0.003968, 0.123, 0.7262, 0.1468)
#> └─ (2.13,7.54] (0.003968, 0.01587, 0.1587, 0.8214)
```
