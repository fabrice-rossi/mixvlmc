# Convert an object to a Variable Length Markov Chain (VLMC)

This generic function converts an object into a VLMC.

## Usage

``` r
# S3 method for class 'ctx_tree_cpp'
as_vlmc(x, alpha, cutoff, ...)
```

## Arguments

- x:

  an object to convert into a VLMC.

- alpha:

  cut off parameter applied during the conversion, quantile scale (if
  specified).

- cutoff:

  cut off parameter applied during the conversion, native scale (if
  specified). Takes precedence over `alpha` if both are specified.

- ...:

  additional arguments for conversion functions.

## Value

a VLMC.

## Details

When `x` is a context tree, the function converts it into a VLMC. If
`alpha` or `cutoff` is specified, it is used to reduce the complexity of
the tree as in a direct call to
[`vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/vlmc.md)
(see also
[`prune()`](https://fabrice-rossi.github.io/mixvlmc/reference/prune.md)).
Otherwise, the tree is converted without pruning (the cut off of the
resulting VLMC is 0 in native scale, i.e. 1 in quantile scale).

When `x` is the result of
[`tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_vlmc.md),
the function returns the best model selected by
[`tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_vlmc.md).

## See also

[`ctx_tree()`](https://fabrice-rossi.github.io/mixvlmc/reference/ctx_tree.md)

[`tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_vlmc.md)

## Examples

``` r
## conversion from a context tree
rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 3, backend = "C++")
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
```
