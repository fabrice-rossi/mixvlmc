# Summary of a context tree

This function computes summary statistics of a context tree, which are
displayed by the associated
[`print()`](https://rdrr.io/r/base/print.html) method.

## Usage

``` r
# S3 method for class 'ctx_tree'
summary(object, ...)

# S3 method for class 'summary.ctx_tree'
print(x, ...)

# S3 method for class 'ctx_tree_cpp'
summary(object, ...)

# S3 method for class 'summary.ctx_tree_cpp'
print(x, ...)
```

## Arguments

- object:

  a context tree (a `ctx_tree` object).

- ...:

  additional arguments (not used).

- x:

  a summary of a context tree, as returned by
  [`summary()`](https://rdrr.io/r/base/summary.html).

## Value

[`summary()`](https://rdrr.io/r/base/summary.html) returns an object of
class `summary.ctx_tree` (and `summary.ctx_tree_cpp` for a context tree
built with the C++ backend), a list with the following components:

- `state_space`: the state space of the context tree;

- `nb`: the number of contexts;

- `depth`: the length of the longest context;

- `avg_depth`: the average length of the contexts.

[`print()`](https://rdrr.io/r/base/print.html) displays the summary and
returns `x` invisibly.

## See also

[`ctx_tree()`](https://fabrice-rossi.github.io/mixvlmc/reference/ctx_tree.md),
[`contexts()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.md),
[`context_number()`](https://fabrice-rossi.github.io/mixvlmc/reference/context_number.md),
[`depth()`](https://fabrice-rossi.github.io/mixvlmc/reference/depth.md)

## Examples

``` r
rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0)
rdts_ctree <- ctx_tree(rdts, min_size = 2, max_depth = 4)
summary(rdts_ctree)
#> Context tree on 0, 1 [C++]
#>  Number of contexts: 9 
#>  Maximum context length: 4 
#>  Average context length: 2.889 
## the components of the summary
unclass(summary(rdts_ctree))
#> $state_space
#> [1] 0 1
#> 
#> $nb
#> [1] 9
#> 
#> $depth
#> [1] 4
#> 
#> $avg_depth
#> [1] 2.888889
#> 
```
