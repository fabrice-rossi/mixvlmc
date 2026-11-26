# Summary of a VLMC

This function computes summary statistics of a VLMC, which are displayed
by the associated [`print()`](https://rdrr.io/r/base/print.html) method.

## Usage

``` r
# S3 method for class 'vlmc'
summary(object, ...)

# S3 method for class 'summary.vlmc'
print(x, ...)

# S3 method for class 'vlmc_cpp'
summary(object, ...)

# S3 method for class 'summary.vlmc_cpp'
print(x, ...)
```

## Arguments

- object:

  a fitted VLMC (a `vlmc` object).

- ...:

  additional arguments (not used).

- x:

  a summary of a VLMC, as returned by
  [`summary()`](https://rdrr.io/r/base/summary.html).

## Value

[`summary()`](https://rdrr.io/r/base/summary.html) returns an object of
class `summary.vlmc` (and `summary.vlmc_cpp` for a VLMC built with the
C++ backend), a list with the following components:

- `state_space`: the state space of the VLMC;

- `nb`: the number of contexts;

- `depth`: the length of the longest context;

- `avg_depth`: the average length of the contexts;

- `cutoff`: the cut off of the VLMC in native scale (see
  [`cutoff.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/cutoff.vlmc.md));

- `alpha`: the cut off of the VLMC in quantile scale.

[`print()`](https://rdrr.io/r/base/print.html) displays the summary and
returns `x` invisibly.

## See also

[`vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/vlmc.md),
[`contexts.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.vlmc.md),
[`context_number()`](https://fabrice-rossi.github.io/mixvlmc/reference/context_number.md),
[`depth()`](https://fabrice-rossi.github.io/mixvlmc/reference/depth.md)

## Examples

``` r
pc <- powerconsumption[powerconsumption$week == 5, ]
rdts <- cut(pc$active_power,
  breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1)))
)
model <- vlmc(rdts)
summary(model)
#> VLMC context tree on (0,0.458], (0.458,1.34], (1.34,2.13], (2.13,7.54] [C++]
#>  Cutoff: 3.907 (quantile: 0.05)
#>  Number of contexts: 15 
#>  Maximum context length: 5 
#>  Average context length: 2.4 
## the components of the summary
unclass(summary(model))
#> $state_space
#> [1] (0,0.458]    (0.458,1.34] (1.34,2.13]  (2.13,7.54] 
#> Levels: (0,0.458] (0.458,1.34] (1.34,2.13] (2.13,7.54]
#> 
#> $nb
#> [1] 15
#> 
#> $depth
#> [1] 5
#> 
#> $avg_depth
#> [1] 2.4
#> 
#> $cutoff
#> [1] 3.907364
#> 
#> $alpha
#> [1] 0.05
#> 
```
