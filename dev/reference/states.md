# State space of an object

This function returns the state space of an object for which this is
meaningful, such as a discrete time series or a context tree.

## Usage

``` r
states(x)

# S3 method for class 'ctx_tree'
states(x)

# S3 method for class 'dts'
states(x)
```

## Arguments

- x:

  an object with a state space.

## Value

the state space of the object.

## Examples

``` r
rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 2)
## should be c(0, 1)
states(rdts_ctree)
#> [1] 0 1
set.seed(0)
x_dts <- dts(sample(c("A", "B", "C"), 20, replace = TRUE))
## should be c("A", "B", "C")
states(x_dts)
#> [1] "A" "B" "C"
```
