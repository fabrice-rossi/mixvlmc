# Print a discrete time series

This function prints a discrete time series.

## Usage

``` r
# S3 method for class 'dts'
print(x, n = 5, ...)
```

## Arguments

- x:

  the `dts` object to print.

- n:

  the number of time steps of the time series to print (defaults to 5).

- ...:

  additional arguments (not used).

## Value

the `x` object, invisibly.

## Examples

``` r
set.seed(0)
x_dts <- dts(sample(c("A", "B"), 20, replace = TRUE))
print(x_dts, n = 10)
#> Discrete time series of length 20 with state space A, B
#>  B A B A A B A A A B ...
```
