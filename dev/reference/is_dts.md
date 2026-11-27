# Test if the object is a discrete time series

This function returns `TRUE` for discrete time series (objects of class
`dts`) and `FALSE` for other objects.

## Usage

``` r
is_dts(x)
```

## Arguments

- x:

  an R object.

## Value

`TRUE` for discrete time series.

## Examples

``` r
set.seed(0)
pre_dts <- sample(c("A", "B"), 20, replace = TRUE)
x_dts <- dts(pre_dts)
is_dts(x_dts)
#> [1] TRUE
is_dts(pre_dts)
#> [1] FALSE
```
