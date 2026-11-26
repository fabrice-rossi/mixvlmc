# Extract the plain discrete time series from a dts object

This function returns a copy of the discrete time series used to build
the `dts` object (see
[`dts()`](https://fabrice-rossi.github.io/mixvlmc/reference/dts.md)).

## Usage

``` r
dts_data(x)
```

## Arguments

- x:

  a `dts` object.

## Value

a vector representing the time series, whose values are taken from the
state space of `x` (see
[`states()`](https://fabrice-rossi.github.io/mixvlmc/reference/states.md)).

## Examples

``` r
set.seed(0)
raw_dts <- sample(c("A", "B", "C"), 50, replace = TRUE)
odts <- dts(raw_dts)
back_to_raw <- dts_data(odts)
## should be TRUE
identical(raw_dts, back_to_raw)
#> [1] TRUE
```
