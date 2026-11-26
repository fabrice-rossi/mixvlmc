# Convert a vector to a discrete time series

This function creates a representation of a discrete time series that
can be further processed by model estimation functions.

## Usage

``` r
dts(x, vals = NULL)
```

## Arguments

- x:

  a discrete time series; can be numeric, character, factor or logical.

- vals:

  the set of values that can be taken by the time series, a.k.a. the
  state space, see details (defaults to `NULL`).

## Value

a discrete time series (an object of a class that inherits from `dts`).

## Details

The discrete time series `x` can be a vector of numeric, character,
factor or logical type. If the state space of the series is not
specified, that is when `vals` is `NULL`, it is computed in a way that
depends on the type of `x`:

- for a factor, `vals` is set to the
  [`levels()`](https://rdrr.io/r/base/levels.html) of `x`;

- for a logical vector, `vals` is set to `c(FALSE, TRUE)`;

- for other types, `vals` is set to all the unique values taken by the
  time series (as returned by `sort(unique(x))`).

If `vals` is specified, the function makes sure that `x` contains only
the specified values and reports an error otherwise. The state space
`vals` may include values that do not appear in `x`.

## Examples

``` r
set.seed(0)
x_dts <- dts(sample(c("A", "B"), 20, replace = TRUE))
x_dts
#> Discrete time series of length 20 with state space A, B
#>  B A B A A ...
## specifying the state space
x_dts_3 <- dts(sample(c("A", "B"), 20, replace = TRUE),
  vals = c("A", "B", "C")
)
states(x_dts_3)
#> [1] "A" "B" "C"
```
