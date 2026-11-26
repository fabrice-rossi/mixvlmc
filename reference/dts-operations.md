# Basic operations on discrete time series

These functions implement standard operations on discrete time series
represented by `dts` objects (see
[`dts()`](https://fabrice-rossi.github.io/mixvlmc/reference/dts.md)),
preserving their state space.

## Usage

``` r
# S3 method for class 'dts'
x[i, ...]

# S3 method for class 'dts'
x[[i, ...]]

# S3 method for class 'dts'
length(x)

# S3 method for class 'dts'
rev(x)
```

## Arguments

- x:

  a `dts` object.

- i:

  indices specifying the elements to extract.

- ...:

  not used.

## Value

- `x[i]` returns a `dts` object with the selected observations and the
  same state space as `x` (including the states that are absent from the
  selection);

- `x[[i]]` returns the `i`-th observation, as a value of the state space
  (see
  [`states()`](https://fabrice-rossi.github.io/mixvlmc/reference/states.md));

- `length(x)` returns the number of observations of `x`;

- `rev(x)` returns a `dts` object with the observations of `x` in
  reverse order and the same state space.

## See also

[`dts()`](https://fabrice-rossi.github.io/mixvlmc/reference/dts.md),
[`dts_data()`](https://fabrice-rossi.github.io/mixvlmc/reference/dts_data.md),
[`states()`](https://fabrice-rossi.github.io/mixvlmc/reference/states.md)

## Examples

``` r
x <- dts(c("A", "B", "C", "A", "A"))
length(x)
#> [1] 5
x[[2]]
#> [1] "B"
## the state space is preserved
x_short <- x[1:2]
x_short
#> Discrete time series of length 2 with state space A, B, C
#>  A B
states(x_short)
#> [1] "A" "B" "C"
rev(x)
#> Discrete time series of length 5 with state space A, B, C
#>  A A C B A
```
