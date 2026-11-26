# Trim a VLMC

This function returns a trimmed VLMC from which the positions of the
contexts in the time series (see the `keep_match` parameter of
[`vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/vlmc.md))
have been removed.

## Usage

``` r
# S3 method for class 'vlmc_cpp'
trim(ct, ...)
```

## Arguments

- ct:

  a VLMC.

- ...:

  additional arguments (not used).

## Value

a trimmed VLMC.

## Details

Trimming with the C++ backend is done directly in the memory managed by
`Rcpp` and cannot be detected at the R level using e.g.
[`utils::object.size()`](https://rdrr.io/r/utils/object.size.html).

## Examples

``` r
## VLMC trimming is generally useless unless match positions were kept
pc <- powerconsumption[powerconsumption$week %in% 5:6, ]
rdts <- cut(pc$active_power, breaks = 4)
model <- vlmc(rdts, backend = "C++", keep_match = TRUE)
model <- trim(model)
```
